/* Interface to sendfile system call for Yaws                */
/* author: vinoski@ieee.org                                  */
/* Created : 09 Nov 2008 by Steve Vinoski <vinoski@ieee.org> */

#include <errno.h>
#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#if defined(__linux__)
#include <sys/sendfile.h>
#elif (defined(__APPLE__) && defined(__MACH__)) || defined(__FreeBSD__)
#include <sys/socket.h>
#include <sys/uio.h>
#else
#error "yaws_sendfile_drv not supported on this platform"
#endif

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

#include "hashtable.h"

#define lshift_index(s, i, shift, t) (((t)((unsigned char*)(s))[i]) << (shift))
#define lshift32(s, i, shift) lshift_index(s, i, shift, uint32_t)
#define lshift64(s, i, shift) lshift_index(s, i, shift, uint64_t)
#define get_int32(s) (lshift32(s,0,24) | lshift32(s,1,16) | lshift32(s,2,8) | lshift32(s,3,0))
#define get_int64(s) (lshift64(s,0,56) | lshift64(s,1,48) | lshift64(s,2,40) | lshift64(s,3,32) | \
                      lshift64(s,4,24) | lshift64(s,5,16) | lshift64(s,6,8) | lshift64(s,7,0))

#define put_shift(i, s, idx, shift) (((unsigned char*)(s))[idx] = ((unsigned char)((i) >> (shift)) & 0XFF))
#define put_int32(i, s) do { \
    put_shift(i, s, 0, 24); \
    put_shift(i, s, 1, 16); \
    put_shift(i, s, 2,  8); \
    put_shift(i, s, 3,  0); \
    } while(0)
#define put_int64(i, s) do { \
    put_shift(i, s, 0, 56); \
    put_shift(i, s, 1, 48); \
    put_shift(i, s, 2, 40); \
    put_shift(i, s, 3, 32); \
    put_shift(i, s, 4, 24); \
    put_shift(i, s, 5, 16); \
    put_shift(i, s, 6,  8); \
    put_shift(i, s, 7,  0); \
    } while(0)

typedef union {
    void* hashkey;
    ErlDrvEvent ev_data;
#ifdef _LP64
    uint64_t socket_fd;
#else
    uint32_t socket_fd;
#endif
} SocketFd;

typedef struct {
    off_t offset;
    size_t count;
    ssize_t total;
    int file_fd;
} Transfer;

typedef struct hashtable* Transfers;

typedef struct {
    ErlDrvPort port;
    Transfers xfer_table;
} Desc;


static unsigned int fdhash(void* k)
{
    return ((SocketFd*)&k)->socket_fd;
}

static int fdeq(void* k1, void* k2)
{
    return k1 == k2;
}

static ErlDrvData yaws_sendfile_drv_start(ErlDrvPort port, char* buf)
{
    Desc* d = (Desc*)driver_alloc(sizeof(Desc));
    if (d == NULL) return (ErlDrvData) -1;
    d->port = port;
    d->xfer_table = create_hashtable(8192, fdhash, fdeq);
    if (d->xfer_table == NULL) {
        driver_free(d);
        return (ErlDrvData) -1;
    }
    return (ErlDrvData)d;
}

static void yaws_sendfile_drv_stop(ErlDrvData handle)
{
    Desc* d = (Desc*)handle;
    hashtable_destroy(d->xfer_table, 1);
    driver_free(d);
}

typedef union {
    off_t offset;
    size_t size;
    ssize_t count;
    uint64_t bits;
    unsigned char bytes[8];
} U64_t;

typedef union {
    char* buffer;
    struct {
        U64_t    offset;
        U64_t    count;
        uint32_t out_fd;
        char     filename[1];
    }* args;
    struct {
        U64_t         count;
        uint32_t      out_fd;
        unsigned char success;
        char          errno_string[1];
    }* result;
} Buffer;

static size_t set_error_buffer(Buffer* b, int socket_fd, int err)
{
    char* s, *t;
    size_t result_size = sizeof *(b->result);
    memset(b->result, 0, result_size);
    put_int32(socket_fd, &(b->result->out_fd));
    s = erl_errno_id(err);
    if (strcmp(s, "unknown") == 0 && err == EOVERFLOW) {
        s = "EOVERFLOW";
    }
    for (t = b->result->errno_string; *s; s++, t++) {
        *t = tolower(*s);
    }
    *t = '\0';
    return result_size - 1 + t - b->result->errno_string;
}

static ssize_t yaws_sendfile_call(int out_fd, int in_fd, off_t* offset, size_t count)
{
#if defined(__linux__)
    off_t cur = *offset;
    ssize_t retval;
    do {
        retval = sendfile(out_fd, in_fd, offset, count);
    } while (retval < 0 && errno == EINTR);
    if (retval >= 0 && retval != count) {
        if (*offset == cur) {
            *offset += retval;
        }
        retval = -1;
	errno = EAGAIN;
    }
    return retval;
#elif defined(__APPLE__) && defined(__MACH__)
    off_t len = count;
    int retval;
    do {
        retval = sendfile(in_fd, out_fd, *offset, &len, NULL, 0);
    } while (retval < 0 && errno == EINTR);
    if (retval < 0 && errno == EAGAIN) {
        *offset += len;
    }
    return retval == 0 ? len : retval;
#elif defined(__FreeBSD__)
    off_t len = 0;
    int retval;
    do {
        retval = sendfile(in_fd, out_fd, *offset, count, NULL, &len, 0);
    } while (retval < 0 && errno == EINTR);
    if (retval < 0 && errno == EAGAIN) {
        *offset += len;
    }
    return retval == 0 ? len : retval;
#else
    errno = ENOSYS;
    return -1;
#endif
}

static void yaws_sendfile_drv_output(ErlDrvData handle, char* buf, int buflen)
{
    int fd, socket_fd;
    Desc* d = (Desc*)handle;
    Buffer b;
    b.buffer = buf;
    socket_fd = get_int32(&(b.args->out_fd));
    fd = open(b.args->filename, O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        int out_buflen = set_error_buffer(&b, socket_fd, errno);
        driver_output(d->port, buf, out_buflen);
    } else {
        Transfer* xfer;
        SocketFd sfd;
        sfd.socket_fd = socket_fd;
        xfer = (Transfer*)hashtable_search(d->xfer_table, sfd.hashkey);
        if (xfer == NULL) {
            /* Transfer objects are intentionally not freed until the
               driver stops, or if an insertion error occurs below. */
            xfer = (Transfer*)malloc(sizeof(Transfer));
            if (xfer == NULL) {
                int out_buflen = set_error_buffer(&b, socket_fd, ENOMEM);
                driver_output(d->port, buf, out_buflen);
                return;
            }
            if (!hashtable_insert(d->xfer_table, sfd.hashkey, xfer)) {
                int out_buflen = set_error_buffer(&b, socket_fd, ENOMEM);
                driver_output(d->port, buf, out_buflen);
                free(xfer);
                return;
            }
        }
        xfer->file_fd = fd;
        xfer->offset = get_int64(&(b.args->offset.offset));
        xfer->count = get_int64(&(b.args->count.size));
        xfer->total = 0;
        driver_select(d->port, sfd.ev_data, DO_WRITE, 1);
    }
}


static void yaws_sendfile_drv_ready_output(ErlDrvData handle, ErlDrvEvent ev)
{
    Desc* d = (Desc*)handle;
    ssize_t result;
    off_t cur_offset;
    Transfer* xfer;
    SocketFd* sfd = (SocketFd*)&ev;
    xfer = (Transfer*)hashtable_search(d->xfer_table, sfd->hashkey);
    if (xfer == NULL) {
        /* fatal error, something is very wrong */
        driver_failure_atom(d->port, "socket_fd_not_found");
        return;
    }
    cur_offset = xfer->offset;
    result = yaws_sendfile_call(sfd->socket_fd, xfer->file_fd,
                                &xfer->offset, xfer->count);
    if (result < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        off_t written = xfer->offset - cur_offset;
        xfer->count -= written;
        xfer->total += written;
    } else {
        int save_errno = errno;
        int out_buflen;
        char buf[36];
        Buffer b;
        b.buffer = buf;
        memset(buf, 0, sizeof buf);
        driver_select(d->port, ev, DO_WRITE, 0);
        close(xfer->file_fd);
        if (result < 0) {
            out_buflen = set_error_buffer(&b, sfd->socket_fd, save_errno);
        } else {
            uint64_t total = xfer->total + result;
            put_int64(total, &(b.result->count.count));
            put_int32(sfd->socket_fd, &(b.result->out_fd));
            b.result->success = 1;
            b.result->errno_string[0] = '\0';
            out_buflen = sizeof(*b.result);
        }
        xfer->file_fd = -1;
        xfer->offset = xfer->count = xfer->total = 0;
        driver_output(d->port, buf, out_buflen);
    }
}

static ErlDrvEntry yaws_sendfile_driver_entry = {
    NULL,
    yaws_sendfile_drv_start,
    yaws_sendfile_drv_stop,
    yaws_sendfile_drv_output,
    NULL,
    yaws_sendfile_drv_ready_output,
    "yaws_sendfile_drv",
    NULL,
    NULL,
    NULL,
    NULL
};

DRIVER_INIT(yaws_sendfile_drv)
{
    return &yaws_sendfile_driver_entry;
}
