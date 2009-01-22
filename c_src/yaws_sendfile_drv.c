/* Interface to sendfile system call for Yaws                */
/* author: vinoski@ieee.org                                  */
/* Created : 09 Nov 2008 by Steve Vinoski <vinoski@ieee.org> */

#include <errno.h>
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

typedef struct _transfer {
    off_t offset;
    size_t count;
    ssize_t total;
    int file_fd;
} Transfer;

typedef struct hashtable* Transfers;

typedef struct _desc {
    ErlDrvPort port;
    Transfers xfer_table;
} Desc;

static unsigned int fdhash(void* k)
{
    return *(unsigned int*)&k;
}

static int fdeq(void* k1, void* k2)
{
    return *(int*)&k1 == *(int*)&k2;
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
    u_int64_t bits;
} U64_t;

typedef union {
    char* buffer;
    struct {
        U64_t        offset;
        U64_t        count;
        unsigned int out_fd: 32;
        char         filename[1];
    }* args;
    struct {
        U64_t         count;
        unsigned int  out_fd: 32;
        unsigned char success;
        char          errno_string[1];
    }* result;
} Buffer;

static int set_error_buffer(Buffer* b, int socket_fd, int err)
{
    char* s, *t;
    memset(b->result, 0, sizeof *b->result);
    b->result->out_fd = socket_fd;
    for (s = erl_errno_id(err), t = b->result->errno_string; *s; s++, t++) {
        *t = tolower(*s);
    }
    *t = '\0';
    return sizeof(*b->result) - 1 + t - b->result->errno_string;
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
    socket_fd = b.args->out_fd;
    fd = open(b.args->filename, O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        int out_buflen = set_error_buffer(&b, socket_fd, errno);
        driver_output(d->port, buf, out_buflen);
    } else {
        Transfer* xfer = (Transfer*)hashtable_search(d->xfer_table, *(void**)&socket_fd);
        if (xfer == NULL) {
            xfer = (Transfer*)malloc(sizeof(Transfer));
            if (xfer == NULL) {
                int out_buflen = set_error_buffer(&b, socket_fd, ENOMEM);
                driver_output(d->port, buf, out_buflen);
                return;
            }
            if (!hashtable_insert(d->xfer_table, *(void**)&socket_fd, xfer)) {
                int out_buflen = set_error_buffer(&b, socket_fd, ENOMEM);
                driver_output(d->port, buf, out_buflen);
                free(xfer);
                return;
            }
        }
        xfer->file_fd = fd;
        xfer->offset = b.args->offset.offset;
        xfer->count = b.args->count.size;
        xfer->total = 0;
        driver_select(d->port, *(ErlDrvEvent*)&socket_fd, DO_WRITE, 1);
    }
}



static void yaws_sendfile_drv_ready_output(ErlDrvData handle, ErlDrvEvent ev)
{
    Desc* d = (Desc*)handle;
    int socket_fd = *(int*)&ev;
    Transfer* xfer = (Transfer*)hashtable_search(d->xfer_table, *(void**)&socket_fd);
    if (xfer == NULL) {
        /* fatal error, something is very wrong */
        driver_failure_atom(d->port, "socket_fd_not_found");
        return;
    }
    off_t cur_offset = xfer->offset;
    ssize_t result = yaws_sendfile_call(socket_fd, xfer->file_fd, &xfer->offset, xfer->count);
    if (result < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        off_t written = xfer->offset - cur_offset;
        xfer->count -= written;
        xfer->total += written;
    } else {
        int out_buflen;
        char buf[36];
        Buffer b;
        b.buffer = buf;
        memset(buf, 0, sizeof buf);
        driver_select(d->port, *(ErlDrvEvent*)&socket_fd, DO_WRITE, 0);
        close(xfer->file_fd);
        if (result < 0) {
            out_buflen = set_error_buffer(&b, socket_fd, errno);
        } else {
            b.result->count.count = xfer->total + result;
            b.result->out_fd = socket_fd;
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
