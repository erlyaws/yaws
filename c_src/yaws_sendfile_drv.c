/* Interface to sendfile system call for Yaws                */
/* author: vinoski@ieee.org                                  */
/* Created : 09 Nov 2008 by Steve Vinoski <vinoski@ieee.org> */

#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <assert.h>
#if defined(__linux__)
#include <sys/sendfile.h>
#elif defined(__APPLE__) && defined(__MACH__)
#include <sys/socket.h>
#include <sys/uio.h>
#else
#error "yaws_sendfile_drv not supported on this platform"
#endif

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif

typedef struct _desc {
    ErlDrvPort port;
    int socket_fd;
    int file_fd;
    off_t offset;
    size_t count;
    ssize_t total;
    unsigned int write_ready: 1;
    unsigned int read_ready:  1;
} Desc;

static ErlDrvData yaws_sendfile_drv_start(ErlDrvPort port, char* buf)
{
    Desc* d = (Desc*)driver_alloc(sizeof(Desc));
    if (d == NULL) return (ErlDrvData) -1;
    d->port = port;
    d->socket_fd = d->file_fd = -1;
    d->offset = d->count = d->total = d->write_ready = d->read_ready = 0;
    return (ErlDrvData)d;
}

static void yaws_sendfile_drv_stop(ErlDrvData drv_data)
{
    driver_free(drv_data);
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
        unsigned char success;
        char          errno_string[1];
    }* result;
} Buffer;

static int set_error_buffer(Buffer* b, int err)
{
    char* s;
    char* t;
    memset(b->result, 0, sizeof *b->result);
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
    } while (retval < -1 && errno == EINTR);
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
#else
    errno = ENOSYS;
    return -1;
#endif
}

static void yaws_sendfile_drv_output(ErlDrvData handle, char* buf, int buflen)
{
    int fd;
    int out_buflen = 0;
    Desc* d = (Desc*)handle;
    Buffer b;
    b.buffer = buf;
    fd = open(b.args->filename, O_RDONLY | O_NONBLOCK);
    if (fd < 0) {
        out_buflen = set_error_buffer(&b, errno);
    } else {
        d->socket_fd = b.args->out_fd;
        d->file_fd = fd;
        d->offset = b.args->offset.offset;
        d->count = b.args->count.size;
        driver_select(d->port, (ErlDrvEvent)d->file_fd, DO_READ, 1);
        driver_select(d->port, (ErlDrvEvent)d->socket_fd, DO_WRITE, 1);
    }
    if (out_buflen != 0) {
        driver_output(d->port, buf, out_buflen);
    }
}

static void yaws_sendfile_do_send(Desc* d)
{
    off_t cur_offset = d->offset;
    ssize_t result;
    result = yaws_sendfile_call(d->socket_fd, d->file_fd, &d->offset, d->count);
    if (result < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
        off_t written = d->offset - cur_offset;
        d->count -= written;
        d->total += written;
    } else {
        int out_buflen;
        char buf[32];
        Buffer b;
        b.buffer = buf;
        memset(buf, 0, sizeof buf);
        driver_select(d->port, (ErlDrvEvent)d->file_fd, DO_READ, 0);
        driver_select(d->port, (ErlDrvEvent)d->socket_fd, DO_WRITE, 0);
        close(d->file_fd);
        if (result < 0) {
            out_buflen = set_error_buffer(&b, errno);
        } else {
            b.result->count.count = d->total + result;
            b.result->success = 1;
            b.result->errno_string[0] = '\0';
            out_buflen = sizeof(*b.result);
        }
        d->socket_fd = d->file_fd = -1;
        d->offset = d->count = d->total = 0;
        driver_output(d->port, buf, out_buflen);
    }
    d->write_ready = d->read_ready = 0;
}

static void yaws_sendfile_drv_ready_input(ErlDrvData handle, ErlDrvEvent ev)
{
    Desc* d = (Desc*)handle;
    d->read_ready = 1;
    if (d->write_ready) {
        yaws_sendfile_do_send(d);
    }
}

static void yaws_sendfile_drv_ready_output(ErlDrvData handle, ErlDrvEvent ev)
{
    Desc* d = (Desc*)handle;
    d->write_ready = 1;
    if (d->read_ready) {
        yaws_sendfile_do_send(d);
    }
}

static ErlDrvEntry yaws_sendfile_driver_entry = {
    NULL,
    yaws_sendfile_drv_start,
    yaws_sendfile_drv_stop,
    yaws_sendfile_drv_output,
    yaws_sendfile_drv_ready_input,
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
