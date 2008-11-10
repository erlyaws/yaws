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
} Desc;

static ErlDrvData yaws_sendfile_drv_start(ErlDrvPort port, char* buf)
{
    Desc* d = (Desc*)driver_alloc(sizeof(Desc));
    if (d == NULL) return (ErlDrvData) -1;
    d->port = port;
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

static int set_error_buffer(Buffer b, int err)
{
    char* s;
    char* t;
    memset(b.result, 0, sizeof(*b.result));
    for (s = erl_errno_id(err), t = b.result->errno_string; *s; s++, t++) {
        *t = tolower(*s);
    }
    return sizeof(*b.result) - 1 + t - b.result->errno_string;
}

#if defined(__linux__)
#define CALL_SENDFILE(out_fd, in_fd, offset, count) sendfile((out_fd), (in_fd), (offset), (count))
#elif defined(__APPLE__) && defined(__MACH__)
static ssize_t CALL_SENDFILE(int out_fd, int in_fd, off_t* offset, size_t count)
{
    off_t len = count;
    int retval = sendfile(in_fd, out_fd, *offset, &len, NULL, 0);
    return retval == 0 ? len : retval;
}
#else
#define CALL_SENDFILE(out_fd, in_fd, offset, count) -1
#endif

static void yaws_sendfile_drv_output(ErlDrvData handle, char* buf, int buflen)
{
    int fd;
    int out_buflen;
    Desc* d = (Desc*)handle;
    Buffer b;
    b.buffer = buf;
    fd = open(b.args->filename, O_RDONLY);
    if (fd < 0) {
        out_buflen = set_error_buffer(b, errno);
    } else {
        ssize_t result = CALL_SENDFILE(b.args->out_fd, fd, &b.args->offset.offset, b.args->count.size);
        close(fd);
        if (result < 0) {
            out_buflen = set_error_buffer(b, errno);
        } else {
            b.result->count.count = result;
            b.result->success = 1;
            b.result->errno_string[0] = '\0';
            out_buflen = sizeof(*b.result);
        }
    }
    assert(out_buflen <= buflen);
    driver_output(d->port, buf, out_buflen);
}

static ErlDrvEntry yaws_sendfile_driver_entry = {
    NULL,
    yaws_sendfile_drv_start,
    yaws_sendfile_drv_stop,
    yaws_sendfile_drv_output,
    NULL,
    NULL,
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
