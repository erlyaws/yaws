/* author: klacke@hyber.org                                  */
/* purpose, make us run under a different username           */
/*          as well as iface to some other idiotic syscalls  */
/*          FIXME replace this entirely with a proper        */
/*          posix interface                                  */


#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>
#include <stdlib.h>

#include "erl_driver.h"



static ErlDrvData setuid_start(ErlDrvPort port, char *buf);
static void setuid_stop(ErlDrvData drv_data);

static ErlDrvEntry setuid_driver_entry;


/* buf is the name of the intented user */
static ErlDrvData setuid_start(ErlDrvPort port, char *buf)
{
    char *t;
    char xbuf[BUFSIZ];
    struct passwd *pe;

    if ((t = strchr(buf, ' ')) == NULL)
        return (ErlDrvData) -1;

    /* Rewind pw mapping */
    setpwent();

    t++;
    switch (*t++) {
    case 's':  /* setuid */
        while ((pe = getpwent())) {
            if (strcmp(pe->pw_name , t) == 0) {
                if ((setuid(pe->pw_uid)  != 0) ||
                    (setreuid(pe->pw_uid, pe->pw_uid) != 0)) {
                    return (ErlDrvData) -1;
                }
                sprintf(xbuf, "ok %d", pe->pw_uid);
                endpwent();
                driver_output(port,xbuf, strlen(xbuf));
                return (ErlDrvData) port;
            }
        }
        endpwent();
        break;
    case 'n': {
        int uid = atoi(t);
        while ((pe = getpwent())) {
            if (pe->pw_uid == uid) {
                sprintf(xbuf, "ok %s", pe->pw_name);
                endpwent();
                driver_output(port,xbuf, strlen(xbuf));
                return (ErlDrvData) port;
            }
        }
        endpwent();
        driver_output(port, "ok -", 4);
        return (ErlDrvData) port;
    }
    case 'g':   /* getuid */
        sprintf(xbuf, "ok %d", getuid());
        driver_output(port,xbuf, strlen(xbuf));
        return (ErlDrvData) port;
    case 'u':
        while ((pe = getpwent())) {
            if (strcmp(pe->pw_name , t) == 0) {
                sprintf(xbuf, "ok %d", pe->pw_uid);
                endpwent();
                driver_output(port,xbuf, strlen(xbuf));
                return (ErlDrvData) port;
            }
        }
        endpwent();
        break;
    case 'h':
        while ((pe = getpwent())) {
            if (strcmp(pe->pw_name , t) == 0) {
                sprintf(xbuf, "ok %s", pe->pw_dir);
                endpwent();
                driver_output(port,xbuf, strlen(xbuf));
                return (ErlDrvData) port;
            }
        }
        endpwent();
        break;
    }
    // In any case return error(?) for non void function
    return (ErlDrvData) -1;


}


static void setuid_stop(ErlDrvData drv_data)
{
}




/*
 * Initialize and return a driver entry struct
 */




DRIVER_INIT(setuid_drv)
{
    setuid_driver_entry.init            = NULL;   /* Not used */
    setuid_driver_entry.start           = setuid_start;
    setuid_driver_entry.stop            = setuid_stop;
    setuid_driver_entry.output          = NULL;
    setuid_driver_entry.ready_input     = NULL;
    setuid_driver_entry.ready_output    = NULL;
    setuid_driver_entry.driver_name     = "setuid_drv";
    setuid_driver_entry.finish          = NULL;
    setuid_driver_entry.handle          = NULL;
    setuid_driver_entry.control         = NULL;
    setuid_driver_entry.timeout         = NULL;
    setuid_driver_entry.outputv         = NULL;
    setuid_driver_entry.ready_async     = NULL;
    setuid_driver_entry.flush           = NULL;
    setuid_driver_entry.call            = NULL;
    setuid_driver_entry.event           = NULL;
    setuid_driver_entry.extended_marker = ERL_DRV_EXTENDED_MARKER;
    setuid_driver_entry.major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION;
    setuid_driver_entry.minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION;
    setuid_driver_entry.driver_flags    = 0;
    setuid_driver_entry.handle2         = NULL;
    setuid_driver_entry.process_exit    = NULL;
    setuid_driver_entry.stop_select     = NULL;
    return (ErlDrvEntry*) &setuid_driver_entry;
}


