/* author: klacke@hyber.org                                  */
/* purpose, make us run under a different username           */

#ifndef WIN32
#include <unistd.h>
#endif


#include <sys/types.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pwd.h>

#include "erl_driver.h"
#ifndef ERL_DRV_NIL
#include "erl_driver_compat.h"
#endif



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
    t++;

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
    setuid_driver_entry.init         = NULL;   /* Not used */
    setuid_driver_entry.start        = setuid_start;
    setuid_driver_entry.stop         = setuid_stop;
    setuid_driver_entry.output       = NULL;
    setuid_driver_entry.ready_input  = NULL;
    setuid_driver_entry.ready_output = NULL;
    setuid_driver_entry.driver_name  = "setuid_drv";
    setuid_driver_entry.finish       = NULL;
    setuid_driver_entry.control      = NULL;
    setuid_driver_entry.outputv      = NULL;
    return (ErlDrvEntry*) &setuid_driver_entry;
}


