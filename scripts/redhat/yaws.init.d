#!/bin/bash
#
# Startup script for the Yaws Web Server
#
# config: /etc/yaws.conf
#
# chkconfig: 2345 65 35
# description: yaws - Erlang enabled http server
# use "/sbin/chkconfig --add yaws" to install

# Source function library.

. /etc/rc.d/init.d/functions

yaws=%prefix%/bin/yaws
prog=yaws
#
# Default yawsid is "default". If you change this to another ID,
# be sure to also uncomment the yawsid_opts line just below.
#
yawsid=default
#yawsid_opts="--id $yawsid"
conf="--conf %etcdir%yaws/yaws.conf"

start() {
        echo -n $"Starting $prog: "
        daemon $yaws ${yawsid_opts} --daemon --heart ${conf}
        RETVAL=$?
        echo
        [ $RETVAL = 0 ] && touch /var/lock/subsys/yaws
        return $RETVAL
}

stop() {
	echo -n $"Stopping $prog: "
	str=`$yaws ${yawsid_opts} --stop`
	if [ "$str" = "stopping yaws with id=$yawsid" ]; then
	    echo_success
	    RETVAL=0
	else
	    echo_failure
	    RETVAL=1
	fi
	echo
	rm -f /var/lock/subsys/yaws /var/run/yaws.pid
}


reload() {
	echo -n $"Reloading $prog: "
	r=`$yaws ${yawsid_opts} --hup` 
	RETVAL=$?
	echo $r
}

# See how we were called.
case "$1" in
  start)
	start
	;;
  stop)
	stop
	;;
  status)
        $yaws -S
	RETVAL=$?
	;;
  restart)
	stop
	start
	;;
  condrestart)
	if [ -f /tmp/yaws.ctl ] ; then
		stop
		start
	fi
	;;
  reload)
        reload
	;;
  help)
	$yaws -?
	;;
  *)
	echo $"Usage: $prog {start|stop|restart|condrestart|reload|status|fullstatus|help}"
	exit 1
esac

exit $RETVAL
