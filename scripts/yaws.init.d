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

yaws=/usr/local/bin/yaws
prog=yaws

start() {
        echo -n $"Starting $prog: "
        daemon $yaws -D -heart
        RETVAL=$?
        echo
        [ $RETVAL = 0 ] && touch /var/lock/subsys/yaws
        return $RETVAL
}

stop() {
	echo -n $"Stopping $prog: "
	str=`$yaws -s`
	if [ "$str" = "stopping" ]; then
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
	r=`$yaws -h` 
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
