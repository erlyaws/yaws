#!/bin/bash
#
# Startup script for the Yaws Web Server
#
# config: /etc/yaws.conf
#




yaws=%prefix%/bin/yaws
prog=yaws

# By default we run with the default id
# idopts=--id myserverid

conf="--conf %etcdir%/yaws/yaws.conf"

test -x $yaws || exit 1


case "$1" in
    start)
	echo -n "Starting $prog: "
	$yaws  ${idopts} --daemon --heart ${conf}
	echo "."
    ;;
    stop)
	echo -n "Stopping $prog: "
	$yaws  ${idopts} --stop
	echo "."
	;;
    reload)
	echo -n "Reloading $prog: "
	$yaws  ${idopts} --hup
	echo "."
	;;
    status)
	$yaws  ${idopts} --status
	echo "."
	;;
    restart)
	echo -n "Stopping $prog: "
        $yaws  ${idopts} --stop
	echo -n "Starting $prog: "
        $yaws  ${idopts} --daemon --heart
	echo "."
        ;;

    *)
	echo $"Usage: $prog {start|stop|restart|reload|status}"
	exit 1
esac

exit 0

