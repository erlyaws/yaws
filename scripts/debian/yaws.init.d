#!/bin/bash
#
# Startup script for the Yaws Web Server
#
# config: /etc/yaws.conf
#




yaws=%prefix%/bin/yaws
prog=yaws
yawsid=myserverid
conf="--conf %etcdir%/yaws.conf"

test -x $yaws || exit 1


case "$1" in
    start)
	echo -n "Starting $prog: "
	$yaws  --id ${yawsid} --daemon --heart ${conf}
	echo "."
    ;;
    stop)
	echo -n "Stopping $prog: "
	$yaws  --id ${yawsid} --stop
	echo "."
	;;
    reload)
	echo -n "Reloading $prog: "
	$yaws  --id ${yawsid} --hup
	echo "."
	;;
    status)
	$yaws  --id ${yawsid} --status
	echo "."
	;;
    restart)
	echo -n "Stopping $prog: "
        $yaws  --id ${yawsid} --stop
	echo -n "Starting $prog: "
        $yaws  --id ${yawsid} --daemon --heart
	echo "."
        ;;

    *)
	echo $"Usage: $prog {start|stop|restart|reload|status}"
	exit 1
esac

exit 0

