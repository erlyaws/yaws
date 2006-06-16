#!/bin/bash
#
# Startup script for the Yaws Web Server
#
# config: /etc/yaws.conf
#




yaws=%prefix%/bin/yaws
prog=yaws
yawsid=myserverid

test -x $yaws || exit 1


case "$1" in
    start)
	echo -n "Starting $prog: "
	start-stop-daemon --start --quiet --exec $yaws -- --id ${yawsid} --daemon --heart
	echo "."
    ;;
    stop)
	echo -n "Stopping $prog: "
	start-stop-daemon --stop --quiet --exec $yaws --id ${yawsid} --stop
	echo "."
	;;
    reload)
	echo -n "Reloading $prog: "
	start-stop-daemon  --exec $yaws --id ${yawsid} --hup
	echo "."
	;;
    status)
	start-stop-daemon  --exec $yaws --id ${yawsid} --status
	echo "."
	;;
    *)
	echo $"Usage: $prog {start|stop|restart|reload|status}"
	exit 1
esac

exit 0

