#! /bin/sh


YAWS_BIN=%prefix%bin/yaws

## By default we run with the default id
# YAWS_ID_OPTS=--id myserverid

conf="--conf %etcdir%yaws/yaws.conf"

test -x $YAWS_BIN || exit 5



# Shell functions sourced from /etc/rc.status:
#      rc_check         check and set local and overall rc status
#      rc_status        check and set local and overall rc status
#      rc_status -v     ditto but be verbose in local rc status
#      rc_status -v -r  ditto and clear the local rc status
#      rc_failed        set local and overall rc status to failed
#      rc_failed <num>  set local and overall rc status to <num><num>
#      rc_reset         clear local rc status (overall remains)
#      rc_exit          exit appropriate to overall rc status
#      rc_active	checks whether a service is activated by symlinks
. /etc/rc.status

# First reset status of this service
rc_reset

# Return values acc. to LSB for all commands but status:
# 0 - success
# 1 - generic or unspecified error
# 2 - invalid or excess argument(s)
# 3 - unimplemented feature (e.g. "reload")
# 4 - insufficient privilege
# 5 - program is not installed
# 6 - program is not configured
# 7 - program is not running
# 
# Note that starting an already running service, stopping
# or restarting a not-running service as well as the restart
# with force-reload (in case signalling is not supported) are
# considered a success.

case "$1" in
    start)
	echo -n "Starting YAWS"
	## Start daemon with startproc(8). If this fails
	## the echo return value is set appropriate.

	# NOTE: startproc returns 0, even if service is 
	# already running to match LSB spec.
	startproc $YAWS_BIN --daemon --heart  ${YAWS_ID_OPTS} ${conf}

	# Remember status and be verbose
	rc_status -v
	;;
    stop)
	echo -n "Shutting down YAWS"
	## Stop daemon with killproc(8) and if this fails
	## set echo the echo return value.

	startproc $YAWS_BIN --stop  ${YAWS_ID_OPTS}

	# Remember status and be verbose
	rc_status -v
	;;
    try-restart)
	## Stop the service and if this succeeds (i.e. the 
	## service was running before), start it again.
	## Note: try-restart is not (yet) part of LSB (as of 0.7.5)
	$0 status >/dev/null &&  $0 restart

	# Remember status and be quiet
	rc_status
	;;
    restart)
	## Stop the service and regardless of whether it was
	## running or not, start it again.
	$0 stop
	$0 start

	# Remember status and be quiet
	rc_status
	;;
    force-reload)
	## Signal the daemon to reload its config. Most daemons
	## do this on signal 1 (SIGHUP).
	## If it does not support it, restart.

	echo -n "Force Reload service YAWS"
	## if it supports it:
	$YAWS_BIN  ${YAWS_ID_OPTS} --hup
	rc_status -v

	;;
    reload)
	## Like force-reload, but if daemon does not support
	## signalling, do nothing (!)

	# If it supports signalling:

	startproc $YAWS_BIN  ${YAWS_ID_OPTS} --hup
	rc_status -v
	
	;;
    status)
	echo -n "Checking for service FOO: "
	## Check status with checkproc(8), if process is running
	## checkproc will return with exit status 0.

	# Return value is slightly different for the status command:
	# 0 - service running
	# 1 - service dead, but /var/run/  pid  file exists
	# 2 - service dead, but /var/lock/ lock file exists
	# 3 - service not running

	# NOTE: checkproc returns LSB compliant status values.
	checkproc $YAWS_BIN  ${YAWS_ID_OPTS} --status
	rc_status -v
	;;
    probe)
	## Optional: Probe for the necessity of a reload,
	## print out the argument which is required for a reload.

	test /etc/FOO/FOO.conf -nt /var/run/FOO.pid && echo reload
	;;
    *)
	echo "Usage: $0 {start|stop|status|try-restart|restart|force-reload|reload|probe}"
	exit 1
	;;
esac
rc_exit
