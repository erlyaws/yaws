#!/bin/sh
#

#
# PROVIDE: yaws
# REQUIRE: DAEMON
#
# You will need to set some variables in /etc/rc.conf to start Yaws:
#
# yaws=YES
# yaws_flags=""
# yaws_id=""
#

if [ -f /etc/rc.subr ]
then
        . /etc/rc.subr
fi

name="yaws"
rcvar=$name
yaws_command="%prefix%bin/${name}"
required_files="%etcdir%yaws.conf"

start_cmd="yaws_start"
stop_cmd="yaws_stop"
status_cmd="yaws_status"
reload_cmd="yaws_reload"
extra_commands="reload status"

: ${yaws_id:=default}
: ${yaws_flags:=--heart}

yaws_start() {
        $yaws_command --id $yaws_id $yaws_flags --daemon
}

yaws_stop() {
        $yaws_command --id $yaws_id --stop 
}

yaws_status() {
        $yaws_command --id $yaws_id --status
}

yaws_reload() {
        $yaws_command --id $yaws_id --hup
}

if [ -f /etc/rc.subr -a -f /etc/rc.conf ]
then
        load_rc_config $name
        run_rc_command "$1"
else
        yaws_start
fi
