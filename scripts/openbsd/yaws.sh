#!/bin/sh

daemon="/usr/local/bin/yaws"
daemon_flags="--daemon --conf /etc/yaws/yaws.conf"

. /etc/rc.d/rc.subr

pexp="/usr/local/lib/erlang/erts.*/usr/local/lib/yaws/ebin -run yaws"

rc_stop() {
	su -l -c ${daemon_class} -s ${daemon_shell} ${daemon_user} \
		-c "${daemon} --stop" >/dev/null
}

rc_reload() {
	su -l -c ${daemon_class} -s ${daemon_shell} ${daemon_user} \
		-c "${daemon} --hup" >/dev/null
}

rc_cmd $1
