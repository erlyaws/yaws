#!/bin/sh

daemon="${TRUEPREFIX}/bin/yaws --daemon"
daemon_flags="--conf ${SYSCONFDIR}/yaws/yaws.conf"

. /etc/rc.d/rc.subr

pexp="${LOCALBASE}/lib/erlang/erts.*${TRUEPREFIX}/lib/yaws/ebin -run yaws"

rc_reload() {
	${rcexec} "${daemon} --hup"
}

rc_stop() {
	${rcexec} "${daemon} --stop"
}

rc_cmd $1
