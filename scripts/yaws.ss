#!/bin/sh


yawsdir=%yawsdir%
erl=%erl%


help()
{
	echo "usage: server modes ... "
	echo "       yaws -i         -- interactive (no daemon) mode"
	echo "       yaws -D         -- daemon mode"
	echo "       yaws -d         -- debug mode"
	echo "       yaws -c file    -- set config file"
	echo "       yaws -t         -- trace all traffic"
	echo "       yaws -T         -- trace http traffic"
	echo "       yaws -v         -- print version"
	echo ""
	echo ""
	echo "ctl functions ... "
	echo "        yaws -h         -- hup the daemon  "
	echo "        yaws -s         -- stop the daemon "
	echo "        yaws -S         -- query the daemon status "
	exit 1
}
     

debug=""
daemon="";
interactive="";
trace=""
conf=""

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
                interactive="true";
		daemon="";;
           -D)
    	        daemon=" -detached ";;
	   -d)
                set -x
		debug=" -boot start_sasl -yaws debug ";;
	   -t)
	        trace=" -yaws trace traffic ";;
	   -T)
	        trace=" -yaws trace http ";;
           -c)
		conf=" -conf $1 "
		shift;;
	   -h)
	        exec $erl -noshell -pa ${yawsdir}/ebin -s yaws_ctl hup;
		exit normal;;
	   -s)
	        exec $erl -noshell -pa ${yawsdir}/ebin -s yaws_ctl stop;
		exit normal;;
	   -S)
	        exec $erl -noshell -pa ${yawsdir}/ebin -s yaws_ctl status;
		exit normal;;
	   -v) 
	        exec $erl -noshell -pa ${yawsdir}/ebin -s yaws printversion;
		exit normal;;
	    *)
		help
       esac
done

[ -z "$daemon" ] && [ -z "$interactive" ] && help

exec $erl ${daemon} -pa ${yawsdir}/ebin  ${debug} -s yaws $trace $conf

