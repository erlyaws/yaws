#!/bin/sh


yawsdir=%yawsdir%
erl=%erl%


help()
{
	echo "usage: yaws -i         -- interactive (no daemon) mode"
	echo "       yaws -d         -- debug mode"
	echo "       yaws -c file    -- set config file"
	echo "       yaws -t         -- trace all traffic"
	echo "       yaws -T         -- trace http traffic"
	exit 1
}
     

debug=""
daemon=" -detached "
trace=""
conf=""

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
		daemon="";;
	   -d)
		debug=" -boot start_sasl -yaws debug ";;
	   -t)
	        trace=" -yaws trace traffic ";;
	   -T)
	        trace=" -yaws trace http ";;
           -c)
		conf=" -conf $1 "
		shift;;
	    *)
		help
       esac
done

exec $erl $daemon ${debug} -pa ${yawsdir}/ebin -s yaws $trace $conf

