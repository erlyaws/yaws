#!/bin/sh


yawsdir=%yawsdir%
erl=%erl%


help()
{
	echo "usage: yaws -i         -- interactive (no daemon) mode"
	echo "       yaws -d         -- debug mode"
	echo "       yaws -c file    -- set config file"
	exit 1
}
     

debug=""
daemon=" -daemon "

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
		daemon="";;
	   -d)
		debug=" -boot start_sasl -yaws debug ";;
           -c)
		conf=$1
		shift;;
	    *)
		help
       esac
done

exec $erl $daemon ${debug} -pa ${yawsdir}/ebin -s yaws -yaws conf xx${conf}

