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
     

debug=false
daemon=" -daemon "

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
		daemon=""
		break;;
	   -d)
		debug=true
		break;;
           -c)
		conf=$1
		shift
		break;;
	    *)
		help
       esac
done

exec $erl $daemon -pa ${yawsdir}/ebin -s yaws -yaws conf xx$conf debug xx$debug

