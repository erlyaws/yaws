#!/bin/sh


yawsdir=%yawsdir%
erl=%erl%


ENV_PGM=`which env`


help()
{
	echo "usage: server modes ... "
	echo "       yaws -i         -- interactive (no daemon) mode"
	echo "       yaws -D         -- daemon mode"
	echo "       yaws -d         -- debug mode"
	echo "       yaws -c file    -- set config file"
	echo "       yaws -r mod     -- call mod:start/0 at startup"
	echo "       yaws -t         -- trace all traffic"
	echo "       yaws -T         -- trace http traffic"
	echo "       yaws -v         -- print version"
	echo ""
	echo "       yaws -heart     -- auto restart yaws if it crashes"
	echo "                          (requires the -D switch)"
	echo ""
	echo ""
	echo "ctl functions ... "
	echo "        yaws -h         -- hup the daemon  "
	echo "        yaws -s         -- stop the daemon "
	echo "        yaws -S         -- query the daemon status "
	exit 1
}
     

debug="";
daemon="";
interactive="";
trace="";
conf="";
runmod="";
sname="";
heart="";

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
                interactive="true";
		debug=" -yaws debug ";
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
           -r)
		runmod=" -runmod $1 "
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
           -sname)
		sname=" -sname $1 "
		shift;;
           -heart)
		heart=" -heart ";;
           -check)
                out=`exec $erl -noshell -pa ${yawsdir}/ebin -s yaws_ctl check $1`
		if [ "$out" = "ok" ]; then
		    exit 0
		fi
		echo $out
		exit 1;;
	    *)
		help
       esac
done

[ -z "$daemon" ] && [ -z "$interactive" ] && help

if [ -z "$heart" ] || [ -z "$daemon" ]; then
    HEART_COMMAND="";
else
    ## ............................this line
    export HEART_COMMAND="${ENV_PGM} HEART=true $erl ${daemon} ${heart} -pa ${yawsdir}/ebin  ${sname} ${debug} -s yaws $trace $conf $runmod";
fi

## keep this line in sync with ....^
exec $erl ${daemon} ${heart} -pa ${yawsdir}/ebin  ${sname} ${debug} -s yaws $trace $conf $runmod


