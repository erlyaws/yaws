#!/bin/sh


yawsdir="%yawsdir%"
erl="%erl%"
case `uname` in
    CYGWIN*)
	yawsdir=`cygpath --windows $yawsdir`
	werl="%werl%"
	delim=\\;;
    *)
        delim=/
esac


ENV_PGM=`which env`


help()
{
	echo "usage: server modes ... "
	echo "       yaws -i         -- interactive (no daemon) mode"
        echo "       yaws -w         -- cygwin interactive (werl) "
	echo "       yaws -D         -- daemon mode"
	echo "       yaws -d         -- debug mode"
	echo "       yaws -c file    -- set config file"
	echo "       yaws -r mod     -- call mod:start/0 at startup"
	echo "       yaws -t         -- trace all traffic"
	echo "       yaws -T         -- trace http traffic"
       	echo "       yaws -x         -- trace output to stdout"
	echo "       yaws -v         -- print version"
        echo "       yaws -M dir     -- start Mnesia in dir"
        echo "       yaws -sname xxx -- start with sname xxx"
	echo ""
	echo "       yaws -heart     -- auto restart yaws if it crashes"
	echo "                          (requires the -D switch)"
	echo ""
	echo ""
	echo "ctl functions ... "
	echo "        yaws -h             -- hup the daemon  "
	echo "        yaws -s             -- stop the daemon "
	echo "        yaws -S             -- query the daemon status "
	echo "        yaws -load Modules  -- load modules "
	echo "        yaws -check YawsFile [IncDirs] -- test compile File "
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
xpath="";
mnesia="";

while [ $# -gt 0 ] 
do
      arg=$1
      shift;
      case $arg in
	   -i)
                interactive="true";
		debug=" -yaws debug ";
		daemon="";;
           -w)
                interactive="true";
		debug=" -yaws debug ";
		daemon="";
                erl=$werl;;
           -D)
    	        daemon=" -detached ";;
	   -d)
		debug=" -boot start_sasl -yaws debug ";;
	   -t)
	        trace=" -yaws trace traffic ";;
	   -T)
	        trace=" -yaws trace http ";;
	   -x)
	        traceoutput=" -yaws traceoutput ";;
           -M)
               mnesia=" -mnesia dir '"$1"' -run mnesia start"
               shift;;
           -c)
		conf=" -conf $1 "
		shift;;
          -pa)
	        xpath=" ${xpath} -pa $1 "
                shift;;
           -r)
		runmod=" -runmod $1 "
		shift;;
	   -h)
	        exec $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl hup;
		exit 0;;
	   -s)
	        exec $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl stop;
		exit 0;;
	   -S)
	        exec $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl status;
		exit 0;;
	   -load)
	        exec $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl load $*;
		exit 0;;
	   -v) 
	        exec $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws printversion;
		exit 0;;
           -sname)
		sname=" -sname $1 "
		shift;;
           -heart)
		heart=" -heart ";;
           -check)
                ID=`id -u`
		mkdir /tmp/yaws 2> /dev/null
	        mkdir /tmp/yaws/${ID} 2> /dev/null
                out=`exec $erl -noshell -pa ${yawsdir}${delim}ebin ${xpath} -s yaws_ctl check $*`
		if [ "$out" = "ok" ]; then
                    echo "$1" ok
		    exit 0
		fi
		echo $out
		exit 1;;
	    *)
		help
       esac
done

trace="${trace} ${traceoutput}"

[ -z "$daemon" ] && [ -z "$interactive" ] && help

if [ -z "$heart" ] || [ -z "$daemon" ]; then
    HEART_COMMAND="";
else
    ## ............................this line
    export HEART_COMMAND="${ENV_PGM} HEART=true \"$erl\" ${daemon} ${heart} -pa ${yawsdir}${delim}ebin  ${sname} ${debug} -s yaws $trace $conf $runmod $mnesia";
fi

## keep this line in sync with ....^
exec "$erl" ${daemon} ${heart} -pa ${yawsdir}${delim}ebin  ${sname} ${debug} -s yaws $trace $conf $runmod $mnesia


