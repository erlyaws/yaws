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
        echo "usage:  "
        echo ""
        echo 
        echo "       yaws -i         -- interactive (no daemon) mode"
        echo "       yaws -w         -- cygwin interactive (werl) "
        echo "       yaws -D         -- daemon mode"

        echo ""

        echo ""
        echo "       Auxilliary flags for the daemon: "
        echo "            -I Id      --  Set system id"
        echo "            -d         --  debug mode "
        echo "            -c File    --  set config file"
        echo "            -r mod     --  call mod:start/0 at startup"
        echo "            -t         --  trace all traffic"
        echo "            -T         --  trace http traffic"
        echo "            -x         --  trace output to stdout"
        echo "            -v         --  print version"
        echo "            -pa path   --  add path"
        echo "            -M dir     --  start Mnesia in dir"
        echo "            -sname xxx --  start with sname xxx"
        echo "            -r mod     --  call mod:start/0 at startup"
        echo "            -heart     --  auto restart yaws if it crashes"

        echo ""

        echo "ctl functions ... "
        echo "        yaws -h [-I id]       -- hup the daemon  "
        echo "        yaws -s [-I id]       -- stop the daemon "
        echo "        yaws -S [-I id]       -- query the daemon status "
        echo "        yaws -load Modules    -- load modules "
	echo "        yaws -j traffic|http  -- toggle trace of running daemon"
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
id="default";

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
	   -I)
                id=$1
		shift;;
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
	        ex="$erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl hup";;
	   -s)
	        ex="$erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl stop";;
	   -S)
	        ex="$erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl status";;
	   -load)
	        $erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl load ${id} $*
		exit 0;;
	   -j)
	        ex="$erl -noshell -pa ${yawsdir}${delim}ebin -s yaws_ctl trace $1"
		shift;;
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


if [ ! -z "${ex}" ]; then
    exec ${ex} ${id}
    exit 0
fi


trace="${trace} ${traceoutput}"

[ -z "$daemon" ] && [ -z "$interactive" ] && help

if [ -z "$heart" ] || [ -z "$daemon" ]; then
    HEART_COMMAND="";
else
    ## ............................this line
    export HEART_COMMAND="${ENV_PGM} HEART=true \"$erl\" ${daemon} ${heart} -pa ${yawsdir}${delim}ebin ${xpath} ${sname} ${debug} -s yaws $trace $conf $runmod $mnesia";
fi

## keep this line in sync with ....^
exec "$erl" ${daemon} ${heart} -pa ${yawsdir}${delim}ebin  ${xpath} ${sname} ${debug} -s yaws $trace $conf $runmod $mnesia


