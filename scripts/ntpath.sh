#! /bin/sh


for i  in $*
    do
        xxx=`echo  $i  | sed 's/\//\\\\/g'`
	echo -n " "
	echo -n \'
	echo -n $xxx
	echo -n \' " "
    done
