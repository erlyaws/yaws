#!/bin/bash --

progname=$0

function usage () {
    cat <<EOF
Usage: $progname [directories]
EOF
    exit 0
}

function updateDir () {
    if test -d $udir ; then
	suffix=`date -I`
	echo "updating " $udir
	install -C -b -S $suffix wiki/*.yaws $udir
	install -C -b -S $suffix wiki/*.wob $udir
	if test '!' -d $udir/WikiPreferences.files ; then
	    mkdir $udir/WikiPreferences.files
	fi
	install -C -b -S $suffix wiki/WikiPreferences.files/* $udir/WikiPreferences.files
    else
	echo "Error: " $udir " is not a directory"
	usage
	exit 1
    fi
}

if test $# -lt 1 ; then
    usage
    exit 1
fi

while test $# -gt 0 ; do
    udir=$1
    shift 1
    updateDir
done
