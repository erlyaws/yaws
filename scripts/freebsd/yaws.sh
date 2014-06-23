#! /bin/sh
#
# Startup script for Yaws. Use this scripts for FreeBSD versions prior to 9.


YAWS_BIN=%prefix%/bin/yaws
# By default we run with the default id
# YAWS_ID=myserverid
CONF=%etcdir%/yaws/yaws.conf


if [ "x${YAWS_ID}" = x ]; then
    idarg=""
else
    idarg="--id ${YAWS_ID}"
fi

start() {
    echo -n "Starting Yaws: "
    $yaws ${idarg} --daemon --heart --conf ${conf} > /dev/null
    $yaws  ${idarg} --wait-started=10 > /dev/null
    RETVAL=$?
    if [ $RETVAL = 0 ]; then
        echo "OK"
    else
        echo "FAILED"
    fi
    return  $RETVAL
}

stop() {
    echo -n "Stopping Yaws: "
    $yaws ${idarg} --stop > /dev/null
    $yaws  ${idarg} --wait-stopped=10 > /dev/null
    RETVAL=$?
    if [ $RETVAL = 0 ]; then
        echo "OK"
    else
        echo "FAILED"
    fi
    return  $RETVAL
}

reload() {
    echo -n "Reloading Yaws: "
    $yaws ${idarg} --hup > /dev/null
    RETVAL=$?
    if [ $RETVAL = 0 ]; then
        echo "OK"
    else
        echo "FAILED"
    fi
    return  $RETVAL
}


status() {
   $yaws ${idarg} --status > /dev/null
    RETVAL=$?
    if [ $RETVAL = 0 ]; then
        echo "Yaws is running"
    else
        echo "Yaws is stopped"
    fi
    return  $RETVAL
}

