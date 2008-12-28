#!/bin/sh

. ../support/include.sh


i=0
while :; do
    sleep 1
    ${YTOP}/bin/yaws --id testid --status 2>&1 > /dev/null
    if [ $? = 0 ]; then
        echo "yaws started "
        exit 0
    fi
    i=$(($i + 1))
    if [ $i = 10 ]; then
        echo "No status reply - yaws is not started"
        exit 1
    fi
done
