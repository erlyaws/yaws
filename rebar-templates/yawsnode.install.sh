#!/bin/sh
set -e
wd=$PWD
for file in releases/*/sys.config etc/yaws/yaws.conf; do
    mv $file ${file}.bak
    sed -e "s:%YAWSNODE%:$wd:" ${file}.bak > $file
    rm ${file}.bak
done
exit 0
