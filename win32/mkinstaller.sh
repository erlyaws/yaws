#!/bin/sh

. ../vsn.mk

set -x

# If this script  runs on kisel@hyber.org we
# have the installbuilder as below. 
# Developers can install installbuilder anywhere and invoke
# as e.g
# INSTALL_BUILDER=/home/luser/installbuilder-5.4.10 ./mkinstaller.sh 


INSTALL_BUILDER=${INSTALL_BUILDER:=${HOME}/installbuilder-5.4.14}

test -d "$INSTALL_BUILDER" || {
	echo "Error: Could not find directory $INSTALL_BUILDER."
	echo "Fatal: $0 requires installbuilder."
	exit 13
}

(cd ../src; make clean)
echo "" > ../src/yaws_configure.hrl
(cd ../doc; make docs)
(cd ../src; make)



DIRS0="ebin examples applications ssl src www include"
DIRS1="doc priv bin logs scripts"

rm -rf  ${DIRS0} ${DIRS1}

for d in `echo ${DIRS0}`; do
    cp -a ../${d} .
done

for d in `echo ${DIRS1}`; do
    mkdir $d
done

(cd ../man; make pdf)

cp ../scripts/Subst scripts
cp yaws.exe bin
cp ../man/*.pdf doc
cp ../doc/*.pdf doc
cp ../priv/*.xsd priv
cp ../priv/mime.types priv
cp ../LICENSE LICENSE.txt
cp ../vsn.mk .


for d in `echo ${DIRS0} ${DIRS1}`; do
    find $d -name '*.svn' | xargs rm -rf
done




# Convert readable files to DOS style:
TODOS=""
if [ -x /usr/bin/unix2dos ]; then
    TODOS=/usr/bin/unix2dos
fi
if [ -x /usr/bin/todos ]; then
    TODOS=/usr/bin/todos
fi
if [ "$TODOS" = "" ]; then
    echo "No unix2dos/todos program found";
    echo "Run aptitude install tofrodos";
    exit 1
fi

${TODOS} LICENCE.txt
for d in `echo ${DIRS0} ${DIRS1}`; do
    for suffix in txt erl conf html yaws js pem; do
        find . -name "*.$suffix" -exec ${TODOS} {} \;
    done
done

vsn=`echo ${YAWS_VSN} | sed 's/.*=//'`
cat build.xml.in | sed -e "s/@vsn@/${vsn}/g"  > build.xml





## Need to have builder in the path
${INSTALL_BUILDER}/bin/builder build build.xml windows > /dev/null
mv ${INSTALL_BUILDER}/output/*.exe .
echo .exe files copied to `pwd`

