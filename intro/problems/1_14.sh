#!/bin/sh
# 1_14.sh

# UNFINISHED

mkdir $1
if [ $? != 0 ]; then
    exit $?
fi
DIRNAME=$( realpath $1 )
I=1
for FILENAME in ./*; do
    FULLNAME=$( realpath $FILENAME )
    if [ $FULLNAME != $DIRNAME ]; then
        ln -s $FULLNAME "$DIRNAME/filename$I"
        I=$(( I + 1 ))
    fi
done
