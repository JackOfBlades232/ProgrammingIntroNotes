#!/bin/sh
# 1_14.sh

# UNFINISHED

mkdir $1
if [ $? != 0 ]; then
    exit $?
fi
I=1
for FILENAME in ./*; do
    if [ $FILENAME != $1 ]; then
        ln -s -t$1 $FILENAME "filename$I${FILENAME##*.}"
        I=$(( I + 1 ))
    fi
done
