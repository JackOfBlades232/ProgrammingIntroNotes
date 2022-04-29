#!/bin/sh
# 1_10.sh
START=$1
I=0
while [ $I -lt $2 ]; do
    echo -n $(( START + I ))
    echo -n " "
    I=$(( I + 1 ))
done
echo
