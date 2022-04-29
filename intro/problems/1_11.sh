#!/bin/sh
# 1_11.sh
CNT=$( ls | wc -l )
I=1
while [ $I -le $CNT ]; do
    echo -n '@'
    I=$(( I + 1 ))
done
echo
