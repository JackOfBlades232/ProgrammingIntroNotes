#!/bin/sh
# 5_7/gen_rand_list.sh

GENERATOR=$1
LIST_NM=$2
SIZE=$3

I=0
while [ $I -lt $SIZE ]; do
    $GENERATOR $TABLE add $(echo $RANDOM | md5sum | head -c 20)
done
