#!/bin/sh
# 5_7/gen_rand_list.sh

I=0
while [ $I -lt $3 ]; do
    $1 $2 add $(echo $(( RANDOM + I )) | md5sum | head -c 20)
    I=$(( I + 1 ))
done
