#!/bin/sh


I=1;
while [ $I -lt 10 ]; do
    ./hanoi $I > _HAN.$I.txt
    ./a.out $I > _HAN2.$I.txt
    diff _HAN.$I.txt _HAN2.$I.txt || {
        echo FAILED for $I
        exit 1
    }
    I=$((I+1))
done
