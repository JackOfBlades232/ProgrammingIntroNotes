#!/bin/sh
# 4_22/auto_test.sh

I=1
while [ $I -le $1 ]; do
    test_generation/generate_test.out $I | ./prog.out
    I=$(( I + 1 ))
done
