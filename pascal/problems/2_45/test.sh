#!/bin/sh
I=1
while [ $I -le $1 ]; do
    ./generate_test > test_case.txt
    ./unitdemo < test_case.txt
    I=$(( I + 1 ))
done
rm test_case.txt
