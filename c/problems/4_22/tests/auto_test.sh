#!/bin/sh

I=1
while [ $I -le $1 ]; do
    ../tests/prog.out | ../prog.out
    I=$(( I + 1 ))
done
