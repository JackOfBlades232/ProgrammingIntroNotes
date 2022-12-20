#!/bin/sh
# CPURenderer/compile.sh

for FILENAME in ./*.c; do
    gcc -Wall -g -c $FILENAME
done

gcc *.o -lpng -o prog.out
