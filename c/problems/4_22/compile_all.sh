#!/bin/sh
# 4_22/compile_all.sh

../../compile.sh
gcc -Wall -g test_generation/generate_test.c -o test_generation/generate_test.out
