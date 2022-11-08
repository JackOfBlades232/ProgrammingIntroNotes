#!/bin/sh
# compile_in_dir.sh

for FILENAME in ./*.asm; do
    nasm -f elf $FILENAME
done

ld -m elf_i386 *.o -o main.out
