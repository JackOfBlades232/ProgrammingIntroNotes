#!/bin/sh
echo "good"
MESSAGE="success"
echo $MESSAGE
I=10
J=$(( $I + 10 ))
echo ${I}abc ${J}def
echo "" "$3 $2 $1"
if [ -f "file.txt" ]; then
	cat "file.txt"
else
	echo "File not found"
fi
K=1
while [ $K -le 100 ]; do
	echo $K
	K=$(( $K + 1 ))
done
for C in red orange yellow; do
	echo $C
done
ls || ls -l
ps ax && ls
