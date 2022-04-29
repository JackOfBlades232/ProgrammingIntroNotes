#!/bin/sh

OK=ok

while read a b c d ; do
    res=`echo $a $b | ./frcancel`
    if [ x"$c $d" != x"$res" ]; then
        echo TEST $a $b FAILED: expected "$c $d", got "$res"
        OK=failed
    fi
done <<EOF
   25 15 5 3
   7 12 7 12
   100 2000 1 20
EOF

if [ $OK = "ok" ]; then
   echo "Everything seems okay"
   exit 0
else
   echo "Some tests failed"
   exit 1
fi

