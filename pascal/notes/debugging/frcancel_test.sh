#!/bin/sh
# frcancel_test.sh

while read a b c d ; do
    res=$( echo $a $b | ./frcancel )
    if [ "$c $d" != "$res" ]; then
        echo TEST $a $b FAILED: expected $c $d, got $res
    fi
done <<END
    25 15 5 3
    7 12 7 12
    100 2000 1 20
END
