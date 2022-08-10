#/bin/sh
# tests for 3_21

while read a b c d ; do
    set -f
    res=`echo -e "$a\n$b\n$c\n" | ./3_21`
    if [ x"$d" != x"$res" ]; then
        echo TEST $a $b $c FAILED: expected "$d", got "$res"
    fi
    set +f
done <<END
    4 + 12 16
    4 + -3 1
    4 + -6 -2
    12 - 3 9
    12 - 17 -5
    12 - -34 46
    5 * 4 20
    5 * -6 -30
    -5 * 7 -35
    -6 * -6 36
    123 / 10 12
    123 / -10 -12
    -123 / -10 12
    123 / -10 -12
    1 - 1 0
    2 + -2 0
    0 / 4 0
    5 * 0 0
END
