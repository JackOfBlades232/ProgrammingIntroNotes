#/bin/sh
# tests for 3_27

while read expr expected ; do
    set -f
    res=`echo -e "$expr" | ./main`
    if [ x"$expected" != x"$res" ]; then
        echo TEST $expr FAILED: expected "$expected", got "$res"
    fi
    set +f
done <<END
    1+1+1 3
    500-100-250 150
    1-2+3-4+5+123 126
    2*3*12 72
    14/3/2 2
    1232232*0 0
    4*8-3*3*3/2 19
    (9*9-41*1)/8 5
    ((13+22*2)/4-(32-11)*(1-(12*43-300))+3)-(34-32)/2 4531
    (13-(12+3*3)+3/2-4*4)*(123-32*2+3*(4-2*2))*(432-108*4-1) 1357 
    ((1+1) ERROR
    -1-1 ERROR
    (1+1)-(1-1)) ERROR
    2/0 ERROR
END
