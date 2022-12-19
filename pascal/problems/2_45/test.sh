# /bin/sh

fpc -dTEST rbtree.pp > /dev/null
fpc -dTEST unitdemo.pas > /dev/null
fpc -dTEST generate_test > /dev/null

I=1
while [ $I -le $1 ]; do
    ./generate_test > input.txt
    ./unitdemo < input.txt
    I=$(( I + 1 ))
done

rm input.txt
