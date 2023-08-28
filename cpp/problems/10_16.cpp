/* 10_16.cpp */
#include <stdio.h>

template <class T>
T get_and_zero(T &b)
{
    T val = b;
    b = 0;
    return val;
}

int main()
{
    int a = 3, b = 2, c = 1;
    printf("a = %d, b = %d, c = %d\n", a, b, c);
    b = get_and_zero(c);
    printf("(after b = get_and_zero(c)): a = %d, b = %d, c = %d\n", a, b, c);
    a = get_and_zero(b);
    printf("(after a = get_and_zero(b)): a = %d, b = %d, c = %d\n", a, b, c);
    return 0;
}
