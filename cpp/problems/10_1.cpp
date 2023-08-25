/* 10_1.cpp */
#include <stdio.h>

class A {
    int n;

public:
    A(int a_n) : n(a_n) {}
    int operator[](int idx) const { return n+idx; }
};

int main()
{
    A first = 1;
    A second(10);
    printf("first: %d %d\n", first[100], first[200]);
    printf("second: %d %d\n", second[100], second[200]);
    return 0;
}
