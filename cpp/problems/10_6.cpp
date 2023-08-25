/* 10_6.cpp */
#include <stdio.h>

class E {
    class ER {
        int x;

    public:
        ER(int a_x) : x(a_x) {}
        int operator[](int y) { return x-y; }
    };

public:
    ER operator[](int x) { return ER(x); }
};

int main()
{
    E e;
    printf("%d %d %d\n", e[0][0],  e[100][100], e[-10][-10]);
    printf("%d %d %d\n", e[1500][7],  e[7][55], e[-8][-16]);
    return 0;
}

