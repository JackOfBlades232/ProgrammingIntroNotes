/* 10_15.cpp */
#include <stdio.h>

template <class T>
void swap3(T &x, T &y, T &z)
{
    T tmp;
    tmp = x;
    x = y;
    y = z;
    z = tmp;
}

int main()
{
    int x, y, z;
    printf("Input <x y z>: ");
    if (scanf("%d %d %d", &x, &y, &z) != 3) {
        fprintf(stderr, "Args: <x y z>, all integers\n");
        return 1;
    }

    swap3(x, y, z);
    printf("(one swap): %d %d %d\n", x, y, z);
    swap3(x, y, z);
    printf("(two swaps): %d %d %d\n", x, y, z);
    swap3(x, y, z);
    printf("(three swaps): %d %d %d\n", x, y, z);

    return 0;
}
