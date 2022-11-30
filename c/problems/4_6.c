/* 4_6.c */
#include <stdio.h>

int put_sum(int *x, int *y, int *z)
{
    *x += *y + *z;
    *z = *y = *x;
    return *x;
}

int main()
{
    int x, y, z, sum;

    scanf("%d %d %d", &x, &y, &z);
    sum = put_sum(&x, &y, &z);
    printf("Sum: %d, x: %d, y: %d, z: %d\n", sum, x, y, z);

    return 0;
}
