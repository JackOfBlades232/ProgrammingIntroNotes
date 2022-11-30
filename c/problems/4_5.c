/* 4_5.c */
#include <stdio.h>

int get_and_zero(int *xp)
{
    int x;
    x = *xp;
    *xp = 0;
    return x;
}

int main() 
{
    int x, out;

    scanf("%d", &x);
    out = get_and_zero(&x);
    printf("Output: %d, new value: %d\n", out, x);

    return 0;
}
