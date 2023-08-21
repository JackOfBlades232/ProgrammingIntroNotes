/* 9_8.c */
#include <stdio.h>

void print_fib_do(int n1, int n2, int cnt, int cap)
{
    if (cnt >= cap)
        return;
    printf("%d ", n1 + n2);
    print_fib_do(n2, n1 + n2, cnt+1, cap);
}

void print_fib(int n)
{
    if (n > 0)
        printf("%d ", 0);
    if (n > 1)
        printf("%d ", 1);
    print_fib_do(0, 1, 2, n);
    putchar('\n');
}

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1) {
        fprintf(stderr, "Input: <n>\n");
        return 1;
    }

    print_fib(n);
    return 0;
}
