/* 9_5.c */
#include <stdio.h>
#include <limits.h>

#define MAX(_a, _b) ((_a) > (_b) ? (_a) : (_b))

/*
 * This solution is basically the same, with the fact that there are no
 * assignments except the zeroeing out highlighted
 */

unsigned int uintvecmax(const unsigned int *arr, int len)
{
    return len > 1 ? MAX(*arr, uintvecmax(arr+1, len-1)) : *arr;
}

void uintvec_clearval(unsigned int *arr, int len, int val)
{
    if (len <= 0)
        return;
    if (*arr == val)
        *arr = 0;
    uintvec_clearval(arr+1, len-1, val);
}

void uintvec_clearmax(unsigned int *arr, int len)
{
    uintvec_clearval(arr, len, uintvecmax(arr, len));
}

int main()
{
    unsigned int arr[128];
    int len;

    printf("Input arr len: ");
    if (scanf("%d", &len) != 1 || len < 2) {
        fprintf(stderr, "Invalid arr len\n");
        return 1;
    }

    for (int i = 0; i < len; i++) {
        if (scanf("%d", &arr[i]) != 1) {
            fprintf(stderr, "Invalid arr elem\n");
            return 1;
        }
    }

    uintvec_clearmax(arr, len);

    printf("Cleared array: ");
    for (int i = 0; i < len; i++)
        printf(" %d", arr[i]);
    putchar('\n');

    return 0;
}
