/* 9_1.c */
#include <stdio.h>

#define MAX(_a, _b) ((_a) > (_b) ? (_a) : (_b))

/*
 * recursion basis: len == 1, then max == the only elem
 * the main case: when len > 1, max is max of first elem and max of the rest
 */

int intvecmax(const int *arr, int len)
{
    return len > 1 ? MAX(*arr, intvecmax(arr+1, len-1)) : *arr;
}

int main()
{
    int arr[128];
    int len;

    printf("Input arr len: ");
    if (scanf("%d", &len) != 1 || len < 1) {
        fprintf(stderr, "Invalid arr len\n");
        return 1;
    }

    for (int i = 0; i < len; i++) {
        if (scanf("%d", &arr[i]) != 1) {
            fprintf(stderr, "Invalid arr elem\n");
            return 1;
        }
    }

    printf("Max: %d\n", intvecmax(arr, len));
    return 0;
}
