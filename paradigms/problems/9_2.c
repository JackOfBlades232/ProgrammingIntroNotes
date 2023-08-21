/* 9_2.c */
#include <stdio.h>
#include <limits.h>

#define MAX(_a, _b) ((_a) > (_b) ? (_a) : (_b))

/*
 * Here, we use an accumulator to enable tail recursion, thus a smart 
 * compiler may optimize it. However, this can no longer be neatly described
 * using the recursion paradigm.
 */

int intvecmax_acc(const int *arr, int len, int max)
{
    return len > 0 ? intvecmax_acc(arr+1, len-1, MAX(max, *arr)) : max;
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

    printf("Max: %d\n", intvecmax_acc(arr, len, INT_MIN));
    return 0;
}
