/* 9_3.c */
#include <stdio.h>
#include <limits.h>

#define MAX(_a, _b) ((_a) > (_b) ? (_a) : (_b))

/* Also tail recursion */
void intvec2max(const int *arr, int len, int res[2])
{
    if (*arr > res[0]) {
        res[1] = res[0];
        res[0] = *arr;
    } else if (*arr > res[1])
        res[1] = *arr;
    if (len > 1) 
        intvec2max(arr+1, len-1, res);
}

int main()
{
    int arr[128];
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

    int res[2];
    intvec2max(arr, len, res);
    printf("Max: %d, second: %d\n", res[0], res[1]);
    return 0;
}
