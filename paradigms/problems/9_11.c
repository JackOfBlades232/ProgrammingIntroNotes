/* 9_10.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void swap_int(int *a, int *b)
{
    int tmp = *a;
    *a = *b;
    *b = tmp;
}

int factorial(int n)
{
    int res = 1;
    for (int i = 2; i <= n; i++) {
        res *= i;
        if (res < 0)
            return -1;
    }
    return res;
}

struct perm_fsm {
    int n;
    int *last_perm;
    int longest_inv_tail;
    int done;
};

struct perm_fsm *fsm_create(int n)
{
    struct perm_fsm *fsm = malloc(sizeof(*fsm));
    fsm->n = n;
    fsm->last_perm = malloc(n * sizeof(*fsm->last_perm));
    for (int i = 0; i < n; i++)
        fsm->last_perm[i] = i+1;
    fsm->longest_inv_tail = 1;
    fsm->done = 0;
    return fsm;
}

/* won't bother with out buf safety now */
void fsm_step(struct perm_fsm *fsm, int *out)
{
    memcpy(out, fsm->last_perm, fsm->n * sizeof(*fsm->last_perm));

    /* If already generated all permutations, just clear the outbuf */
    if (fsm->done)
        return;
    else if (!fsm->done && fsm->longest_inv_tail >= fsm->n) {
        memset(fsm->last_perm, 0, fsm->n * sizeof(*fsm->last_perm));
        fsm->done = 1;
        return;
    }

    /* find the first element larger than pivot in tail
     * (the tail is the longest motonously decreasing suffix) */
    int pivot_idx = fsm->n - fsm->longest_inv_tail - 1;
    int next_idx;
    for (next_idx = fsm->n - 1; next_idx > pivot_idx; next_idx--) {
        if (fsm->last_perm[next_idx] > fsm->last_perm[pivot_idx])
            break;
    }

    /* swap it with the pivot */
    swap_int(&fsm->last_perm[pivot_idx], &fsm->last_perm[next_idx]);

    /* reverse the tail */
    for (int i = 0; i < fsm->longest_inv_tail / 2; i++)
        swap_int(&fsm->last_perm[pivot_idx+i+1], &fsm->last_perm[fsm->n-i-1]);

    /* reset the tail len: if tail was longer that 1 elem, after the reverse
     * it becomes 1 elem long, otherwise we have to recalculate it's len */
    if (fsm->longest_inv_tail > 1)
        fsm->longest_inv_tail = 1;
    else {
        for (int i = fsm->n-1; i >= 1; i--) {
            if (fsm->last_perm[i-1] <= fsm->last_perm[i])
                break;
            fsm->longest_inv_tail++;
        }
    }

}

void fsm_free(struct perm_fsm *fsm) 
{
    free(fsm->last_perm);
    free(fsm); 
}

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1 || n <= 0) {
        fprintf(stderr, "Input: <n>, n >= 1\n");
        return 1;
    }

    struct perm_fsm *fsm = fsm_create(n);
    int *outbuf = malloc(n * sizeof(*outbuf));

    int nfac = factorial(n);
    if (nfac == -1) {
        fprintf(stderr, "N is too large!\n");
        return 1;
    }

    for (int i = 0; i < nfac; i++) {
        fsm_step(fsm, outbuf);

        putchar('{');
        for (int j = 0; j < n-1; j++)
            printf("%d, ", outbuf[j]);
        printf("%d}\n", outbuf[n-1]);
    }

    fsm_free(fsm);

    return 0;
}
