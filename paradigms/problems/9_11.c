/* 9_10.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct perm_fsm {
    int n;
    int *last_perm;
    int longest_inv_tail;
};

struct perm_fsm *fsm_create(int n)
{
    struct perm_fsm *fsm = malloc(sizeof(*fsm));
    fsm->n = n;
    fsm->last_perm = malloc(n * sizeof(*fsm->last_perm));
    for (int i = 0; i < n; i++)
        fsm->last_perm[i] = i+1;
    fsm->longest_inv_tail = 1;
    return fsm;
}

/* won't bother with out buf safety now */
void fsm_step(struct perm_fsm *fsm, int *out)
{
    memcpy(out, fsm->last_perm, fsm->n * sizeof(int));

    /* TODO: step the permutation */
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

    for (int i = 0; i < n; i++) {
        fsm_step(fsm, outbuf);

        putchar('{');
        for (int j = 0; j < n-1; j++)
            printf("%d, ", outbuf[j]);
        printf("%d}\n", outbuf[n-1]);
    }

    fsm_free(fsm);

    return 0;
}
