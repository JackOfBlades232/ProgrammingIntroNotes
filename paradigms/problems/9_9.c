/* 9_9.c */
#include <stdio.h>
#include <stdlib.h>

struct fib_fsm {
    int n1, n2;
    unsigned int cur_idx;
};

struct fib_fsm *fsm_create()
{
    struct fib_fsm *fsm = malloc(sizeof(*fsm));
    fsm->n1 = 0;
    fsm->n2 = 1;
    fsm->cur_idx = 0;
    return fsm;
}

int fsm_step(struct fib_fsm *fsm)
{
    int res;
    if (fsm->cur_idx < 2)
        res = fsm->cur_idx == 0 ? fsm->n1 : fsm->n2;
    else {
        res = fsm->n1 + fsm->n2;
        fsm->n1 = fsm->n2;
        fsm->n2 = res;
    }

    fsm->cur_idx++;
    return res;
}

void fsm_free(struct fib_fsm *fsm) { free(fsm); }

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1) {
        fprintf(stderr, "Input: <n>\n");
        return 1;
    }

    struct fib_fsm *fsm = fsm_create();

    for (int i = 0; i < n; i++)
        printf("%d ", fsm_step(fsm));
    putchar('\n');

    fsm_free(fsm);

    return 0;
}
