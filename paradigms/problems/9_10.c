/* 9_10.c */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct intq_item {
    int val;
    struct intq_item *next;
};

struct intq {
    struct intq_item *head, *tail;
};

void intq_enqueue(struct intq *q, int val)
{
    struct intq_item *new = malloc(sizeof(*new));
    new->val = val;
    new->next = NULL;

    if (q->tail)
        q->tail->next = new;
    else
        q->head = new;
    q->tail = new;
}

int intq_dequeue(struct intq *q)
{
    if (!q->head)
        return -1;

    struct intq_item *tmp = q->head;
    q->head = tmp->next;
    if (!q->head)
        q->tail = NULL;

    int val = tmp->val;
    free(tmp);
    return val;
}

void intq_free(struct intq *q)
{
    while (q->head)
        intq_dequeue(q);
    free(q);
}

struct pas_fsm {
    int row, idx_in_row;
    struct intq *cache;
};

struct pas_value {
    int row, idx_in_row, val;
};

struct pas_fsm *fsm_create()
{
    struct pas_fsm *fsm = malloc(sizeof(*fsm));
    fsm->row = 0;
    fsm->idx_in_row = 0;
    fsm->cache = malloc(sizeof(*fsm->cache));
    fsm->cache->head = NULL;
    fsm->cache->tail = NULL;
    return fsm;
}

struct pas_value fsm_step(struct pas_fsm *fsm)
{
    struct pas_value res;
    res.row = fsm->row;
    res.idx_in_row = fsm->idx_in_row;

    if (fsm->idx_in_row == 0)
        res.val = 1; 
    else if (fsm->idx_in_row == fsm->row) {
        res.val = 1;
        intq_dequeue(fsm->cache);
    } else {
        int n1 = intq_dequeue(fsm->cache);
        int n2 = fsm->cache->head->val;
        res.val = n1 + n2;
    }

    if (fsm->idx_in_row == fsm->row) {
        fsm->row++;
        fsm->idx_in_row = 0;
    } else
        fsm->idx_in_row++;

    intq_enqueue(fsm->cache, res.val);
    return res;
}

void fsm_free(struct pas_fsm *fsm) 
{
    if (fsm->cache)
        free(fsm->cache);
    free(fsm); 
}

int main()
{
    int n;
    printf("Input N: ");
    if (scanf("%d", &n) != 1) {
        fprintf(stderr, "Input: <n>\n");
        return 1;
    }

    struct pas_fsm *fsm = fsm_create();

    struct pas_value elem;
    for (int i = 0; i < n; i++) {
        elem = fsm_step(fsm);
        printf("(%d, %d, %d) ", elem.row, elem.idx_in_row, elem.val);
    }
    putchar('\n');

    fsm_free(fsm);

    return 0;
}
