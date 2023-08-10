/* paradigms/bin_tree_revisited.c */
#include <stdio.h>
#include <stdlib.h>

struct node {
    int val;
    struct node *left, *right;
};

void int_bin_tree_print_rec(const struct node *r)
{
    if (!r)
        return;
    int_bin_tree_print_rec(r->left);
    printf("%d ", r->val);
    int_bin_tree_print_rec(r->right);
}

void int_bin_tree_print_loop(const struct node *r)
{
    enum state { start, left_visited, completed };
    struct backpath {
        const struct node *p;
        enum state st;
        struct backpath *next;
    };

    struct backpath *bp, *t;
    bp = malloc(sizeof(*bp));
    bp->p = r;
    bp->st = start;
    bp->next = NULL;

    /* DFS traversal */
    while (bp) {
        switch (bp->st) {
            case start:
                bp->st = left_visited;
                if (bp->p->left) {
                    t = malloc(sizeof(*t));
                    t->p = bp->p->left;
                    t->st = start;
                    t->next = bp;
                    bp = t;
                    continue;
                }
            /* no break (if -> left visited) */
            case left_visited:
                printf("%d ", bp->p->val);
                bp->st = completed;
                if (bp->p->right) {
                    t = malloc(sizeof(*t));
                    t->p = bp->p->right;
                    t->st = start;
                    t->next = bp;
                    bp = t;
                    continue;
                }
            /* no break (if -> completed) */
            case completed:
                t = bp;
                bp = bp->next;
                free(t);
        }
    }
}

int int_bin_tree_sum(const struct node *r)
{
    return r ? int_bin_tree_sum(r->left) + r->val + int_bin_tree_sum(r->right) : 0;
}

int main()
{
    struct node *r = malloc(sizeof(*r));
    r->left = malloc(sizeof(*r->left));
    r->right = malloc(sizeof(*r->right));
    r->left->left = malloc(sizeof(*r->left->left));
    r->left->right = malloc(sizeof(*r->left->right));
    r->right->left = NULL;
    r->right->right = NULL;
    r->left->left->left = NULL;
    r->left->left->right = NULL;
    r->left->right->left = NULL;
    r->left->right->right = NULL;

    r->val = 3;
    r->left->val = 1;
    r->right->val = 28;
    r->left->left->val = -1;
    r->left->right->val = 2;

    printf("Rec: ");
    int_bin_tree_print_rec(r);
    printf("\nLoop: ");
    int_bin_tree_print_loop(r);
    printf("\nSum: %d\n", int_bin_tree_sum(r));

    return 0;
}
