#include <stdio.h>
#include <stdlib.h>

struct node {
    int val;
    struct node *left, *right;
};

void int_bin_tree_add(struct node **root, int n)
{
    if(!*root) {
        *root = (struct node*) malloc(sizeof(**root));
        (*root)->val = n;
        (*root)->left = NULL;
        (*root)->right = NULL;
        return;
    }
    if((*root)->val == n)
        return;
    if(n < (*root)->val)
        int_bin_tree_add(&(*root)->left, n);
    else
        int_bin_tree_add(&(*root)->right, n);
}

void int_bin_tree_print_rec(struct node *r)
{
    if(!r)
        return;
    int_bin_tree_print_rec(r->left);
    printf("%d ", r->val);
    int_bin_tree_print_rec(r->right);
}

void int_bin_tree_print_loop(struct node *r)
{
    enum state { start, left_visited, completed };
    struct backpath {
        struct node *p;
        enum state st;
        struct backpath *next;
    };
    struct backpath *bp, *t;
    bp = malloc(sizeof(*bp));
    bp->p = r;
    bp->st = start;
    bp->next = NULL;
    while(bp) {
        switch(bp->st) {
        case start:
            bp->st = left_visited;
            if(bp->p->left) {
                t = malloc(sizeof(*t));
                t->p = bp->p->left;
                t->st = start;
                t->next = bp;
                bp = t;
                continue;
            }
            /* no break here */
        case left_visited:
            printf("%d ", bp->p->val);
            bp->st = completed;
            if(bp->p->right) {
                t = malloc(sizeof(*t));
                t->p = bp->p->right;
                t->st = start;
                t->next = bp;
                bp = t;
                continue;
            }
            /* no break here */
        case completed:
            t = bp;
            bp = bp->next;
            free(t);
        }
    }
}

void int_bin_tree_dispose(struct node *r)
{
    if(!r)
        return;
    int_bin_tree_dispose(r->left);
    int_bin_tree_dispose(r->right);
    free(r);
}


int main()
{
    static const int vals[] = {
        50, 25, 75, 15, 30, 60, 90, 10, 20, 40,
        60, 70, 80, 95,  5, 35, 45, 55, 65, 85
    };
    struct node *root = NULL;
    int i;

    for(i = 0; i < sizeof(vals) / sizeof(*vals); i++)
        int_bin_tree_add(&root, vals[i]);

    int_bin_tree_print_rec(root); printf("\n");
    int_bin_tree_print_loop(root); printf("\n");

    int_bin_tree_dispose(root);

    return 0;
}
