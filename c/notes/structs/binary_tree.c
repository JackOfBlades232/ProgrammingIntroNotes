/* binary_tree.c */
#include <stdio.h>
#include <stdlib.h>

struct node {
    int val;
    struct node *left, *right;
};

void int_bin_tree_print_rec(struct node *r)
{
    if (!r)
        return;

    int_bin_tree_print_rec(r->left);
    printf("%d ", r->val);
    int_bin_tree_print_rec(r->right);
}

void int_bin_tree_add(struct node **root, int n)
{
    if (!*root) {
        *root = malloc(sizeof(**root));
        (*root)->val = n;
        (*root)->left = NULL;
        (*root)->right = NULL;
        return;
    }
    
    if ((*root)->val == n)
        return;

    if (n < (*root)->val)
        int_bin_tree_add(&(*root)->left, n);
    else
        int_bin_tree_add(&(*root)-> right, n);
}

int main()
{
    struct node *r;

    int_bin_tree_add(&r, 3);
    int_bin_tree_add(&r, 2);
    int_bin_tree_add(&r, 4);
    int_bin_tree_add(&r, 1);

    int_bin_tree_print_rec(r);

    return 0;
}
