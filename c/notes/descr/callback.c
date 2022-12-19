/* callback.c */
#include <stdio.h>
#include <stdlib.h>

struct node {
    int val;
    struct node *left, *right;
};

/* common usage of fptr, for passing a user func to a serves, for ex */
void int_callback_print(int data, void *userdata)
{
    printf("%d ", data);
}

void int_callback_sum(int data, void *userdata)
{
    *(int*)userdata += data;
}

struct minmaxcount {
    int count, min, max;
};

void int_callback_minmaxcount(int data, void *userdata)
{
    struct minmaxcount *mmc = userdata;

    if (mmc->count == 0) {
        mmc->min = mmc->max = data;
    } else {
        if (mmc->min > data)
            mmc->min = data;
        if (mmc->max < data)
            mmc->max = data;
    }
    mmc->count++;
}

void int_bin_tree_traverse(
    struct node *r,
    void (*callback)(int, void*),
    void *userdata
)
{
    if (!r)
        return;

    int_bin_tree_traverse(r->left, callback, userdata);
    (*callback)(r->val, userdata);
    int_bin_tree_traverse(r->right, callback, userdata);
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

void int_bin_tree_destroy(struct node *r)
{
    if (!r)
        return;

    int_bin_tree_destroy(r->left);
    int_bin_tree_destroy(r->right);
    free(r);
}

int main()
{
    struct node *r;
    int sum;
    struct minmaxcount mmc;

    int_bin_tree_add(&r, 3);
    int_bin_tree_add(&r, 2);
    int_bin_tree_add(&r, 4);
    int_bin_tree_add(&r, 1);

    int_bin_tree_traverse(r, &int_callback_print, NULL);
    putchar('\n');

    sum = 0;
    int_bin_tree_traverse(r, &int_callback_sum, &sum);
    printf("sum: %d\n", sum);

    mmc.count = 0;
    int_bin_tree_traverse(r, &int_callback_minmaxcount, &mmc);

    int_bin_tree_destroy(r);

    return 0;
}
