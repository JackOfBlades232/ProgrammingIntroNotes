/* 4_22/rbtree.c */
#include "rbtree.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

const tree_node *rbtree_get_element(const tree_node *root, const char *key) {
    int comp_res;

    if (!root)
        return NULL;

    comp_res = strcmp(root->key, key);

    if (comp_res == 0)
        return root;
    else
        return rbtree_get_element(comp_res > 0 ? root->left : root->right, key);
}

static tree_node *add_element_simple(tree_node **root, tree_node *parent,
        const char *key, void *data);
static void insert_case_1(tree_node *n, tree_node **root);
static void insert_case_2(tree_node *n, tree_node **root);
static void insert_case_3(tree_node *n, tree_node **root);
static void insert_case_4(tree_node *n, tree_node **root);

int rbtree_add_element(tree_node **root, const char *key, void *data)
{
    tree_node *n;

    n = add_element_simple(root, NULL, key, data);

    if (!*root)
        *root = n;
    if (n)
        insert_case_1(n, root);

    return n != NULL;
}

void rbtree_print(const tree_node *root)
{
    if (!root)
        return;

    rbtree_print(root->left);
    printf("%s\n", root->key);
    rbtree_print(root->right);
}

static void destroy_node(tree_node* n);

void rbtree_destroy(tree_node *root)
{
    if (!root)
        return;

    rbtree_destroy(root->left);
    rbtree_destroy(root->right);
    destroy_node(root);
}

static int is_black(tree_node *n) 
{ 
    return !n || n->color == black; 
}

static int is_red(tree_node *n) 
{ 
    return !is_black(n);
}

static tree_node *create_node(tree_node *parent, const char *key, void *data)
{
    tree_node *n;

    n = malloc(sizeof(*n));
    n->key = malloc(strlen(key) * sizeof(char));
    strcpy(n->key, key);
    n->data = data;
    n->left = n->right = NULL;
    n->parent = parent;

    return n;
}

static void destroy_node(tree_node* n)
{
    if (n) {
        free(n->key);
        free(n);
    }
}

static tree_node *add_element_simple(tree_node **root, tree_node *parent,
        const char *key, void *data)
{
    int comp_res;

    if (!(*root)) {
        *root = create_node(parent, key, data);
        return *root;
    }

    comp_res = strcmp((*root)->key, key);

    if (comp_res == 0)
        return NULL;

    if (comp_res > 0)
        return add_element_simple(&((*root)->left), *root, key, data);
    else
        return add_element_simple(&((*root)->right), *root, key, data);
}

static tree_node *grandparent(const tree_node *n)
{
    return (n && n->parent) ? n->parent->parent : NULL;
}

static tree_node *uncle(const tree_node *n)
{
    const tree_node *gp;
    gp = grandparent(n);

    if (gp)
        return n->parent == gp->left ? gp->right : gp->left;
    else
        return NULL;
}

static void rotate_left(tree_node *n, tree_node **root)
{
    tree_node *pivot;

    if (!n || !n->right)
        return;

    pivot = n->right;
    pivot->parent = n->parent;

    if (n->parent) {
        if (n == n->parent->left)
            n->parent->left = pivot;
        else
            n->parent->right = pivot;
    } else 
        *root = pivot;

    n->right = pivot->left;
    if (pivot->left)
        pivot->left->parent = n;
    pivot->left = n;
    n->parent = pivot;
}

static void rotate_right(tree_node *n, tree_node **root)
{
    tree_node *pivot;

    if (!n || !n->left)
        return;

    pivot = n->left;
    pivot->parent = n->parent;

    if (n->parent) {
        if (n == n->parent->left)
            n->parent->left = pivot;
        else
            n->parent->right = pivot;
    } else 
        *root = pivot;

    n->left = pivot->right;
    if (pivot->right)
        pivot->right->parent = n;
    pivot->right = n;
    n->parent = pivot;
}

static void insert_case_1(tree_node *n, tree_node **root)
{
    if (!(n->parent))
        n->color = black;
    else if (n->parent->color == red)
        insert_case_2(n, root);
}

static void insert_case_2(tree_node *n, tree_node **root)
{
    tree_node *gp, *un;

    un = uncle(n);
    if (is_red(un)) {
        n->parent->color = black;
        un->color = black;

        gp = grandparent(n);
        gp->color = red;
        insert_case_1(gp, root);
    } else 
        insert_case_3(n, root);
}

static void insert_case_3(tree_node *n, tree_node **root)
{
    tree_node *gp;

    gp = grandparent(n);
    if (n->parent == gp->left && n == n->parent->right) {
        rotate_left(n->parent, root);
        n = n->left;
    } else if (n->parent == gp->right && n == n->parent->left) {
        rotate_right(n->parent, root);
        n = n->right;
    }

    insert_case_4(n, root);
}
    
static void insert_case_4(tree_node *n, tree_node **root)
{
    tree_node *gp;

    gp = grandparent(n);
    if (n->parent == gp->left)
        rotate_right(gp, root);
    else
        rotate_left(gp, root);

    n->parent->color = black;
    gp->color = red;
}
