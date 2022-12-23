/* 4_22/rbtree.c */
#include "rbtree.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* API Impl and forward declarations */

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

    if (n)
        insert_case_1(n, root);

    return n != NULL;
}

static void substitute_and_remove_element(tree_node *n, tree_node **root);
static tree_node *rightmost_element_in_subtree(tree_node *root);
static tree_node *replace_with_child_and_get_parent(
        tree_node **n, tree_node **root);
static void delete_case_1(tree_node *n, tree_node* parent, tree_node **root);
static void delete_case_2(tree_node *n, tree_node* parent, tree_node **root);
static void delete_case_3(tree_node *n, tree_node* parent, tree_node **root);
static void delete_case_4(tree_node *n, tree_node* parent, tree_node **root);
static void delete_case_5(tree_node *n, tree_node* parent, tree_node **root);

int rbtree_remove_element(tree_node **root, const char *key)
{
    tree_node *n;

    n = (tree_node *)rbtree_get_element(*root, key);

    if (!n)
        return 0;

    substitute_and_remove_element(n, root);
    return 1;
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

/* Memory functions */

static tree_node *create_node(tree_node *parent, const char *key, void *data)
{
    tree_node *n;

    n = malloc(sizeof(*n));
    n->key = malloc((strlen(key) + 1) * sizeof(char));
    strcpy(n->key, key);
    n->data = data;
    n->left = n->right = NULL;
    n->parent = parent;
    n->color = red;

    return n;
}

static void destroy_node(tree_node* n)
{
    if (n) {
        if (n->key)
            free(n->key);
        free(n);
    }
}

/* General data structure utility functions */

static int is_black(tree_node *n) 
{ 
    return !n || n->color == black; 
}

static int is_red(tree_node *n) 
{ 
    return !is_black(n);
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

static tree_node *sibling(const tree_node *n, const tree_node *parent)
{
    if (!parent)
        return NULL;

    return n == parent->left ? parent->right : parent->left;
}

static tree_node *child(const tree_node *n)
{
    if (!n)
        return NULL;

    return n->left ? n->left : n->right;
}

static void rotate_left(tree_node *n, tree_node **root)
{
    tree_node *pivot;

    if (!n || !(n->right))
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
    n->parent = pivot;
    pivot->left = n;
}

static void rotate_right(tree_node *n, tree_node **root)
{
    tree_node *pivot;

    if (!n || !(n->left))
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
    n->parent = pivot;
    pivot->right = n;
}

/* Debug */

#ifdef DEBUG

static void debug_print_node(tree_node *n, int depth)
{
    int i;

    for (i = 0; i < depth; i++)
        printf("    ");
    if (!n)
        printf(" null\n");
    else {
        printf("%s, ", n->key); 
        printf(is_black(n) ? "b\n" : "r\n");
    }
}

static void debug_print_tree(tree_node *root, int depth)
{
    if (!root) {
        debug_print_node(root, depth);
        return;
    }

    debug_print_tree(root->left, depth+1);
    debug_print_node(root, depth);
    debug_print_tree(root->right, depth+1);
}

#endif

/* Insertion */

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

static void insert_case_1(tree_node *n, tree_node **root)
{
    if (!(n->parent))
        n->color = black;
    else if (is_red(n->parent))
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
    n->parent->color = black;
    gp->color = red;
    
    if (n == n->parent->left && n->parent == gp->left)
        rotate_right(gp, root);
    else
        rotate_left(gp, root);
}

/* Deletion */

static void substitute_and_remove_element(tree_node *n, tree_node **root)
{
    tree_node *repl = NULL, *parent;
    char *key_tmp;
    node_color old_color;

    if (n->left && n->right) {
        repl = rightmost_element_in_subtree(n->left);

        key_tmp = repl->key;
        repl->key = n->key;
        n->key = key_tmp;

        n->data = repl->data;
        n = repl;
    }

    old_color = n->color;
    
    parent = replace_with_child_and_get_parent(&n, root);
    if (old_color == black)
        delete_case_1(n, parent, root);
}

static tree_node *rightmost_element_in_subtree(tree_node *root)
{
    if (!root || !(root->right))
        return root;

    return rightmost_element_in_subtree(root->right);
}

static tree_node *replace_with_child_and_get_parent(
        tree_node **np, tree_node **root)
{
    tree_node *c, *parent;

    c = child(*np);
    if (c)
        c->parent = (*np)->parent;

    if (!((*np)->parent))
        *root = c;
    else if ((*np) == (*np)->parent->left)
        (*np)->parent->left = c;
    else
        (*np)->parent->right = c;

    parent = (*np)->parent;
    destroy_node(*np);
    *np = c;

    return parent;
}

static void delete_case_1(tree_node *n, tree_node* parent, tree_node **root)
{
    if (is_red(n))
        n->color = black;
    else if (parent)
        delete_case_2(n, parent, root);
}

static void delete_case_2(tree_node *n, tree_node* parent, tree_node **root)
{
    tree_node *s;
    s = sibling(n, parent);

    if (is_red(s)) {
        s->color = black;
        parent->color = red;
        if (n == parent->left)
            rotate_left(parent, root);
        else
            rotate_right(parent, root);
    }

    delete_case_3(n, parent, root);
}

static void delete_case_3(tree_node *n, tree_node* parent, tree_node **root)
{
    tree_node *s;
    s = sibling(n, parent);

    if (s && is_black(s->left) && is_black(s->right)) {
        s->color = red;
        if (parent->color == black)
            delete_case_1(parent, parent->parent, root);
        else
            parent->color = black;
    } else 
        delete_case_4(n, parent, root);
}

static void delete_case_4(tree_node *n, tree_node* parent, tree_node **root)
{
    tree_node *s;
    s = sibling(n, parent);

    if (s && n == parent->left && is_red(s->left) && is_black(s->right)) {
        s->color = red;
        s->left->color = black;
        rotate_right(s, root);
    } 
    else if (s && n == parent->right && is_red(s->right) && is_black(s->left)) {
        s->color = red;
        s->right->color = black;
        rotate_left(s, root);
    }

    delete_case_5(n, parent, root);
}

static void delete_case_5(tree_node *n, tree_node* parent, tree_node **root)
{
    tree_node *s;
    s = sibling(n, parent);

    s->color = parent->color;
    parent->color = black;

    if (n == parent->left) {
        s->right->color = black;
        rotate_left(parent, root);
    } else {
        s->left->color = black;
        rotate_right(parent, root);
    }
}
