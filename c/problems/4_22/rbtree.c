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

    if (n)
        insert_case_1(n, root);

    return n != NULL;
}

static tree_node *rightmost_node_in_subtree(tree_node *root);
static void replace_with_child(tree_node *n, tree_node *c, tree_node **root);
static void delete_one_child(tree_node *n, tree_node **root);
static void delete_case_1(tree_node *n, tree_node **root);
static void delete_case_2(tree_node *n, tree_node **root);
static void delete_case_3(tree_node *n, tree_node **root);
static void delete_case_4(tree_node *n, tree_node **root);
static void delete_case_5(tree_node *n, tree_node **root); 
static void delete_case_6(tree_node *n, tree_node **root);

int rbtree_remove_element(tree_node **root, const char *key)
{
    tree_node *n, *r;

    n = (tree_node *)rbtree_get_element(*root, key);
    if (!n)
        return 0;

    if (n->left && n->right) {
        r = rightmost_node_in_subtree(n->left);

        free(n->key);
        n->key = r->key;
        n->data = r->data;
        r->key = NULL;

        n = r;
    }

    delete_one_child(n, root);
    return 1;
}

void print_with_info(const tree_node *root, int depth)
{
    if (!root) {
        printf("nil %d\n", depth);
        return;
    }

    print_with_info(root->left, depth+1);
    printf("%s %d ", root->key, depth);
    printf(root->color == black ? "black" : "red");
    if (root->parent) {
        printf(" parent: %s ", root->parent->key);
        printf(root->parent->color == black ? "black" : "red ");
    }
    if (root->left) {
        printf(" left: %s ", root->left->key);
        printf(root->left->color == black ? "black" : "red ");
    }
    if (root->right) {
        printf(" right: %s ", root->right->key);
        printf(root->right->color == black ? "black" : "red ");
    }
    putchar('\n');
    print_with_info(root->right, depth+1);
}

void rbtree_print(const tree_node *root)
{
    print_with_info(root, 0);
    return;

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

static int children_are_black(tree_node *n)
{
    return !n || (is_black(n->left) && is_black(n->right));
}

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

static tree_node *sibling(const tree_node *n)
{
    if (!n || !(n->parent))
        return NULL;

    return n == n->parent->left ? n->parent->right : n->parent->left;
}

static tree_node *child(const tree_node *n)
{
    if (!n)
        return NULL;

    return n->right ? n->right : n->left;
}

static void rotate_left(tree_node *n, tree_node **root)
{
    tree_node *pivot;

    if (!n || !(n->right))
        return;

    printf("rl\n");

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

    printf("rr\n");

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
    printf("ic1\n");
    if (!(n->parent)) {
        printf("recolor\n");
        n->color = black;
    } else if (n->parent->color == red)
        insert_case_2(n, root);
}

static void insert_case_2(tree_node *n, tree_node **root)
{
    tree_node *gp, *un;

    printf("ic2\n");

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

    printf("ic3\n");

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

    printf("ic4\n");

    gp = grandparent(n);
    n->parent->color = black;
    gp->color = red;
    
    if (n == n->parent->left && n->parent == gp->left)
        rotate_right(gp, root);
    else
        rotate_left(gp, root);
}

static tree_node *rightmost_node_in_subtree(tree_node *root)
{
    if (!(root->right))
        return root;

    return rightmost_node_in_subtree(root->right);
}

static void replace_with_child(tree_node *n, tree_node *c, tree_node **root)
{
    if (c)
        c->parent = n->parent;

    if (!(n->parent))
        *root = c;
    else if (n == n->parent->left)
        n->parent->left = c;
    else
        n->parent->right = c;
}

static void delete_one_child(tree_node *n, tree_node **root)
{
    tree_node *c;
    c = child(n);
    
    if (n->color == black) {
        if (is_red(c))
            c->color = black;
        else if (n->parent) {
            delete_case_1(n, root);
        }
    }

    print_with_info(*root, 0);

    replace_with_child(n, c, root);
    destroy_node(n);
}

static void delete_case_1(tree_node *n, tree_node **root) 
{
    printf("dc1\n");

    if (n->parent)
        delete_case_2(n, root);
}

static void delete_case_2(tree_node *n, tree_node **root) 
{
    tree_node *s;
    s = sibling(n);

    printf("dc2\n");

    if (is_red(s)) {
        printf("dc2red\n");
        n->parent->color = red;
        s->color = black;

        if (n == n->parent->left)
            rotate_left(n, root);
        else
            rotate_right(n, root);
    }
    
    delete_case_3(n, root);
}

static void delete_case_3(tree_node *n, tree_node **root) 
{
    tree_node *s;
    s = sibling(n);

    printf("dc3\n");

    if (is_black(s) && is_black(n->parent) && children_are_black(s)) {
        s->color = red;
        delete_case_1(n->parent, root);
    } else 
        delete_case_4(n, root);
}

static void delete_case_4(tree_node *n, tree_node **root) 
{
    tree_node *s;
    s = sibling(n);

    printf("dc4\n");

    if (is_black(s) && is_red(n->parent) && children_are_black(s)) {
        printf("dc4fin\n");
        s->color = red;
        n->parent->color = black;
    } else
        delete_case_5(n, root);
}

static void delete_case_5(tree_node *n, tree_node **root) 
{
    tree_node *s;
    s = sibling(n);

    printf("dc5\n");

    if (is_black(s)) {
        printf("dc5black\n");
        if (n == n->parent->left && is_red(s->left) && is_black(s->right)) {
            s->color = red;
            s->left->color = black;
            rotate_right(s, root);
        } else if (n == n->parent->right &&
                is_red(s->right) && is_black(s->left)) {
            s->color = red;
            s->right->color = black;
            rotate_left(s, root);
        }
    }

    delete_case_6(n, root);
}

static void delete_case_6(tree_node *n, tree_node **root) 
{
    tree_node *s;
    s = sibling(n);

    printf("dc6\n");

    s->color = n->parent->color;
    n->parent->color = black;

    if (n == n->parent->left) {
        s->right->color = black;
        rotate_left(n->parent, root);
    } else {
        s->left->color = black;
        rotate_right(n->parent, root);
    }
}
