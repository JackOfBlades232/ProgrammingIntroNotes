/* 4_22/rbtree.c */
#include <stdlib.h>

typedef enum tag_node_color { red, black } node_color;

typedef struct tag_tree_node {
    char *key;
    void *data;
    node_color color;
    struct tag_tree_node *left, *right, *parent;
} tree_node;
