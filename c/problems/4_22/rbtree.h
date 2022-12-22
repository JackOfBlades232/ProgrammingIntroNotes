/* 4_22/rbtree.h */
#ifndef RBTREE_SENTRY
#define RBTREE_SENTRY

typedef enum tag_node_color { red, black } node_color;

typedef struct tag_tree_node {
    char *key;
    void *data;
    node_color color;
    struct tag_tree_node *left, *right, *parent;
} tree_node;

const tree_node *rbtree_get_element(const tree_node *root, const char *key);
int rbtree_add_element(tree_node **root, const char *key, void *data);
int rbtree_remove_element(tree_node **root, const char *key);
void rbtree_print(const tree_node *root);
void rbtree_destroy(tree_node *root);

#endif
