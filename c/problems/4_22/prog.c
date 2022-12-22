/* 4_22/prog.c */
#include "rbtree.h"
#include <stdio.h>

#define INTERACTIVE

int read_line(char *command, char *key) {
    *command = getchar();
    *key = getchar();

    return getchar() == 10;
}

int check_black_root_property(const tree_node *root) 
{
    return !root || root->color == black;
}

int check_red_node_children_property(const tree_node *root)
{
    if (!root)
        return 1;

    if (root->parent && root->color == red && root->parent->color == red)
        return 0;

    return check_red_node_children_property(root->left) && 
        check_red_node_children_property(root->right);
}

int check_leaf_depth_property_iteratively(const tree_node *root,
        int cur_depth, int *req_depth)
{
    if (!root) {
        if (*req_depth >= 0 && cur_depth != *req_depth)
            return 0;
        else if (*req_depth < 0)
            *req_depth = cur_depth;

        return 1;
    }

    if (root->color == black)
        cur_depth++;

    return check_leaf_depth_property_iteratively(
            root->left, cur_depth, req_depth) &&
        check_leaf_depth_property_iteratively(
                root->right, cur_depth, req_depth);
}

int check_leaf_depth_property(const tree_node *root)
{
    int req_depth = -1;

    return check_leaf_depth_property_iteratively(root, 0, &req_depth);
}

int check_tree(const tree_node *root) 
{
    return check_black_root_property(root) &&
        check_red_node_children_property(root) &&
        check_leaf_depth_property(root);
}

int main() {
    tree_node *root = NULL;
    char command, key[2];
    int prog_res = 1;
#ifdef INTERACTIVE
    const tree_node *n;
    int op_res;
#endif

    while (read_line(&command, key)) {
        switch (command) {
            case 'a':
#ifdef INTERACTIVE
                op_res = rbtree_add_element(&root, key, NULL);
                printf(op_res ? "Added\n" : "Was in tree\n");
#else
                rbtree_add_element(&root, key, NULL);
#endif
                break;
            case 'd':
#ifdef INTERACTIVE
                op_res = rbtree_remove_element(&root, key);
                printf(op_res ? "Removed\n" : "Was not in tree\n");
#else
                rbtree_remove_element(&root, key);
#endif
                break;
#ifdef INTERACTIVE
            case 'p':
                rbtree_print(root);
                break;
            case 'g':
                n = rbtree_get_element(root, key);
                printf(n ? "Found!\n" : "Not found\n");
                break;
#endif
            case 'c':
                prog_res = check_tree(root);
#ifdef INTERACTIVE
                printf(prog_res ? "Valid!\n" : "Not valid\n");
#else
                if (!prog_res)
                    printf("Not valid!\n");
#endif
            default:
                goto cleanup;
        }
    }

cleanup:
    rbtree_destroy(root);

    return !prog_res;
}
