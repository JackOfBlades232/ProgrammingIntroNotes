/* 4_22/prog.c */
#include "rbtree.h"
#include <stdio.h>

int read_line(char *command, char *key)
{
    *command = getchar();
    *key = getchar();

    return getchar() == 10;
}

int check_tree(const tree_node *root, int cur_count)
{
    /* TODO : write tree validity check */
}

int main()
{
    tree_node *root = NULL;
    const tree_node *n;
    char command, key[2];
    int add_res;

    while (read_line(&command, key)) {
        switch (command) {
            case 'a':
                add_res = rbtree_add_element(&root, key, NULL);
                printf(add_res ? "Added\n" : "Was in tree\n");
                break;
            case 'p':
                rbtree_print(root);
                break;
            case 'g':
                n = rbtree_get_element(root, key);
                printf(n ? "Found!\n" : "Not found\n");
                break;
            default:
                goto cleanup;
        }

        printf(root ? "NONNULL\n" : "NULL\n");
    }

cleanup:
    rbtree_destroy(root);

    return 0;
}
