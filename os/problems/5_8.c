/* 5_8.c */
#include <stdio.h>
#include <sys/types.h>
#include <dirent.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>

enum { dirname_cap = 256 };

struct dirpath_node {
    char dirname[256];
    struct dirpath_node *prev, *next;
};

struct dirpath {
    struct dirpath_node *root;
    struct dirpath_node *last;
};

void init_dirpath(struct dirpath *dp)
{
    dp->root = NULL;
    dp->last = NULL;
}

void add_dir_to_path(struct dirpath *dp, const char *name)
{
    struct dirpath_node *new = malloc(sizeof(struct dirpath_node));
    strncpy(new->dirname, name, dirname_cap);
    new->next = NULL;

    if (dp->last) {
        dp->last->next = new;
        new->prev = dp->last;
    } else {
        new->prev = NULL;
        dp->root = new;
    }

    dp->last = new;
}

int remove_last_from_path(struct dirpath *dp)
{
    struct dirpath_node *tmp;

    if (!dp->root || !dp->root->next)
        return 0;

    tmp = dp->last;
    dp->last = dp->last->prev;
    free(tmp);
    dp->last->next = NULL;

    return 1;
}

void print_dirpath(struct dirpath *dp)
{
    struct dirpath_node *tmp;

    if (!dp->root)
        return;

    for (tmp = dp->root; tmp; tmp = tmp->next)
        printf("%s/", tmp->dirname);
}

int parse_dir(const char *name, const char *target, struct dirpath *dp)
{
    DIR *dir;
    struct dirent *dent;

    dir = opendir(name);
    if (!dir)
        return 1;
    if (chdir(name) == -1)
        return 1;

    add_dir_to_path(dp, name);

    while ((dent = readdir(dir)) != NULL) {
        if (strcmp(dent->d_name, target) == 0) {
            print_dirpath(dp);
            printf("%s\n", dent->d_name);
        }

        if (
                dent->d_type == DT_DIR && 
                strcmp(dent->d_name, ".") != 0 &&
                strcmp(dent->d_name, "..") != 0
           ) {
            parse_dir(dent->d_name, target, dp);
        }
    }
     
    remove_last_from_path(dp);
    chdir("..");
    closedir(dir);
    return 0;
}

int main(int argc, char **argv)
{
    struct dirpath dp;
    const char *root_name = ".";

    if (argc < 2) {
        fprintf(stderr, "Provide filename to query\n");
        return 1;
    }

    init_dirpath(&dp);
    return parse_dir(root_name, argv[1], &dp);
}
