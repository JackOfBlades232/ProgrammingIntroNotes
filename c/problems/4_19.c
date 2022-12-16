/* 4_28/2_40_remake.c */
#include <stdio.h>
#include <stdlib.h>

enum constants { max_diff_for_pair = 5 };

typedef struct tag_item {
    long data;
    struct tag_item *next;
} item;

item *add_item_to_long_list(long x, item *lst)
{
    item *cur = NULL, *prev;

    for (cur = lst; cur; prev = cur, cur = cur->next) 
        {}

    cur = malloc(sizeof(item));
    cur->data = x;
    cur->next = NULL;

    if (!lst)
        lst = cur;
    else
        prev->next = cur;

    return lst;
}

void delete_long_list(item *lst)
{
    item *tmp;

    while (lst) {
        tmp = lst;
        lst = lst->next;
        free(tmp);
    }
}

int abs_diff(int a, int b)
{
    return a > b ? a - b : b - a;
}

int main()
{
    item *lst = NULL, *cur = NULL, *prev;
    int items_read = 1;
    long val;

    for (;;) {
        items_read = scanf("%ld", &val);
        
        if (items_read != 1)
            break;

        lst = add_item_to_long_list(val, lst);
    }

    for (cur = lst; cur; prev = cur, cur = cur->next)
        if (prev && abs_diff(prev->data, cur->data) <= max_diff_for_pair)
            printf("%ld %ld\n", prev->data, cur->data);

    delete_long_list(lst);

    return 0;
}
