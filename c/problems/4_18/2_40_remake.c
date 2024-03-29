/* 4_28/2_40_remake.c */
#include <stdio.h>
#include <stdlib.h>

enum constants { desired_count_for_print = 3 };

typedef struct tag_item {
    long data;
    int count;
    struct tag_item *next;
} item;

item *add_item_to_long_list(long x, item *lst)
{
    item *cur, *prev = NULL;

    for (cur = lst; cur; cur = cur->next) {
        if (cur->data == x) {
            (cur->count)++;
            return lst;
        }

        prev = cur;
    }

    cur = malloc(sizeof(item));
    cur->data = x;
    cur->count = 1;
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


int main()
{
    item *lst = NULL, *cur;
    int items_read = 1;
    long val;

    for (;;) {
        items_read = scanf("%ld", &val);
        
        if (items_read != 1)
            break;

        lst = add_item_to_long_list(val, lst);
    }

    for (cur = lst; cur; cur = cur->next)
        if (cur->count == 3)
            printf("%ld ", cur->data);

    putchar('\n');

    delete_long_list(lst);

    return 0;
}
