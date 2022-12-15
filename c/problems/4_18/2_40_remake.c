/* 4_28/2_40_remake.c */
#include <stdio.h>
#include <stdlib.h>

enum constants { desired_count_for_print = 3 };

typedef struct tag_item {
    long data;
    int count;
    struct tag_item *next;
} item;

void add_item_to_long_list(long x, item *lst)
{
    item *prev = NULL;

    for (; lst; lst = lst->next) {
        if (lst->data == x) {
            (lst->count)++;
            return;
        }

        prev = lst;
    }

    lst = malloc(sizeof(item));
    lst->data = x;
    lst->count = 0;
    lst->next = NULL;

    if (prev)
        prev->next = lst;

    if (!lst)
        lst = lst;

    return;
}

int main()
{
    item *lst = NULL;
    int items_read = 1;
    long val;

    for (;;) {
        items_read = scanf("%ld", &val);
        
        if (items_read != 1)
            break;

        add_item_to_long_list(val, lst);
    }

    for (; lst; lst = lst->next)
        if (lst->count == 3)
            printf("%ld ", lst->data);

    putchar('\n');

    return 0;
}
