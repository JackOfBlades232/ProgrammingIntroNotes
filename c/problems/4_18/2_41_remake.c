/* 4_18/2_41_remake.c */
#include <stdio.h>
#include <stdlib.h>

typedef struct tag_item {
    long data;
    int count;
    struct tag_item *next;
} item;

void upd_max_val(int new, int *max)
{
    if (new > *max)
        *max = new;
}

void add_item_to_long_list_and_upd_max_cnt(long x, item **lst, int *max_cnt)
{
    item *cur, *prev = NULL;

    for (cur = *lst; cur; cur = cur->next) {
        if (cur->data == x) {
            (cur->count)++;
            upd_max_val(cur->count, max_cnt);
            return;
        }

        prev = cur;
    }

    cur = malloc(sizeof(item));
    cur->data = x;
    cur->count = 1;
    cur->next = NULL;

    upd_max_val(cur->count, max_cnt);

    if (!(*lst))
        *lst = cur;
    else
        prev->next = cur;

    return;
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
    int items_read, max_cnt;
    long val;

    for (items_read = 1, max_cnt = 0;;) {
        items_read = scanf("%ld", &val);
        
        if (items_read != 1)
            break;

        add_item_to_long_list_and_upd_max_cnt(val, &lst, &max_cnt);
    }

    for (cur = lst; cur; cur = cur->next)
        if (cur->count == max_cnt)
            printf("%ld ", cur->data);

    putchar('\n');

    delete_long_list(lst);

    return 0;
}
