/* 4_16.c */
/* #define RECURSION 1 */

#include <stdio.h>
#include <stdlib.h>

typedef struct tag_item {
    int data;
    struct tag_item *next;
} item;

item *int_array_to_list(const int *arr, int len)
{
#if RECURSION
    item *tmp;

    if (!len)
        return NULL;

    tmp = malloc(sizeof(item));
    tmp->data = *arr;
    tmp->next = int_array_to_list(arr + 1, len - 1);

    return tmp;
#else
    item *first = NULL, *tmp;
    int i;

    for (i = len-1; i >= 0; i--) {
        tmp = malloc(sizeof(item));
        tmp->data = arr[i];
        tmp->next = first;
        first = tmp;
    }

    return first;
#endif
}

#if RECURSION
item *list_to_list_with_len(const item *lst, item *first)
{
    item *tmp;

    if (!first) {
        first = malloc(sizeof(item));
        first->data = 0;
        first->next = NULL;
    }

    if (!lst)
        return NULL;

    tmp = malloc(sizeof(item));
    tmp->data = lst->data;
    tmp->next = list_to_list_with_len(lst->next, first);

    first->data++;
    if (!first->next)
        first->next = tmp;

    return first;
}
#else
item *list_to_list_with_len(const item *lst)
{
    item *first = NULL, *last = NULL, *tmp;

    first = malloc(sizeof(item));
    first->data = 0;
    first->next = NULL;

    for (; lst; lst = lst->next) {
        tmp = malloc(sizeof(item));
        tmp->data = lst->data;
        tmp->next = NULL;

        first->data++;
        last->next = tmp;
    }

    return first;
}
#endif

int main()
{
    return 0;
}
