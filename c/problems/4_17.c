/* 4_17.c */
#define RECURSION 1

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

item *list_to_list_with_len(const item *lst)
{
    item *first, *last = NULL, *tmp;

    last = first = malloc(sizeof(item));
    first->data = 0;
    first->next = NULL;

    for (; lst; lst = lst->next) {
        tmp = malloc(sizeof(item));
        tmp->data = lst->data;
        tmp->next = NULL;

        (first->data)++;

        last->next = tmp;
        last = last->next;
    }

    return first;
}

void print_int_list(const item *lst)
{
#if RECURSION
    if (lst) {
        printf("%d ", lst->data);
        print_int_list(lst->next);
    } else 
        putchar('\n');
#else
    for (; lst; lst = lst->next) 
        printf("%d ", lst->data);
    putchar('\n');
#endif
}

int main()
{
    int arr[] = { 3, 1, 2, 3, 3, 1, 2, 12, 128, -14 };
    item *lst, *new_lst = NULL;

    lst = int_array_to_list(arr, sizeof(arr)/sizeof(int));
    print_int_list(lst);

    new_lst = list_to_list_with_len(lst);

    print_int_list(new_lst);

    return 0;
}
