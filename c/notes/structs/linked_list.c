/* linked_list.c */
#include <stdio.h>
#include <stdlib.h>

struct item {
    int data;
    struct item *next;
};

struct item *int_array_to_list(const int *arr, int len)
{
    struct item *tmp;
    if (!len)
        return NULL;
    tmp = malloc(sizeof(struct item));
    tmp->data = *arr;
    tmp->next = int_array_to_list(arr + 1, len - 1);
    return tmp;
}

int int_list_sum(const struct item *lst)
{
    return lst ? lst->data + int_list_sum(lst->next) : 0;
}

void delete_int_list(struct item *lst)
{
    if (lst) {
        delete_int_list(lst->next);
        free(lst);
    }
}

struct item *delete_negatives_from_int_list(struct item *pcur)
{
    struct item *res = pcur;
    if (pcur) {
        pcur->next = delete_negatives_from_int_list(pcur->next);
        if (pcur->data < 0) {
            res = pcur->next;
            free(pcur);
        }
    }
    return res;
}

void print_int_list(const struct item *lst)
{
    if (lst) {
        printf("%d ", lst->data);
        print_int_list(lst->next);
    } else 
        putchar('\n');
}

int main()
{
    struct item *first;
    int arr[] = { 1, 2, -1, 2, -4, 5, 11, -123, 128 };

    first = int_array_to_list(arr, sizeof(arr)/sizeof(int));
    print_int_list(first);
    printf("sum: %d\n", int_list_sum(first));

    first = delete_negatives_from_int_list(first);
    print_int_list(first);

    delete_int_list(first);

    return 0;
}
