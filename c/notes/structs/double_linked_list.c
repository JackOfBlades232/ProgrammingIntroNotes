/* double_linked_list.c */
#include <stdio.h>
#include <stdlib.h>

struct dbl_item {
    double data;
    struct dbl_item *prev, *next;
};

void add_item_to_beginning_of_dbl_list(double x, 
        struct dbl_item **first, struct dbl_item **last)
{
    struct dbl_item *tmp;
    
    tmp = malloc(sizeof(struct dbl_item));
    tmp->data = x;
    tmp->prev = NULL;
    tmp->next = *first;
    *(*first ? &(*first)->prev : last) = tmp;
    *first = tmp;
}

void add_item_to_end_of_dbl_list(double x, 
        struct dbl_item **first, struct dbl_item **last)
{
    struct dbl_item *tmp;
    
    tmp = malloc(sizeof(struct dbl_item));
    tmp->data = x;
    tmp->prev = *last;
    tmp->next = NULL;
    *(*last ? &(*last)->next : first) = tmp;
    *last = tmp;
}

void pop_item_from_beginning_of_dbl_list(double *out,
        struct dbl_item **first, struct dbl_item **last)
{
    struct dbl_item *tmp;
    
    if (*first) {
        tmp = *first;
        *out = (*first)->data;
        *first = (*first)->next;
        *(*first ? &(*first)->prev : last) = NULL;
        free(tmp);
    } else
        out = NULL;
}

void pop_item_from_end_of_dbl_list(double *out,
        struct dbl_item **first, struct dbl_item **last)
{
    struct dbl_item *tmp;
    
    if (*last) {
        tmp = *last;
        *out = (*last)->data;
        *last = (*last)->prev;
        *(*last ? &(*last)->next: first) = NULL;
        free(tmp);
    } else
        out = NULL;
}

void delete_dbl_list(struct dbl_item **first, struct dbl_item **last)
{
    if (*first) {
        while ((*first = (*first)->next))
            free((*first)->prev);
        free(*last);
        last = NULL;
    }
}

void insert_item_before_current_in_dbl_list(double x, 
        struct dbl_item **current, struct dbl_item **first)
{
    struct dbl_item *tmp;
    
    tmp = malloc(sizeof(struct dbl_item));
    tmp->data = x;
    tmp->prev = (*current)->prev;
    tmp->next = *current;
    (*current)->prev = tmp;
    *(tmp->prev ? &tmp->prev->next : first) = tmp;
}

void insert_item_after_current_in_dbl_list(double x, 
        struct dbl_item **current, struct dbl_item **last)
{
    struct dbl_item *tmp;
    
    tmp = malloc(sizeof(struct dbl_item));
    tmp->data = x;
    tmp->prev = *current;
    tmp->next = (*current)->next;
    (*current)->next = tmp;
    *(tmp->next ? &tmp->next->prev : last) = tmp;
}

void pop_current_item_from_dbl_list(double *out, struct dbl_item **current,
        struct dbl_item **first, struct dbl_item **last)
{
    *((*current)->prev ? &(*current)->prev->next : first) = (*current)->next;
    *((*current)->next ? &(*current)->next->prev : last) = (*current)->prev;

    if (*current)
        *out = (*current)->data;
    else
        out = NULL;

    free(*current);
    *current = NULL;
}

void print_dbl_list(const struct dbl_item *first)
{
    if (first) {
        printf("%lf ", first->data);
        print_dbl_list(first->next);
    } else
        putchar('\n');
}

int main()
{
    struct dbl_item *first = NULL, *last = NULL, *current = NULL;
    double out;

    add_item_to_beginning_of_dbl_list(1.0, &first, &last);
    add_item_to_beginning_of_dbl_list(2.0, &first, &last);
    add_item_to_end_of_dbl_list(3.0, &first, &last);
    add_item_to_end_of_dbl_list(4.0, &first, &last);
    print_dbl_list(first);

    pop_item_from_beginning_of_dbl_list(&out, &first, &last);
    printf("%lf\n", out);
    pop_item_from_end_of_dbl_list(&out, &first, &last);
    printf("%lf\n", out);
    print_dbl_list(first);

    current = first;
    insert_item_after_current_in_dbl_list(7.0, &current, &last);
    current = first->next;
    insert_item_before_current_in_dbl_list(9.0, &current, &first);
    print_dbl_list(first);

    current = last->prev;
    pop_current_item_from_dbl_list(&out, &current, &first, &last);
    printf("%lf\n", out);
    print_dbl_list(first);

    delete_dbl_list(&first, &last);

    return 0;
}
