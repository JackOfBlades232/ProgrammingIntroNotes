/* 4_21.c */
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

enum constants { init_capacity = 32, capacity_multiplier = 2 };

typedef struct tag_stackdbl {
    double *base, *top;
    int capacity;
} stackdbl;

stackdbl *stackdbl_init()
{
    stackdbl *st;

    st = malloc(sizeof(stackdbl));
    st->capacity = init_capacity;
    st->base = calloc(init_capacity, sizeof(double));
    st->top = NULL;

    return st;
}

static void stackdbl_resize(stackdbl *st)
{
    double *new_base, *p, *new_p;
    int top_offset;

    st->capacity *= capacity_multiplier;
    new_base = calloc(st->capacity, sizeof(char));

    top_offset = st->top ? st->top - st->base : -1;

    for (p = st->base, new_p = new_base; *p; p++, new_p++)
        *new_p = *p;

    free(st->base);
    st->base = new_base;

    st->top = st->top ? st->base + top_offset : NULL;
}

void stackdbl_destroy(stackdbl *st)
{
    free(st->base);
    free(st);
}

void stackdbl_push(double x, stackdbl *st)
{
    if (st->top && (st->top - st->base) >= st->capacity - 1)
        stackdbl_resize(st);

    if (st->top)
        st->top++;
    else
        st->top = st->base;

    *(st->top) = x;
} 

int stackdbl_pop(stackdbl *st, double *out)
{
    if (!st->top)
        return 0;

    *out = *(st->top);

    if (st->base == st->top)
        st->top = NULL;
    else
        (st->top)--;

    return 1;
}

int stackdbl_empty(stackdbl *st)
{
    return st->top == NULL;
}

static void stackdbl_print(stackdbl *st)
{
    double *p;
    
    for (p = st->top; p && p >= st->base; p--)
        printf("%lf ", *p);

    putchar('\n');
}

int main()
{
    stackdbl *st;
    int i;
    double x;

    st = stackdbl_init();

    stackdbl_print(st);

    stackdbl_push(2.0, st);
    stackdbl_push(1.0, st);
    stackdbl_push(3.0, st);

    stackdbl_print(st);

    for (i = 0; i < 3; i++) {
        stackdbl_pop(st, &x);
        printf("%lf\n", x);
        stackdbl_print(st);
    }

    printf(stackdbl_empty(st) ? "empty\n" : "non-empty\n");

    stackdbl_destroy(st);

    return 0;
}
