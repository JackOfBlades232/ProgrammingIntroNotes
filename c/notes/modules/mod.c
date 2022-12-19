/* mod.c */
#include <stdio.h>
#include "mod.h" /* for compiler-checking that headers and impls match */

struct item {
    int data;
    struct item *next;
};

int global_var = 1;

/* by default global objs and funcs are global, static hides them */
static int local_func()
{
    return 1;
}

void somefunc()
{
    printf("useless %d\n", local_func());
}
