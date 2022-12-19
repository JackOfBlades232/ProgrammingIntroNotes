/* main.c */
#include "mod.h"

/* decl, will search in modules */
void somefunc();

/* for fetching global vars */
extern int global_var;

int main() 
{
    somefunc();

    return 0;
}
