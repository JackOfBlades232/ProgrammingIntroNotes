/* like this we know, that our mem won't be fucked up.
 * (actually, you can cast it to another, non const type, and mess it up) 
 * So const is more like a promise */
void trustworthy_func(const int *a) {}

int main()
{
    const int iter_count = 78; /* const is still a runtime var, but prohibited
                                  to change. Came from c++ */

    /* c allows to use const vars, and runtime values to init arrays, meaning
     * variable length arrays. In terms of assembly, they are nothing like 
     * regular ones. Not good to use, their names are not a offset in stack,
     * but can be complex expressions, and some compilators use runtime libs
     * to deal with them, violating 0 runtime. Strange, high level feature, 
     * uncalled for in C (there are better high level langs) */

    const int *p; /* this means, that the mem is const, not the pointer val */

    /* a immutable pointer is this (strange shit): */
    int x;
    int * const q = &x;

    return 0;
}
