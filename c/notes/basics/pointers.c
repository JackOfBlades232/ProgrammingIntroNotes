#include <stddef.h>

/* use for pointers */
void swap(int *a, int *b)
{
    int t;
    t = *a;
    *a = *b;
    *b = t;
}

int main()
{
    /* addresses are not just numbers in c (like in asm), but separate types */
    int *p;
    double *q;
    char *s1, *s2, *s3; /* reason to append * to variable name */

    void *z; /* pointer to anything, no constraint on value type */

    int x, y;

    p = &x; /* taking address */
    *p = 27; /* *pointer == value */

    z = NULL; /* pointer to no mem, most times =0, but depends on computer. 
                 equals to false in conditions */
    q = z; /* can put a void pointer in a typed pointer */

    y = 13;
    swap(&x, &y);

    return 0;
}
