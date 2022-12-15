/* typedef.c */
#include <stdio.h>

typedef int *intptr; /* like var decl, makes it a type name */

typedef struct strct {
    int i;
    double d;
} mystruct; /* better to have different struct name and typedef name, it will
               work in c, but in c++ it will not */

int main()
{
    mystruct s1;

    s1.i = 1;
    s1.d = 2.0;
    printf("%d %lf\n", s1.i, s1.d);

    return 0;
}
