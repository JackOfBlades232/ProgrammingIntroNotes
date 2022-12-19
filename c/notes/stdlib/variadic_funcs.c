/* variadic_funcs.c */
#include <stdio.h>
#include <stdarg.h>

/* condition for last param is on the callers consciousness, at least 
 * 1 param is required */
int sum(int c, ...) /* variable param num */
{
    va_list vl; /* param list */
    int s = c, k;

    va_start(vl, c); /* macro, set last named param (to count from it's adr) */

    vprintf("%d %d %d %d %d %d %d %d\n", vl); /* v-copies for io variadic funcs
                                                 to use with va_list */

    while ((k = va_arg(vl, int)) != 0) /* get next param, macro */
        s += k;

    va_end(vl); /* finish macro */

    return s;
}

void print_times(const char *str, ...)
{
    va_list vl;
    const char *p;

    va_start(vl, str);

    for (p = str; p; p = va_arg(vl, const char *)) {
        int n, i;
        n = va_arg(vl, int);
        for (i = 0; i < n; i++)
            printf("%s ", p);
        printf("\n");
    }

    va_end(vl);
}

int main()
{
    printf("%d\n", sum(1, 2, 31, 1, 123, 19, 2, 0));
    print_times("once", 1, "twice", 2, "seven_times", 7, NULL);

    return 0;
}
