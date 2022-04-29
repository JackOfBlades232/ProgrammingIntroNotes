#include <stdio.h>
#include <stdarg.h>


static int sum(int c, ...)
{
    va_list vl;
    int s = c;
    int k;

    va_start(vl, c);
    while((k = va_arg(vl, int)) != 0)
        s += k;
    va_end(vl);

    return s;
}

static void print_times(const char *str, ...)
{
    va_list vl;
    const char *p;

    va_start(vl, str);
    for(p = str; p; p = va_arg(vl, const char *)) {
        int n, i;
        n = va_arg(vl, int);
        for(i = 0; i < n; i++)
            printf("%s ", p);
        printf("\n");
    }
    va_end(vl);
}

int main()
{
    printf("%d\n", sum(5, 40, 300, 2000, 10000, 0));
    print_times("first", 1, "second", 2, "third", 3, "seven", 7, NULL);
    return 0;
}
