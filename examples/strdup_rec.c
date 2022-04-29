#include <stdio.h>
#include <stdlib.h>

char *strdup_rec_do(const char *str, int depth)
{
    char *res;
    if(*str)
        res = strdup_rec_do(str+1, depth+1);
    else
        res = malloc(depth+1);
    res[depth] = *str;
    return res;
}
char *strdup_rec(const char *str)
{
    return strdup_rec_do(str, 0);
}

int main(int argc, char**argv)
{
    char *s = strdup_rec(argv[0]);
    puts(s);
    free(s);
    return 0;
}
