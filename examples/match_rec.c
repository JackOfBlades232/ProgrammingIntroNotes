#include <stdio.h>

int starmatch(const char *str, const char *pat);

int match(const char *str, const char *pat)
{
    switch(*pat) {
        case 0:
            return *str == 0;
        case '*':
            return starmatch(str, pat+1);
        default:
            if(!*str || (*str != *pat && *pat != '?'))
                return 0;
            return
                match(str+1, pat+1);
    }
}

#if 1
int starmatch(const char *str, const char *pat)
{
    if(match(str, pat))
        return 1;
    if(!*str)
        return 0;
    return starmatch(str+1, pat);
}
#else
int starmatch(const char *str, const char *pat)
{
    if(!*str)
        return match("", pat);
    if(match(str, pat))
        return 1;
    return starmatch(str+1, pat);
}
#endif


int main(int argc, const char **argv)
{
    if(argc < 3) {
        printf("Need two arguments\n");
        return 1;
    }
    if(match(argv[1], argv[2]))
        printf("YES\n");
    else
        printf("NO\n");
    return 0;
}
