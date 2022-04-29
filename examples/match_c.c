#include <stdio.h>

int match(const char *str, const char *pat)
{
    int i;
    for(;; str++, pat++) {
        switch(*pat) {
            case 0:
                return *str == 0;
            case '*':
                for(i=0; ; i++) {
                    if(match(str+i, pat+1))
                        return 1;
                    if(!str[i])
                        return 0;
                }
            case '?':
                if(!*str)
                    return 0;
                break;
            default:
                if(*str != *pat)
                    return 0;
        }
    }
}


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
