#include <stdio.h>


const char *strstr0(const char *string, const char *substr)
{
    int d;
    while(*string) {
        for(d = 0; substr[d] && string[d] == substr[d]; d++)
            {}
        if(!substr[d])
            return string;
        string++;
    }
    return NULL;
}

const char *strstr1(const char *string, const char *substr)
{
    int d;
    for(; *string; string++) {
        for(d = 0; string[d]; d++) {
            if(!substr[d])
                return string;
            if(substr[d] != string[d])
                goto next_char;
        }
        return substr[d] ? NULL : string;
    next_char:
        ;
    }
    return NULL;
}

const char *strstr2(const char *string, const char *substr)
{
    int d;
    for(; *string; string++) {
        for(d = 0; string[d]; d++) {
            if(!substr[d])
                return string;
            if(substr[d] != string[d])
                break;
        }
        if(!string[d])
            return substr[d] ? NULL : string;
    }
    return NULL;
}


int main(int argc, char **argv)
{
    printf("[%s]\n", strstr0(argv[1], argv[2]));
    printf("[%s]\n", strstr1(argv[1], argv[2]));
    printf("[%s]\n", strstr2(argv[1], argv[2]));
    return 0;
}
