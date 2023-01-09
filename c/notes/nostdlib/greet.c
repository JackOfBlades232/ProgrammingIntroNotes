/* nostdlib/greet.c */
#include <stdio.h>

int main (int argc, char **argv)
{
    if (argc < 2) {
        printf("I do not know how to greet you\n");
        return 1;
    }

    printf("Hello, dear %s\n", argv[1]);
    
    return 0;
}
