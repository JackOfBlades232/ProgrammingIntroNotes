#include <stdio.h>
int main()
{
    int c, pr;
    pr = 1;   /* true */
    while((c = getchar()) != EOF) {
        if(c == '\n') {
            putchar('\n');
            pr = 1;
        } else
        if(c == ' ') {
            pr = 0;
        } else {
            if(pr)
                putchar(c);
        }
    }
    return 0;
}
