/* format.c */
#include <stdio.h>

int main()
{
    void *p;
    long double x;
    int width, padding;
    int y;
    char s[6];
    int scanf_res;
    char buf[128];
    char str[32];
    int n;

    p = NULL;
    x = 1.12131;
    width = 6;
    padding = 4;

    /* different formatting tools */
    printf("%e %g %g %g %.2g %p\n", 0.01, 1.323, 0.0001, 0.00004, 1212.001, p);
    printf("[%-5d] [%010d] [%+d] [% d] [% d] [%.2f]\n", 
            12, 123213, 2, -1, 1, 1.123112);
    printf("%#X %#f %3d\n", 123, 12.0, 1234);
    printf("[%7.5s] [%6.4d]\n", "abracadabra", 12); 
    printf("[% #7.4Lg]\n", x);
    printf("%*.*d\n", width, padding, 12);

    /* scanf: * means read param, but don't put in stack var, spaces mean 
     * read until met non-space symbol. anything else -- require and read 
     * exactly that symbol */
    /* when reading strings, must specify len to avoid array overflow */
    /* they say, that scanf has it's uses only in small helper progs */
    scanf_res = 
        scanf("%4Lf %i %2s %3[abc] %4[]abc] %5[^xyz]", &x, &y, s, s, s, s);
    /* scanf returns number of successfully read vars, -1 if eof or error.
     * -1 = immediate eof, 0 = error with first, and so on */

    sprintf(buf, "%d %.20s", 1, "dalskdmqpwdmqwpd"); /* as printf, but res to
                                                        char arr. Can't spec 
                                                        size, so be careful
                                                        with format str len and
                                                        spec size with %s */
    printf("%s\n", buf);

    /* common use: int -> str */
    sprintf(str, "%d", n);
    /* sscanf -- same */

    return 0;
}
