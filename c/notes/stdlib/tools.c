/* tools.c */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>

int main()
{
    char *sp;
    char st[] = "Some string\n";
    int r;

    /* heap */

    sp = calloc(10, sizeof(char)); /* allocates anz 0-es out cont mem */
    free(sp);
    sp = malloc(10 * sizeof(char)); /* same, but no 0ing out, and faster */

    sp = realloc(sp, 12 * sizeof(char)); /* reallocates mem 
                                            (usually, finds new and copies) */
    /* realloc(NULL, x) == malloc, realloc(p, 0) == free */

    /* strings */
    
    printf("%ld\n", strlen(st));
    sp = strcpy(sp, st); /* danger: overflow. copy rets dest, 
                            but doesnt change it */
    strncpy(sp, st, 12); /* copy with num chars (anti-overflow) */
    /* copy does not know about #0 byte */
    free(sp);

    sp = strdup(st); /* malloc and copy */

    /* compare -- full str and only first n chars */
    printf(strcmp(sp, st) ? "St != sp\n" : "St == sp\n");
    printf(strncmp(sp, st, 6) ? "St != sp\n" : "St == sp\n");

    free(sp);

    /* looks for left/right char in str and rets adr/NULL */
    sp = strchr(st, 'a');
    sp = strrchr(st, 'a');

    /* looks for substring, returns adr/NULL */
    sp = strstr(st, "string");

    sp = malloc(5 * sizeof(char));
    sp = memset(sp, 0, 5 * sizeof(char)); /* fills mem with value */

    sp = realloc(sp, 12 * sizeof(char));
    sp = memcpy(sp, st, 12 * sizeof(char)); /* non-string copy (fast) */
    sp = memmove(sp, st, 12 * sizeof(char)); /* a smarter copy: if overlap, 
                                                copies one from end, keeps 
                                                copy correct (but slower) */

    printf("%s%s", st, sp);

    free(sp);

    /* random */

    srand(time(NULL)); /* set rand seed with current time (1 by default) */
    r = rand(); /* random from 0 to RAND_MAX with set seed */

    /* clamping with uniformity */
    r = 1 + (int)(12.0*rand()/(RAND_MAX+1.0)); /* 1-12 */
    
    return 0;
}
