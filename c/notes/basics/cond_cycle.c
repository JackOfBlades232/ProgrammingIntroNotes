#include <stdio.h>

int main()
{
    /* all local variables should (must before C99) inited at the beginning of 
     * a code block {} */
    int m[] = { 23, 20, 17, 8, 3 };
    int i, j, flag;
    double s, k;

    i = 2;
    /* cond must be integer or address type */
    if (m[i] > m[i+1]) {
        int t;
        t = m[i];
        m[i] = m[i+1];
        m[i+1] = t;
        flag = 1;
    }
    else {
        flag = 0;
    }

    i = 0;
    while (i < 5) {
        ++i;
        printf("%i, ", m[i]);
    }
    printf("\n");

    do {
        printf("%i, ", m[i]);
        i--;
    } while (i >= 0);

    /* for has 3 ops -- initialization, condition, iteration, 
     * these can be anything */
    for (j = 1; j <= 25; j++)
        printf("%d x %d\t = %d\n", j, j, j*j);

    for (s = 0; scanf("%lf", &k) == 1; s += k)
        ;
    printf("\n%lf\n", s);

    /* can pack more ops with , */
    for (i = 0, j = 5; i < j; i++, j--) {
        if (i == 1)
            continue;

        printf("%i %i\n", i, j);
        
        if (i == 2)
            break;
    }

    goto quit; /* can go to any label inside the function,
                  even into/out of {} */

    /* goes to the given label (case of i, or default), and will do everything 
     * till the end of switch. Remedied via break. Case vals must be compile-
     * time consts */
    switch (i) {
        case 1:
            printf("First\n");
        case 2:
            printf("Second\n");
            break;
        case 3:
            printf("Third\n");
        default:
            printf("More\n");
    }
    /* can also goto to exit switch, instead of break, or continue, if is in 
     * loop, or return to quit function */

quit:
    return 0;
}
