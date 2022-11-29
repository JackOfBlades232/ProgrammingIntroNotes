#include <stdio.h>
#include <stdlib.h>
#include <math.h>

int main()
{
    double *k;
    k = malloc(360*sizeof(double)); /* void *malloc(int size) */

    /* fill mem like an array */
    for (int i = 0; i < 360; i++) {
        k[i] = sin((2*M_PI/360.0) * (double)i);
        printf("%.5f ", k[i]);
    }
    printf("\n");
    
    free(k); /* void free(void *p) */
}
