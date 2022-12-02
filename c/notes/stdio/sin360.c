/* sin360.c */
#include <stdio.h>
#include <math.h>

int main()
{
    FILE *f;
    int deg;

    f = fopen("sincos.txt", "w");
    if (!f) {
        perror("sincos.txt");
        return 1;
    }

    for (deg = 0; deg < 360; deg++) {
        double rad, s, c;
        rad = (double)deg * M_PI / 180.0;
        s = sin(rad);
        c = cos(rad);
        fprintf(f, "%03d % 7.5f % 7.5f\n", deg, s, c);
    }

    fclose(f);
    return 0;
}
