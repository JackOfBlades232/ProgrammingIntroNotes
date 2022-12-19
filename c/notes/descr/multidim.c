/* multidim.c */

int main ()
{
    int i, j;
    /* multidim arrays, stored linearly */
    int m[3][4] = 
        {
            {0, 0, 0, 0},
            {0, 1, 2, 3},
            {0, 2, 6, 4}
        };
    /* here, m is a special type -- array pointer, so that m[1] offsets it by
     * row lenght, and not by a byte. Now rows are just int* m is the 
     * address of a 4-element int array. */
    int (*p)[4]; /* p is of same type */
    int *q[4]; /* and q is an array of int pointers */
    /* here, same, +1 offsets us by 15x20x4=1200 bytes */
    int z[10][15][20];
    int (*zptr)[15][20];

    for (i = 0; i < 3; i++)
        for (j = 0; j < 4; j++)
            m[i][j] = i * j;
    
    return 0;
}
