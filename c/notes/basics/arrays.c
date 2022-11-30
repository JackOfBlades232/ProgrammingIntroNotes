#include <stdio.h>

int main()
{
    int m[20]; /* really not an array, can only access elements, m is actually
                  int* */
    int *p;
    int size, diff;

    /* can init array with compile-time consts, 
     * then specifying size is optional */
    int primes[] = { 2, 3, 5, 7, 11, 13, 17, 19, 21, 23 };
    /* when initting arrays locally, vals will be copied to stack on every call,
     * so be cautious. When array is global, it just goes to .data */

    p = m; /* legitimate, they are really of one type */

    m[1] = 1;
    p[2] = 3; /* can do both now */ 
    *m = 0; /* *m === m[0] */

    *(m+19) = 100; /* m is an adr, so you can apply arifm to it to ger other
                      addresses. I e: *(a+b) = a[b]. Can even do 17[m].
                      The addition is in terms of arr elem size (not bytes) */

    /* the only place where m is not a simple adr (will give arr byte size) */
    size = sizeof(m);

    /* problem with all this -- cant copy-assign arrays (or strings), like in
     * pascal */

    p = &m[13];
    diff = p - m; /* will be integer -- 13 */

    /* how to calc len of initialized arr: */
    for (int i = 0; i < sizeof(primes)/sizeof(*primes); i++)
        printf("[%d] = %d\n", i, primes[i]);
}
