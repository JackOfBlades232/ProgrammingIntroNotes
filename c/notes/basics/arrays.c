int main()
{
    int m[20]; /* really not an array, can only access elements, m is actually
                  int* */
    int *p;
    int size, diff;

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
}
