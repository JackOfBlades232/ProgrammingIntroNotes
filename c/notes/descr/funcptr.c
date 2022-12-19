/* funcptr.c */

double dbl_sum(const double *a, int size)
{
    return size > 0 ? *a + dbl_sum(a+1, size-1) : 0;
}

double dbl_min(const double *a, int size)
{
    double d;
    if (size == 1)
        return *a;
    d = dbl_min(a+1, size-1);
    return *a < d ? *a : d;
}

double dbl_average(const double *a, int size)
{
    return dbl_sum(a, size) / (double) size;
}

int main()
{
    double (*fptr)(const double *, int); 
    double arr[] = { 2.0, 3.0, 1.0 };
    double res;
    /* func ptr, that can be used with all these functions. parens are 
     * required, otherwise it would be a declaration of a func returning
     * double*. */

    /* can do both */
    fptr = dbl_sum;
    fptr = &dbl_min;

    res = (*fptr)(arr, sizeof(arr)/sizeof(*arr));
    res = fptr(arr, sizeof(arr)/sizeof(*arr));

    return 0;
}
