/* weirdness.c */

double matrix[100][10];

int f(int x, int y) { return x + y; }

/* this is a func that takes 1 int and returnes a funcptr with f's profile */
int (*funret(int x))(int, int)
{
    return &f;
}

/* a little more hardcore */
int (*replace_f(int (*func)(int, int)))(int, int)
{
    return func;
}

/* array pointer returning func */
double (*select_segment(int a, int b))[10]
{
    return matrix;
}

/* now, the sane way */
typedef int (*fptr)(int, int);

int main()
{
    /* array of f-profile functions */
    int (*funvec[10])(int, int);
    /* weird shit (pointer to array pointer returning func */
    double (*(*selptr)(int, int))[10];
    /* what the fuck (pointer to replace_f */
    int (*(*replace_f_p)(int (*)(int, int)))(int, int);

    /* same, but sane */
    fptr (*replace_f_ptr)(fptr);

    return 0;
}
