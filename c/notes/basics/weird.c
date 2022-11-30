int main()
{
    int m[10];
    int i;
    int a, b;

    i = 0;
    a[i] = i++; /* order is not defined with operations, so we dont know, will
                   0 go to first or second elem */

    a = 1;
    b = (a+=5) + (a*=2); /* this can go terribly different each time */
    /* ; is a sequence point: all side effects are conducted before going on */
    /* && and || are also sequence points, also "?:", ",", and calling of a 
     * function (all args are calculated beforehand */
