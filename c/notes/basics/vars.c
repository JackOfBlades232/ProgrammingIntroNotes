#include <stdio.h>

int my_func(int x, int y)
{
    int tt = 25; /* normal vars in stack, freed when their scope ends */
    static int zz = 75; /* statics are local but seen from all program from def
                           to end, go to .data or .bss respectively */

    return 1;
}

/* this func will print next number every call (statics dont get redefined) */
void print_next_number()
{
    static int n = 0;
    printf("%d\n", n);
    n++;
}

int main()
{
    int i;

    for (i = 0; i < 5; i++) {
        print_next_number(); 
    }

    return 0;
}
