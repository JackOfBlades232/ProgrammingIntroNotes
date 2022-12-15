/* unions.c */
#include <stdio.h>

/* a union only holds one value at a time -- all fields have same adr,
 * assiginig a new value overrides th prev one (union -- set of values is
 * a union. Size = max(field size) */
union sample_un {
    int i;
    double k;
    char str[16];
};

/* usage -- keeping byte split of a number */
union split_int {
    int integer;
    unsigned char bytes[sizeof(int)];
};

/* also used for keeping untyped data and instructions for reading it */
enum expt_item_types /* values for c */
    { eit_int = 0, eit_dbl = 0, eit_var = 2, eit_min_op = ' ' };

struct expression_item {
    char c;
    union un_data {
        int i;
        double d;
        char var[sizeof(double)];
    } data; /* if you don't specify name, union still works, 
               as fields of struct */
    struct expression_item *next;
};

int main()
{
    union sample_un su;
    int i;
    union split_int si;

    su.i = 12;

    printf("Enter an integer: ");
    scanf("%d", &si.integer);
    for (i = 0; i < sizeof(int); i++)
        printf("byte #%d is %02x\n", i, si.bytes[i]);

    return 0;
}
