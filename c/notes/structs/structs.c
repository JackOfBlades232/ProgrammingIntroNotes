/* structs.c */
#include <stdlib.h>

enum { max_name_len = 64, max_group_len = 8 };

/* like enums, struct name is not mandatory, can be anon (when decl var) */
struct student {
    char name[max_name_len];
    char sex; /* 'm' or 'f' */
    int year_of_birth;
    int major_code;
    int year;
    char group[max_group_len];
    float average;
};

/* can assign exact number of bits for struct fields, this is not a regular
 * struct, this is the only way to split a machine word into bits, not good
 * to use with regular fields. This struct will be of uns-int size, still 4 
 * bytes */
struct flags {
    unsigned io_error:1;
    unsigned seen_a_digit:1;
    unsigned had_a_eol:1;
    unsigned signaled:1;
    unsigned count:4;
};

int main()
{
    /* structs may take more mem than sum of fields, because in some arch-s
     * it is impossible to pack some values tightly, so compiler adds
     * padding bytes. The only way to know the size is sizeof. */
    struct student s1 = {
        "Kiyashko Petr Sergeevich",
        'm', 2001, 0xAA, 4, "A1", 8.8f
    };
    struct student *ptr;

    s1.year = 5;
    ptr = malloc(sizeof(struct student));
    ptr->sex = 'f'; /* === (*ptr).sex = 'f' */

    return 0;
}
