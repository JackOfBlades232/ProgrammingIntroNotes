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
