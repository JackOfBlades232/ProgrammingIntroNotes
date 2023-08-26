/* polymorphism/struct_inheritance.cpp */
#include <stdio.h>
#include <string.h>

struct person {
    char name[64];
    char sex; // 'm' or 'f'
    int year_of_birth;
};

// This struct will also have the fields of person, with no
// performance hit too. In memory ancestor fields precede new ones,
// Thus c++ allows implicit casting of pointers student * -> person *
// (not the other way around)
struct student : person {
    int code;
    int year;
    float gpa;
};

void print_person_data(person &pers)
{
    printf("Person, name: %s; sex: %c; year of birth: %d\n",
           pers.name, pers.sex, pers.year_of_birth);
}

int main()
{
    student s1;
    strcpy(s1.name, "John Doe");
    s1.sex = 'm';
    s1.year_of_birth = 1989;
    s1.code = 51311;
    s1.year = 2;
    s1.gpa = 4.75;

    printf("Name: %s; sex: %c; year of birth: %d; "
           "code: %d; year: %d; GPA: %lf\n",
           s1.name, s1.sex, s1.year_of_birth,
           s1.code, s1.year, s1.gpa);

    // These are legal cause implicit casting is allowed here
    person *p = &s1;
    person &ref = s1;
    print_person_data(s1);

    // Not freeing name exlicitly
    return 0;
}
