/* templates/sort.cpp */
#include <stdio.h>
#include <string.h>

// Templated func is instantianed in many copies in the code, depending on
// which versions were called. class here means "any type"
// This is basically eases what in C is done via multi-line macros

// Funcrtion for main sort template
template <class T>
inline bool is_less(T a, T b)
{
    return a < b;
}

// When no params are passed, this is considered a special-case template
// The function requires all main template params, though they can
// be inferred
template <>
bool is_less<const char *>(const char *a, const char *b)
{
    return strcmp(a, b) < 0;
}
// This version sorts string arrays in alphabetic order

template <class T>
void sort(T *array, int len)
{
    for (int start = 0; ; start++) {
        bool done = true;
        for (int i = len-2; i >= start; i--) {
            if (is_less(array[i+1], array[i])) {
                T tmp = array[i];
                array[i] = array[i+1];
                array[i+1] = tmp;
                done = false;
            }
        }
        if (done)
            break;
    }
}

// Here, sort is not a function. It is a template for functions to be 
// instantiated by the compiler

int main()
{
    int a[] = { -1, 123, -123, 0, 4, 1, 7, 9, 10, -3 };
    const int alen = sizeof(a)/sizeof(*a);

    sort<int>(a, alen);

    for (int i = 0; i < alen; i++)
        printf("%d ", a[i]);
    putchar('\n');

    double b[] = { 0.12, 0.01, -1.23, 77.7, 10.9, 3.14, 2.2222 };
    const int blen = sizeof(b)/sizeof(*b);

    // For function templates, type can be inferred, thus omitted
    sort(b, blen);

    for (int i = 0; i < blen; i++)
        printf("%lf ", b[i]);
    putchar('\n');

    // Our special case will help us work with strings -- the sort template
    // is the same, but it uses the spec-case is_less template
    const char *sa[] = { "asdjklasn", "lckxnvbksvn", "9123alksn", "+sadak" };
    const int salen = 4;
    
    sort(sa, salen);

    for (int i = 0; i < salen; i++)
        printf("%s ", sa[i]);
    putchar('\n');

    return 0;
}
