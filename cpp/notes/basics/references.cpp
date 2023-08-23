/* basics/references.cpp */
#include <stdio.h>

/*
 * If we consider variable names and dereferenced pointers to be 
 * references (in a semantic sense), then the notion of lvalue becomes
 * equivalent to something being a reference.
 */

// Without references, stars everywhere
void min_max_cstyle(float *arr, int len, float *min, float *max)
{
    int i = 0;
    *min = arr[0];
    *max = arr[0];
    for (i = 1; i < len; i++) {
        if (*min > arr[i])
            *min = arr[i];
        if (*max < arr[i])
            *max = arr[i];
    }
}

// With references -- easier to write, and can just pass the variables 
// themselves, they will be cast to references
void min_max(float *arr, int len, float &min, float &max)
{
    int i = 0;
    min = arr[0];
    max = arr[0];
    for (i = 1; i < len; i++) {
        if (min > arr[i])
            min = arr[i];
        if (max < arr[i])
            max = arr[i];
    }
}

// When returning a reference from a function, we may use it both as a 
// value and as an lvalue for mutating it
float &find_max(float *arr, int len)
{
    int i = 0;
    int max_idx = 0;
    float max_val = arr[0];
    for (i = 1; i < len; i++) {
        if (max_val < arr[i]) {
            max_val = arr[i];
            max_idx = i;
        }
    }
    return arr[max_idx];
}

int main()
{
    int i = 3;    // variable
    int *ip = &i; // pointer to i
    int &r = i;   // Reference to i: created only with initialization by value,
                  // it wraps the pointer to i so that all operations with r
                  // do the same as with i (and affect i instead of r itself)
                  // Therefore, the reference itself is immutable in it's life

    printf("prev %d ", i);
    
    i++;          // increases i by 1
    (*ip)++;      // same
    r++;          // again, same

    printf("new %d\n", i);

    float arr[] = { 0, -1, 2.3, 9.7, -38.49 };
    int len = sizeof(arr)/sizeof(*arr);
    float min, max;

    min_max_cstyle(arr, len, &min, &max);
    printf("(cstyle) Min: %f, Max: %f\n", min, max);

    min_max(arr, len, min, max);
    printf("Min: %f, Max: %f\n", min, max);

    printf("Max+5=%f\n", find_max(arr, len)+5);
    find_max(arr, len) *= -3;
    printf("Max after changing prev one: %f\n", find_max(arr, len));

    // Constness:
    // This is a mutable pointer to constant data
    const char *p;
    p = "A string"; // ok
    // *p = a -- not ok
    // p[5] = 'b' -- not ok

    char buf[20];
    // This is a pointer which is constant itself:
    char * const p1 = buf+5;

    // p1++ -- not ok
    *p1 = 'a'; // ok
    p1[5] = 'b'; // ok
    
    // If you want constant pointer to constant data, here it is:
    const char * const p2 = buf+10;

    // References behave the same
    // Also, a reference to a const must be itself const:
    const int j = 5; // const integer
    // int &jr = j -- not ok
    const int &jcr = j; // ok

    // Const refs are useful for passing big const data to funcs
    // (like const *)

    // The lvalue demostration
    int x = 0;
    int y = 1;
    int z = 28;
    printf("x=%d, y=%d, z=%d\n", x, y, z);
    // C way, need and lvalue
    *(x < y ? &x : &y) = z;
    printf("x=%d, y=%d, z=%d\n", x, y, z);
    // C++ way -- the stuff gets auto-cast to reference
    (x < y ? x : y) = z;
    printf("x=%d, y=%d, z=%d\n", x, y, z);
}
