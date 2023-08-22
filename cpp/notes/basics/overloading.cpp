/* basics/overloading.cpp */
#include <stdio.h>

// In cpp a function with the same name can be defined multiple times in
// one namespace with different parameters (not return value)
void print(int n) { printf("%d\n", n); }
void print(const char *s) { printf("%s\n", s); }
void print() { printf("Hello, world!\n"); }

/* There are problems with this function. In object code, these funcs will
 * have deformed names, since they have to have different assembly labels.
 * example: "_Z5printi" -- _Z is a prefix, 5 for name len, print and i for int
 * This may cause problems when linking agains C modules -- these would
 * have normal labels, and C++ modules with C headers included may mangle
 * the names, and thus will not be able to link _Z5printi with print, 
 * leading to a linker error
 *
 * Note: if the function takes user defined types or is template-instantiated,
 * the generated labels are truly horrifying
 *
 * Furthermore, different C++ compilers may have different naming rules, 
 * thus it may not be safe to link modules built by different compilers,
 * although this has become better.
 *
 * Here is a remedy:
 */

// This, among other things, restricts overloading and makes the
// label the same as the function name, thus making this header linkable
extern "C" {

double foo(double a, int b); 

}

// One may even do this 
extern "C" {
#include <stdlib.h>
}

// And for C headers that may be used with C++, one can do this:
#ifdef __cplusplus
extern "C" {
#endif

double bar(int a, double b);

#ifdef __cplusplus
}
#endif
// The __cplusplus symbol is defined in every C++ compiler, thus we use
// this feature only when needed

int main()
{
    print(50);
    print("Have a nice day");
    print();
    // print(NULL) would be invalid, since 0 is both a NULL char* and an int
    // and the compiler would throw an error. It seems to understand print(0)
    // though
}
