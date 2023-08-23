/* adt/other.cpp */
#include <stdio.h>

// The function call operator can be overloaded, to create functors,
// or callbacks. This must be a method.

class Fun {
    int id;

public:
    Fun(int a_id = 0) : id(a_id) {}
    void operator()() 
        { printf("fun0\n"); }
    void operator()(int a) 
        { printf("fun1: %d\n", a); }
    void operator()(int a, int b) 
        { printf("fun2: %d %d\n", a, b); }

    // For implicit casts to types, for which you can not define a cast
    // constructor (basic types or some other types which are closed for
    // some reason) we can define cast operator
    operator int() const { return id; }
};

int main()
{
    Fun f(1234);
    f(); f(100); f(25, 36);
    int id;
    id = f;
    printf("Id: %d\n", id);

    return 0;
}
