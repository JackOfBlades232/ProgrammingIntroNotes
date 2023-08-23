/* adt/copy_constructor.cpp */
#include <stdio.h>

/*
 * If a class allocates memory on construction, and deallocates on
 * destruction, then simple bitwise copies may lead to bad results --
 * if we pass the object to a function by value, a copy is created, and
 * on destruction the copy frees the memory, rendering the remaining copy
 * invalid. Thus we need to write our own copy logic, here -- deepcopy
 */

class Cls1 {
    int *p;

public:
    Cls1() { p = new int[20]; }
    // A copy constructor is a constructor with an arg of ref to same type obj
    // Again, here explicit keyword can be used to restrict copy-initialization
    // A constructor that takes a same-class object by value is not allowed,
    // since it would inifintely call itself on parameter passing
    Cls1(const Cls1 &a) {
        p = new int[20];
        for (int i = 0; i < 20; i++)
            p[i] = a.p[i];
    }
    ~Cls1() { delete [] p; }

    void Print() const {
        for (int i = 0; i < 20; i++)
            printf("%d ", p[i]);
        putchar('\n');
    }

    // Other methods
};

void f(Cls1 c)
{
    printf("In func: ");
    c.Print();
}

int main()
{
    Cls1 c;
    printf("Init: ");
    c.Print();
    f(c);
    printf("After: ");
    c.Print();
    return 0;
}
