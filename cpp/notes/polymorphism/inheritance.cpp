/* polymorphism/inheritance.cpp */
#include <stdio.h>

class A {
    int x, y;

public:
    A() : x(0), y(0) {}
    A(int a_x, int a_y) : x(a_x), y(a_y) {}
    void f() { printf("f\n"); }
    // The code of parent methods (like f) when called to children is
    // literally the same, since implicit casting is used on "this" and
    // the parent fields come before child fields

// These methods/members are open to descendants, given there
// was not a private inheritance in the way
protected:
    int r;
    void g() { printf("g %d %d\n", x++, ++y); }
};

// Public inheritance -- all public methods and members of A are
// accessible through B and protected can be used by descendants of B
class B : public A {
    int z;

public:
    // The way of constructors in inheritance -- init first with passing
    // params. The parent constructor will be called first
    // Destructor of the parent is just implicitly called at the end
    // of child destructor
    B(int a_x, int a_y, int a_z) : A(a_x, a_y), z(a_z) {}
    void h() { f(); g(); printf("h\n\n"); }
};

// Private -- public/protected methods of A only available in B, 
// not outside ot down the hierarchy
class C : private A {
    int z;

public:
    void h() { printf("h\n\n"); }
};

// Struct inheritance is public by default (no keyword), class -- private

int main()
{
    B b(3, 2, 1);
    C c;
    b.h();
    b.f();
    c.h();
    return 0;
}
