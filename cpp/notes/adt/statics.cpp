/* adt/statics.cpp */
#include <stdio.h>

class Cls {
public:
    // class-wide persistent member, restricted to the class namespace
    // like globals, if in module should be inited in module (like extern)
    static int static_field;
    // Same with method (is always a friend to this class objects)
    static int IncStaticField(int amount) { return ++static_field; }
};

// In the class is just a decl w-out mem, sill has to be inited outside;
int Cls::static_field = 0;

// Static methods may be used in conjunction with prohibiting constructors
// as creation methods

int main()
{
    Cls c;
    c.static_field = 15; // ok
    Cls::static_field = 15; // also ok
    printf("%d\n", c.IncStaticField(4));                            
    printf("%d\n", Cls::IncStaticField(6));                            
    return 0;
}
