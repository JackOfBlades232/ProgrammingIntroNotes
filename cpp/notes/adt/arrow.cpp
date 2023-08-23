/* adt/arrow.cpp */
#include <stdio.h>

struct s1 {
    int a, b;
};

class Pointer_s1 {
    s1 *p;

public:
    Pointer_s1(s1 *ptr = 0) : p(ptr) {}
    ~Pointer_s1() { if (p) delete p; }
    s1 *operator=(s1 *ptr) {
        if (p)
            delete p;
        p = ptr;
        return p;
    }

    // Ref cause we want it to be an lvalue also
    s1 &operator*() const { return *p; }
    // overloaded -> must return a field, such that it either has an ->
    // overloaded, or is a pointer with this field (thus, it can be used
    // to "reach down" for a field)
    s1 *operator->() const { return p; } // reaches into p for the field

private:
    // Restricting copy and assignment
    Pointer_s1(const Pointer_s1 &) {}
    void operator=(const Pointer_s1 &) {}
};

int main()
{
    // This pointer can be recreated, and will be auto-freed at the end
    // of scope
    Pointer_s1 p;
    p = new s1;
    p->a = 25;
    p->b = p->a + 36;
    printf("a = %d, b = %d\n", p->a, p->b);

    return 0;
}
