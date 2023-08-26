/* polymorphism/other.cpp */

class A {
    // If we made the destructor private, then it would
    // not be possible to allocate it on the stack, only dynamically.
    // If we make it protected, we also allow inheritance 
    // (if destructor is private, children destructors can't call it, 
    // thus children can not be compiled).
    // This may be used with defining a public method for deallocation
    ~A() {}

public:
    void Deallocate() { delete this; }
};

class B {
protected:
    int x;
public:
    void f(int a, int b) {}
    // For dynamic_cast to have a vmtp
    virtual void bruh() {}
};

// If the child class contains something (method or field), with the same
// name, as something in the base class, the base class thing gets hidden
// (irrespective of profile)
class C: public B {
public:
    //void f(int a, int b) {}
    //double f;
    double f(const char *str) { return 1.0; }
};

class D : public B { };

int main()
{
    C c;
    double t = c.f("abra"); // legal;
    // c.f(2, 3) -- no can do, it is hidden;
    
    // but hidden != removed, one can call with class namespace
    c.B::f(2, 3);

    // Explicitly specifying class namespace also bypasses virtual func
    // mechanism. This may be useful if you want to call base version from
    // child version

    // About explicit type casting: in C (...) can do a lot of things -- 
    // remove constness, for example.
    // In C++ there are also operators for restricting cast capabilities,
    // for greater safety

    int a = 12;
    const int *p = &a;
    int *q;
    // const cast can only add/remove const and volatile stuff
    q = const_cast<int *>(p);

    D d;
    B *idp = &d;
    D *dp;
    // static cast can cast pointers down the heirarchy, casting parents
    // to children (you should be sure, that the child is really in mem)
    dp = static_cast<D *>(idp);

    // reinterpret cast is just like C cast, but it can not change const
    // modifier. It also has other limitations -- it can do
    // only p-t-p, r-t-p, p-t-r, i-t-p, p-t-i conversions
    // (no floats). Seems dumb and unusable, but i might change my mind and
    // use it for pointers
    int xx = 1234;
    int dd = reinterpret_cast<int&>(xx);

    // Same as static, but it will actually check if such an object is in
    // mem at runtime (via vtable pointer, since each class has a
    // distinct and known vtable), and return NULL if not.
    // Therefore, dynamic_cast requires vtables to exist, i.e. vfuncs to
    // exist
    dp = dynamic_cast<D *>(idp);
    // Sounds overheady and slow

    // It is recommended to use cast operators instead of C ones, since
    // they are safer and clearly indicate intention

    return 0;
}
