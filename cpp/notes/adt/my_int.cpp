/* adt/my_int.cpp */
#include <stdio.h>

// ++, --, pre and post are defined as follows:
// operator++/-- for prefix form
// operator++/--(int) for suffix (a fictive type for overloading)
// Both may be methods or global funcs with one arg
// usually return void, val or const ref

class MyInt {
    int i;
public:
    MyInt(int x) : i(x) {}
    const MyInt &operator++() { i++; return *this; }
    const MyInt &operator--() { i--; return *this; }
    MyInt operator++(int) 
        { MyInt tmp(*this); i++; return tmp; }
    MyInt operator--(int) 
        { MyInt tmp(*this); i--; return tmp; }

    void Print(const char *pref) const { printf("%s%d\n", pref, i); }
};

int main()
{
    MyInt n(0);
    (++n).Print("++0 = ");
    (--n).Print("--1 = ");
    (n++).Print("0++ = ");
    (n--).Print("1-- = ");
    return 0;
}
