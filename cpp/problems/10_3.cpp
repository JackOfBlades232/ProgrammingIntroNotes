/* 10_3.cpp */
#include <stdio.h>

class D {
    int val;

public:
    D() : val(0) {}
    D(const D &other) : val(other.val+1) {}
    int Get() const { return val; }
};

int main()
{
    D x;
    D y(x);
    D z = y;
    printf("%d %d %d\n", x.Get(), y.Get(), z.Get());
    return 0;
}
