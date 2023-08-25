/* 10_2.cpp */
#include <stdio.h>

class B {
    int val;

public:
    B(int a_val) : val(a_val) {}
    const B &operator+=(B other) { val += other.val; return *this; }
    int Get() const { return val; }
};

int main()
{
    B first(1), second = 2;
    first += 10; second += 1000;
    printf("%d %d\n", first.Get(), second.Get());
    return 0;
}
