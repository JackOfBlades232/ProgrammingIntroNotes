/* templates/other.cpp */
#include <stdio.h>

// One could also use parametrized specialization, when the specialization
// still has some params

// Only classes allow parametrized spec, not functions

template <class T, class S>
class Cls { };

// Simple case -- specializing some of the params, but not all
template <class X>
class Cls<X, int> { };

// Another case: modifying the type (
template <class A, class B>
class Cls<A *, B &> { };

int main()
{
    return 0;
}
