/* 10_8.cpp */
#include <stdio.h>

typedef long long ll;
typedef unsigned long long ull;

class Rational {
    ll numerator;
    ull denominator;

    static char string_repr[1024];

public:
    // Should not allow denominator=0
    Rational(ll a_numerator = 0, ull a_denominator = 1)
        : numerator(a_numerator), denominator(a_denominator) {}

    ll Numerator() { return numerator; }
    ull Denominator() { return denominator; }

    operator double() { return (double)numerator/denominator; }
    operator ll() { return (double)(*this); }

    ll floor() { return numerator/denominator; } 

    // Not to be assigned, for use in printf (once per call)
    const char *AsString();

    const Rational &operator+=(const Rational &op);
    const Rational &operator-=(const Rational &op);
    const Rational &operator*=(const Rational &op);
    const Rational &operator/=(const Rational &op);

    friend Rational operator+(const Rational &r1, const Rational &r2);
    friend Rational operator-(const Rational &r1, const Rational &r2);
    friend Rational operator*(const Rational &r1, const Rational &r2);
    friend Rational operator/(const Rational &r1, const Rational &r2);
};

char Rational::string_repr[1024] = { 0 };

const char *Rational::AsString()
{
    snprintf(string_repr, sizeof(string_repr), 
             "%lld/%lld", numerator, denominator);
    return string_repr;
}

const Rational &Rational::operator+=(const Rational &op)
{
    numerator = numerator*op.denominator + denominator*op.numerator;
    denominator = denominator*op.denominator;
    return *this;
}

const Rational &Rational::operator-=(const Rational &op)
{
    numerator = numerator*op.denominator - denominator*op.numerator;
    denominator = denominator*op.denominator;
    return *this;
}

const Rational &Rational::operator*=(const Rational &op)
{
    numerator = numerator*op.numerator;
    denominator = denominator*op.denominator;
    return *this;
}

const Rational &Rational::operator/=(const Rational &op)
{
    numerator = numerator*op.denominator;
    denominator = denominator*op.numerator;
    return *this;
}

Rational operator+(const Rational &r1, const Rational &r2)
{
    return Rational(r1.numerator*r2.denominator + r2.numerator*r1.denominator,
                    r1.denominator * r2.denominator);
}

Rational operator-(const Rational &r1, const Rational &r2)
{
    return Rational(r1.numerator*r2.denominator - r2.numerator*r1.denominator,
                    r1.denominator * r2.denominator);
}

Rational operator*(const Rational &r1, const Rational &r2)
{
    return Rational(r1.numerator * r2.numerator,
                    r1.denominator * r2.denominator);
}

Rational operator/(const Rational &r1, const Rational &r2)
{
    return Rational(r1.numerator * r2.denominator,
                    r1.denominator * r2.numerator);
}

int main()
{
    Rational r1;
    Rational r2(-32);
    Rational r3(15, 6);
    Rational r4(102.3421);
    printf("r1() = %s\n", r1.AsString());
    printf("r2(-32) = %s\n", r2.AsString());
    printf("r3(15, 6) = %s\n", r3.AsString());
    printf("r4(102.3421) = %s\n", r4.AsString());

    printf("r3.Numerator() = %lld, r3.Denominator() = %lld\n",
           r3.Numerator(), r3.Denominator());

    // @TODO: full demostration-test coverage

    return 0;
}
