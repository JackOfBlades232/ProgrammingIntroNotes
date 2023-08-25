/* 10_8.cpp */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef long long ll;

#define ABS(_a) ((_a) >= 0 ? (_a) : -(_a))
#define SGN(_a) ((_a) > 0 ? 1 : ((_a) < 0 ? -1 : 0))

// Just a container used for it's destructor
class StringCont {
    char *str;

public:
    StringCont(char *a_str) : str(a_str) {}
    ~StringCont() { free(str); }
    const char *Str() const { return str; }
};

class Rational {
    ll numerator;
    ll denominator;

public:
    // Should not allow denominator <= 0
    Rational(ll a_numerator = 0, ll a_denominator = 1)
        : numerator(a_numerator), denominator(a_denominator) 
        { if (denominator <= 0) { fprintf(stderr, "Denom<=0\n"); exit(1); } }

    ll Numerator() const { return numerator; }
    ll Denominator() const { return denominator; }

    operator double() const { return (double)numerator/denominator; }
    operator ll() const { return numerator/((ll)denominator); } 
    ll Round() const;

    StringCont ToString() const;

    const Rational &operator+=(const Rational &op);
    const Rational &operator-=(const Rational &op);
    const Rational &operator*=(const Rational &op);
    const Rational &operator/=(const Rational &op);

    friend Rational operator+(const Rational &r1, const Rational &r2);
    friend Rational operator-(const Rational &r1, const Rational &r2);
    friend Rational operator*(const Rational &r1, const Rational &r2);
    friend Rational operator/(const Rational &r1, const Rational &r2);
};

ll Rational::Round() const
{
    ll whole = (ll)(*this);
    double fr = (double)(*this) - whole; 
    return whole + (ABS(fr) >= 0.5 ? SGN(fr) : 0);
}

StringCont Rational::ToString() const
{
    size_t nchars = snprintf(NULL, 0, "%lld/%lld", numerator, denominator);
    char *str = (char *)malloc((nchars+1) * sizeof(*str));
    sprintf(str, "%lld/%lld", numerator, denominator);
    return StringCont(str);
}

const Rational &Rational::operator+=(const Rational &op)
{
    numerator = numerator*op.denominator + op.numerator*denominator;
    denominator = denominator*op.denominator;
    return *this;
}

const Rational &Rational::operator-=(const Rational &op)
{
    numerator = numerator*op.denominator - op.numerator*denominator;
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
    denominator = op.numerator*denominator;
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
    ll numerator = r1.numerator * r2.denominator;
    ll denominator = r2.numerator * r1.denominator;
    if (denominator < 0) {
        numerator *= -1;
        denominator *= -1;
    }
    return Rational(numerator, denominator);
}

int main()
{
    Rational r1;
    Rational r2(-32);
    Rational r3(15, 6);
    Rational r4(102.3421);

    printf("r1() = %s\n", r1.ToString().Str());
    printf("r2(-32) = %s\n", r2.ToString().Str());
    printf("r3(15, 6) = %s\n", r3.ToString().Str());
    printf("r4(102.3421) = %s\n", r4.ToString().Str());

    putchar('\n');

    printf("r3.Numerator() = %lld, r3.Denominator() = %lld\n",
           r3.Numerator(), r3.Denominator());
 
    putchar('\n');

    printf("(double)r1 = %lf\n", (double)r1);
    printf("(double)r2 = %lf\n", (double)r2);
    printf("(double)r3 = %lf\n", (double)r3);

    Rational r5 = Rational(-9, 31);
    printf("(double)(%s) = %lf\n", r5.ToString().Str(), (double)r5);

    putchar('\n');

    Rational r6 = Rational(17, 6);
    printf("(long long)r1 = %lld\n", (ll)r1);
    printf("(long long)r2 = %lld\n", (ll)r2);
    printf("(long long)r3 = %lld\n", (ll)r3);
    printf("(long long)(%s) = %lld\n", r5.ToString().Str(), (ll)r5);
    printf("(long long)(%s) = %lld\n", r6.ToString().Str(), (ll)r6);

    putchar('\n');

    printf("round(r3) = %lld\n", r3.Round());
    printf("round(%s) = %lld\n", r5.ToString().Str(), r5.Round());
    printf("round(%s) = %lld\n", r6.ToString().Str(), r6.Round());

    putchar('\n');

    printf("%s + %s = %s\n", r5.ToString().Str(), r6.ToString().Str(), 
           (r5+r6).ToString().Str());
    printf("%s + %s = %s\n", 
           Rational(22, 10).ToString().Str(), Rational(-13, 8).ToString().Str(),
           (Rational(22, 10) + Rational(-13, 8)).ToString().Str());
    printf("%s + %s = %s\n", 
           Rational(-1, 3).ToString().Str(), Rational(1, 27).ToString().Str(),
           (Rational(-1, 3) + Rational(1, 27)).ToString().Str());

    putchar('\n');

    printf("%s - %s = %s\n", 
           r5.ToString().Str(), Rational(2, 5).ToString().Str(), 
           (r5 - Rational(2, 5)).ToString().Str());
    printf("%s - %s = %s\n", 
           Rational(0, 7).ToString().Str(), Rational(-33, 412).ToString().Str(),
           (Rational(0, 7) - Rational(-33, 412)).ToString().Str());
    printf("%s - %s = %s\n", 
           Rational(2, 2).ToString().Str(), Rational(33, 33).ToString().Str(),
           (Rational(2, 2) - Rational(33, 33)).ToString().Str());

    putchar('\n');

    printf("%s * %s = %s\n", 
           r5.ToString().Str(), Rational(2, 5).ToString().Str(),
           (r5 * Rational(2, 5)).ToString().Str());
    printf("%s * %s = %s\n", 
           Rational(0, 7).ToString().Str(), Rational(-33, 412).ToString().Str(),
           (Rational(0, 7) * Rational(-33, 412)).ToString().Str());
    printf("%s * %s = %s\n", 
           Rational(-1, 3).ToString().Str(), Rational(1, 27).ToString().Str(),
           (Rational(-1, 3) * Rational(1, 27)).ToString().Str());

    putchar('\n');

    printf("%s / %s = %s\n", r5.ToString().Str(), r6.ToString().Str(), 
           (r5/r6).ToString().Str());
    printf("%s / %s = %s\n", 
           Rational(22, 10).ToString().Str(), Rational(-13, 8).ToString().Str(),
           (Rational(22, 10) / Rational(-13, 8)).ToString().Str());
    printf("%s / %s = %s\n", 
           Rational(2, 2).ToString().Str(), Rational(33, 33).ToString().Str(),
           (Rational(2, 2) / Rational(33, 33)).ToString().Str());

    Rational r(0);
    printf("r = %s\n", r.ToString().Str());
    printf("r += %s = %s\n", Rational(13, 46).ToString().Str(),
           (r += Rational(13, 46)).ToString().Str());
    printf("r -= %s = %s\n", Rational(122, 55).ToString().Str(),
           (r -= Rational(122, 55)).ToString().Str());
    printf("r *= %s = %s\n", Rational(8, 3).ToString().Str(),
           (r *= Rational(8, 3)).ToString().Str());
    printf("r /= 7 = %s\n", (r /= 7).ToString().Str());

    return 0;
}
