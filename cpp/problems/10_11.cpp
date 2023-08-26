/* 10_11.cpp */
#include <stdio.h>
#include <math.h>

class Body {
    double desity;

public:
    Body(double a_density) : desity(a_density) {}
    virtual ~Body() {}
    virtual double Volume() const = 0;
    double Mass() const { return desity * Volume(); }
};

class Cube : public Body {
    double a;

public:
    Cube(double a_a, double a_density)
        : Body(a_density), a(a_a) {}
    virtual ~Cube() {}
    virtual double Volume() const { return a*a*a; }
};

class Tetrahedron : public Body {
    int a;

public:
    Tetrahedron(double a_a, double a_density)
        : Body(a_density), a(a_a) {}
    virtual ~Tetrahedron() {}
    virtual double Volume() const { return (M_SQRT2/12.0) * (a*a*a); }
};

int main()
{
    const Body *p, *q, *r;
    Cube a(2, 10), b(5, 0.1);
    Tetrahedron t(6, 2.5);
    p = &a; q = &b; r = &t;
    printf("Volumes: %3.3lf %3.3lf %3.3lf\n",
            p->Volume(), q->Volume(), r->Volume());
    printf("Masses: %3.3lf %3.3lf %3.3lf\n",
            p->Mass(), q->Mass(), r->Mass());
    return 0;
}
