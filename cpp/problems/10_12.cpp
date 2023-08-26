/* 10_12.cpp */
#include <stdio.h>

class Prism {
    double height;

public:
    Prism(double a_height) : height(a_height) {}
    virtual ~Prism() {}
    virtual double Area() const = 0;
    double Volume() const { return height * Area(); }
};

class Box : public Prism {
    double side;

public:
    Box(double a_height, double a_side)
        : Prism(a_height), side(a_side) {}
    virtual ~Box() {}
    virtual double Area() const { return side*side; }
};

class Cube : public Box {
public:
    Cube(double a_side) : Box(a_side, a_side) {}
};

int main()
{
    const Prism *p, *q, *r;
    Box a(0.5, 2), b(5, 0.2);
    Cube c(0.5);
    p = &a; q = &b; r = &c;
    printf("Areas: %3.3lf %3.3lf %3.3lf\n",
           p->Area(), q->Area(), r->Area());
    printf("Volumes: %3.3lf %3.3lf %3.3lf\n",
           p->Volume(), q->Volume(), r->Volume());
}
