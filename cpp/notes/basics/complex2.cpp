/* basics/complex2.cpp */
#include <stdio.h>
#include <string.h>
#include <math.h>

// The only difference between classes and structs -- the methods and
// members of a class are private by default
// Everything before the public keyword is private
class Complex {
    double re, im;

public:
    Complex(double a_re, double a_im) { re = a_re; im = a_im; }

    // The default constructor is required for arrays and simple declarations
    // once at least one other constructor is defined
    Complex() { re = 0; im = 0; }

    // If we defined the constructor as
    // Complex(double a_re = 0, double a_im = 0) { re = a_re; im = a_im; }
    // Then it would act as the default constructor, the cast from double
    // constructor and a regular constructor at the same time

    // Every constructor with one param of some other type is treated as
    // a casting constructor, which is used for implicit casting
    Complex(double a) { re = a; im = 0; }
    // And the explicit keyword restricts implicit casting
    explicit Complex(const char *s) { re = strlen(s); im = -re; }

    // We mark all methods as const, restricting them changing members, and
    // thus allowing these methods to be called on const objects
    // (regular methods cant be called on const Complex, const Complex */&)
    // If a method does not mutate the object, it is best to make it const
    //
    // In regular methods this has type Complex *, and in const --
    // const Complex *, thus regular methods also can not be called from
    // const ones

    double GetRe() const { return re; }
    double GetIm() const { return im; }
    double Modulo() const { return sqrt(re*re + im*im); }
    double Argument() const { return atan2(im, re); }

    // Operator overloading is also allowed, for almost all operators
    // (except the ternary operator ?: and the . operator)
    // For bigger data structures passing by const reference saves copies
    Complex operator+(const Complex &op2) const 
        { return Complex(re + op2.re, im + op2.im); }
    Complex operator-(const Complex &op2) const
        { return Complex(re - op2.re, im - op2.im); }
    Complex operator*(const Complex &op2) const 
        { return Complex(re*op2.re - im*op2.im, re*op2.im + im*op2.re); }
    Complex operator/(const Complex &op2) const {
        double dvs = op2.re*op2.re + op2.im*op2.im;
        Complex res((re*op2.re + im*op2.im)/dvs,
                    (im*op2.re - re*op2.im)/dvs);
        return res;
    }

    void Print() const { printf("%.2lf + %.2lfi\n", re, im); };
};

// Usually struct are used for c-style open structs with some methods (maybe),
// and classes are used when employing OOP/ADT

int main()
{
    Complex c1(2.7, 3.8);
    printf("(w/class) Im: %lf, re: %lf, mod: %lf, arg: %lf\n", 
           c1.GetIm(), c1.GetRe(), c1.Modulo(), c1.Argument()); 

    // Now we can use additions to get another complex value
    Complex c2(1.15, -7.1);
    printf("Sum modulo: %lf, (and) %lf\n", 
           (c1 + c2).Modulo(), c1.operator+(c2).Modulo());

    // This can only be done if the default constructor is defined
    // (if no other constructors are defined, it is automatically generated)
    Complex sum;
    sum = c1 + c2;
    // The default constructor also enables arrays (here, it is called 50 times)
    Complex v[50];

    // This is enabled by the double->Complex cast constructor
    printf("Modulo of sum of c2 and 9.7: %lf\n", (c2 + 9.7).Modulo());

    const Complex *sp = &sum;
    printf("Sum modulo: %lf\n", sp->Modulo()); // Modulo must be const method

    // Since malloc and free can't call constructors and destructors, they
    // can not be used for object allocation, thus there are new and delete
    Complex *p;
    p = new Complex(2.4, 7.12);
    p->Print();
    delete p;

    // Also works for arrays, but these are different operations,
    // not to be mixed
    Complex *arr = new Complex[100];
    delete [] arr;

    // One should also not mix new/delete with malloc/free, though with some
    // compilers this would not be a problem

    // Anonymous and temporary objects can be created in expressions;
    Complex t = c1 * Complex(0, 1); // anonymous object (created by code)
    printf("(1) t = ");
    t.Print();
    t = t + c1 + c2;                // temp objects (interm expression results)
    printf("(2) t = ");
    t.Print();
    // To disallow all the temp objects slowing the thing down, the compiler
    // relies on inlining

    // Temp objects exist while the statement is alive (if it is a func
    // param, it lives in the function, if it is in an expression, it lives
    // while it is calculates, and if it is used to init a reference, it
    // lives while the reference lives)
    // It also follows, that refs to temp/anon objects must be const,
    // since temp objects should not be mutable
    // Thus const should also be used with refs where possible

    return 0;
}
