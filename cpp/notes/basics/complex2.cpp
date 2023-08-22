/* basics/complex2.cpp */
#include <stdio.h>
#include <math.h>

// The only difference between classes and structs -- the methods and
// members of a class are private by default
// Everything before the public keyword is private
class Complex {
    double re, im;

public:
    Complex(double a_re, double a_im) { re = a_re; im = a_im; }

    double GetRe() { return re; }
    double GetIm() { return im; }
    double Abs() { return sqrt(re*re + im*im); }
    double Argument() { return atan2(im, re); }
};

// Usually struct are used for c-style open structs with some methods (maybe),
// and classes are used when employing OOP/ADT

int main()
{
    Complex zc(2.7, 3.8);
    printf("(w/class) Im: %lf, re: %lf, mod: %lf, arg: %lf\n", 
           zc.GetIm(), zc.GetRe(), zc.Abs(), zc.Argument()); 

    return 0;
}
