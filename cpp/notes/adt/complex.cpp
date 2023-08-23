/* adt/complex.cpp */
#include <stdio.h>
#include <math.h>

#define USE_IMRE 1

#if USE_IMRE == 1
// In cpp, structs are types, thus no typedef is required
struct str_complex {
    // Protection keywords, used like labels, or so I gather
    // Protection is type-wide: a struct can call private methods on other
    // structs of it's type
private:
    double re, im;

public:
    // Constructors are called on initialization
    str_complex(double a_re, double a_im) { re = a_re; im = a_im; }

    // In OOP, calling a method on an object is viewed as sending a message to it
    
    // In terms of instructions, it is the same as passing the struct as param 1
    // One may get a pointer to itself in a method with the "this" keyword
    double abs() { return sqrt(re*re + im*im); }

    double get_re() { return re; }
    double get_im() { return im; }
    double argument() { return atan2(im, re); }
};
#else
// When hiding the fields, we can rewrite this struct to contain mod/arg
// instead of im/re, and the rest of the program is guaranteed to work
struct str_complex {
private:
    double mod, arg;

public:
    str_complex(double re, double im) {
        mod = sqrt(re*re + im*im);
        arg = atan2(im, re);
    }

    double abs() { return mod; }
    double get_re() { return mod * cos(arg); }
    double get_im() { return mod * sin(arg); }
    double argument() { return arg; }
};
#endif

int main()
{
    // Once there is a constructor, the compiler will only accept 
    // initializations compliant with some constructor
    str_complex z(2.7, 3.8);
    double mod(1.0); // same as double mod = 1.0; Everything has constructors
    // In OOP terms, we have sent z the message "count your abs",
    // and it responded with the value
    mod = z.abs();
    printf("Im: %lf, re: %lf, mod: %lf, arg: %lf\n", 
           z.get_im(), z.get_re(), mod, z.argument());

    return 0;
}
