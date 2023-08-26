/* exceptions/throw_catch.cpp */
#include <stdio.h>

struct somestruct { int val = 666; const char *msg = "aaaaa"; };

unsigned int line_count_in_file(const char *file_name)
{
    FILE *f = fopen(file_name, "r");
    // Throwed exception can be any type with a copy-constr and a destructor,
    // and it falls through the call stack until caught. There exists a 
    // style of programming (even with return codes), when all exceptional
    // situations are dealt with in the main function
    if (!f)
        throw "couldn't open the file";
    int n = 0;
    int c = 0;
    while ((c = fgetc(f)) != EOF) {
        if (c == '\n')
            n++;
    }
    fclose(f);

    if (n % 6 == 0)
        throw 666;
    else if (n % 3 == 0)
        throw somestruct();
    return n;
}

// use of catch(...): free resources and throw further down
void f(int n)
{
    int *p = new int[n];
    try {
        // Do smth
    }
    catch(...) {
        delete [] p;
        throw;
    }

    // there is good news though: when throwing all falling through,
    // all local objects' destructors get called, aka automatic cleaning

    delete [] p;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Args: <filename>\n");
        return 1;
    }

    // If I never catch, it is a crash
    // Catching works for funcs in try block, and if caught, goes to
    // catch block with exact type (there can be may catch blocks for 
    // different types)
    // If an exception is not caught by any catch block, it continues
    // it's descent down the call stack
    try {
        printf("Lines in file: %d\n", line_count_in_file(argv[1]));
    } catch (const char *ex) {
        fprintf(stderr, "Exception (string): %s\n", ex);
        return 2;
    } catch (int n) {
        fprintf(stderr, "Exception (int): %d\n", n);
        return 3;

        throw; // this throws the last caught exception further down
    } catch (...) { // catches any (other) type
        fprintf(stderr, "There's something wrong! I should've known\n");
        return 4;

    }

    // The types can be cast when catching, but only in a few ways
    // (normal casting will not work):
    // T -> T &; T -> const T (not the other way round); 
    // and some stuff with inheritance

    f(123);

    return 0;
}
