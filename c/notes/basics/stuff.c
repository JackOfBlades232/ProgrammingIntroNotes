#include <stdio.h>

/* global vars with initializations go to .data, without -- to .bss
    those that go to .bss get filled with zeroes */
int dangerous_global_variable = 42;

char case_up(char c); 

void print_n_chars(char c, int n)
{
    /* local uninited vars still contain junk, will be in stack frame */
    int k;
    for (k = 0; k < n; k++)
        printf("%c", c);
}

char case_up(char c)
{
    if (c >= 'a' && c <= 'z')
        return c - ('a' - 'A');
    else
        return c;
}

void types()
{
    char c; /* 1 byte  (actually, one mem word size) */
    signed char sc; /* regular char can be both signed and uns */
    unsigned char usc; /* all other types are signed by default */
    short s; /* 2 bytes, although on strange computers might be just >char */
    unsigned short int usi; /* strange, but possible */
    int x; /* 4 bytes (used to be 2 bytes on 16-bit architectures) */
    unsigned int ux; /* any integer type can be made unsigned */
    long l; /* 8 bytes (was 4 bytes on 32bit arch), must be able to represent
               addresses */
    long long ll; /* also 64bit, some platforms don't support it */
    /* types sizes, technically, may differ from compiler to compiler */

    float f; /* conventionally, 4 bytes */
    double d; /* 8 bytes */
    long double dd; /* 10 bytes, but in mem might be 12 or 16 even */
    /* all floats are signed, sometimes sizes can also differ, depends on cpu */
    /* all math functions work with doubles, with conversion, so only use for
     * floats is when you need to use less ram for storage */

    printf("%lu\n", sizeof(ll)); /* gets size of variable, a char value */
    printf("%lu\n", sizeof(int)); /* also works for expl types */

    usc = 15; /* simple -- decimal */
    sc = -035; /* with zero -- oct */
    s = 0x1d; /* 0x -- hex */
    l = 0x7E5L; /* can specify type in literal */
    c = 'a'; /* char literals have int type */
    f = 150.0F; /* further -- floating literals, doubles by default */
    d = 15E+2;
    dd = .15e4L;
    d = 1500e-1;
    
    c = '\n'; /* new line (10) */
    c = '\r'; /* cr (13) */
    c = '\t'; /* tab (9) */
    c = '\a'; /* alarm (7) */
    c = '\b'; /* backspace (8) */
    c = '\f'; /* form feed (12) */
    c = '\v'; /* vertical tab (11) */
    c = '\\'; /* just one slash */
    c = '\''; /* unary bracket */
    c = '"'; /* double bracket */

    printf("Never say \"never\"\n"); /* string literals are
                                      char arrs with 0 delim */
    c = '\0'; /* can also specify char by code (oct or hex) */
    printf("split" " " "string\n"); /* string literal can be written split */
}

void expr()
{
    int a, b, x;
    double c, d, y, z, s;
    char p, q, r;
    char *pp;

    a = 5;
    b = 2;
    c = 5.0;
    d = 2.0;
    
    x = 1;
    x = (a < b) && (a > b) || (b != x); /* 0=false, else true, ||&& are lazy */

    p = 1;
    q = 5;
    r = p | (q & p); /* bitwise ops */
    p = ~(-1); /* negation */
    r = p ^ q; /* bitwise xor */
    r = 1 << 4; /* bitwise shifts */
    r = r >> 2; /* if signed, new places get filled with sign byte, not 0s */

    y = (z = 17.0) + 1; /* = is an operation, with value eq to new var value */
    a = b = x + 5; /* this will, obv, fill 2 vars with x + 5 */
    /* used for optimizing assembler (value stays in eax for more movement */

    x += 1; /* obv what, with arrays can be an optimization (taking val by idx
               only once) over a[i] = a[i] + 1 */
    /* also -=, *=, /=, %=, &=, |=, ^=, <<=, >>= */
    --a; /* inc/dec */
    x = a++; /* old a value to x */
    x = ++a; /* new a value to x */

    /* a function call () is and operation too, (of func name and params) */

    x = a > b ? a : b; /* ternary operation */
    
    p = (char)(a/2); /* type cast */

    pp = &p; /* reference/dereference */
    p = *pp;

    printf("%i %i %.5f\n", a / b, a % b, c / d);
}

int main()
{
    types();
    expr();
    return 0;
}
