/* basics.c */
#include <stdio.h> /* <> for libraries (side ones might not work), "" for local
                      files */
/* -I gcc flag lets you specify places to look for headers. */

#define BUFFERSIZE 1024
#define PRHELLOMSG printf("Hello, world\n")
#define IF if(
#define THEN ) {
#define ELSE } else {
#define FI }

/* problem: no chars can be put after \, even spaces */
#define TEXT "This is how one splits a text by lines in a macrodefinition\n" \
    "in the C programming language\n"

/* better fill parametrized macro with parens, so that you don't get
 * MAX(100, 10) * 5 == 100 > 10 ? : 100 : 10 * 5 */
#define MAX(A, B) ((A) > (B) ? (A) : (B))

/* a way not to write identical functions for different types */
/* ## in macros glues together lexems */
#define MAKE_ARRAY_SUM_FUNCTION(TYPE) \
    TYPE TYPE ## _array_sum(const TYPE *a, int n) \
    { \
        TYPE s = 0; \
        while (n > 0) { \
            s += *a; \
            a++; \
            n--; \
        } \
        return s; \
    }

MAKE_ARRAY_SUM_FUNCTION(int);
MAKE_ARRAY_SUM_FUNCTION(double);
MAKE_ARRAY_SUM_FUNCTION(long);

/* #x turns x into a string literal = "name of x" */
#define VAR_PRINT(x) printf("%s = %d\n", #x, x)

/* hack for using macros in no-bracket ifs and whiles */
#define MYMACRO(arg) do { printf("%s = %d\n", #arg, arg); } while (0)

/* macroproc goes after lexic analysis, so you can't leave unclosed comments
 * or strings, but it goes before syntactic analysis, so you can leave non-
 * closed parens and other stuff */
/* #if defined(USE_INDEX_IN_STRING_COPY) && USE_INDEX_IN_STRING_COPY >= 1 */
/* can be defined with comp flag -D USE...Y(=1) */
#ifdef USE_INDEX_IN_STRING_COPY
/* long constans are used in #ifs */
void string_copy(char *dest, const char *src)
{
    int i;
    for (i = 0; src[i]; i++)
        dest[i] = src[i];
#  if 1 /* when nesting, leave # at the beginnig if the line */
    dest[i] = 0;
#  else
    dest[i] = -1;
#  endif
}
#else
void string_copy(char *dest, const char *src)
{
    for (; *src; dest++, src++)
        *dest = *src;
    *dest = 0;
}
#endif

int main()
{
    char buffer[BUFFERSIZE];
    int a, b;
    int abracadabra;

    PRHELLOMSG;

    a = 1;
    b = 2;

    IF a > b THEN
        printf("a > b\n");
        b = a;
    ELSE
        printf("a < b\n");
        a = b;
    FI
    
    a = 2;
    b = 1;
    
    b = MAX(a, b);

    abracadabra = 12;
    VAR_PRINT(abracadabra);

    if (abracadabra)
        MYMACRO(abracadabra);

    return 0;
}

#undef MYMACRO

#ifdef MYMACRO
#error Failed to undef MYMACRO
#endif
