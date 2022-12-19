#ifndef MOD_H_SENTRY /* sentry macro */
#define MOD_H_SENTRY /* when 1.h is included in 2.h, and 1.h and 2.h are 
                        included in 3.h, there is a conflict of names (types
                        and macros). unique macro-symbols for files are used to
                        ignore file if already included */

/* macros and types (for outer use), go to .h, if another .h needs your types, 
 * #include in .h files is required */
#define NUMBER 1

typedef struct tag_sometype {
    int val;
    struct tag_sometype *next;
} sometype;

struct item; /* partial types used to hide fields from compiler (when just 
                working with pointers, to incapsulate access, and speed up
                compilation, like FILE* */

void somefunc();
extern int global_var;

#endif /* sentry macros not needed if only vars and funcs (one can decalre them
          multiple times, but better to use that for all files */
