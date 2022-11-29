#include <stdio.h>

/* can override default 0..n int values of enum */
enum colors { 
    black,
    red = 0xff0000, 
    yellow = 0xffff00,
    green = 0x00ff000, 
    blue = 0x0000ff, 
    hdr_blue = blue * 2,
    violet = 0xee82ee
};

enum greek { alpha, beta, gamma = beta, delta };

void greek_print(enum greek x)
{
    switch (x) {
        case alpha:    
            printf("Alpha\n");
            break;
        case beta:
            printf("Beta\n");
            break;
        default:
            ;
    }
}

int main()
{
    /* in C, the only way to define constants that have local visibility (i e
     * not macros), but are of compile time (not const modifier) is enums.
     * With floats, only macros suffice. */
    enum { max_buffer_size = 1024 };

    /* 2words: enum color is the type name. Really, it will be of type int, 
     * and enum values are int constants, starting with 0. Can be assigned
     * int values, can perform arithmetics with it, and so on. Not as strict
     * as pascal, eh) */
    enum colors one_color;
    /* can merge type def and vars */
    enum states {
        running, bocked, ready
    } new_state, last_state;
    /* this is also legal, like a typedef */
    int;

    return 0;
}
