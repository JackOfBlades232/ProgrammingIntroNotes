#include <tcl/tcl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TCL is designed for embedding into C and extending through it.
// Hope it will be more enjoyable than scheme.

enum { c_max_command = 32 * 1024 };

static int read_command(char *buf, int bufsize)
{
    int len = 0;
    *buf = '\0';

    do {
        char const *r;
        if (len > bufsize - 3) { // no space for }\n
            fprintf(stderr, "Command is too long\n");
            return 0;
        }

        buf[bufsize - 2] = '\0';
        r = fgets(buf + len, bufsize - len, stdin);
        if (!r) // EOF
            return 0;

        if (buf[bufsize - 2]) {
            int c;
            fprintf(stderr, "Command is too long\n");
            while ((c = getchar()) != EOF && c != '\n')
                ;
            return 0;
        }

        len += strlen(buf + len);
    } while (!Tcl_CommandComplete(buf)); // Lexical check
    
    return 1;
}

static void rep_loop(Tcl_Interp *interp, char const *prompt)
{
    char *buf;
    buf = malloc(c_max_command);
    fputs(prompt, stdout);

    while (read_command(buf, c_max_command)) {
        int code;
        char const *resptr;

        Tcl_ResetResult(interp); // Clear eval result storage
        code = Tcl_Eval(interp, buf); // Eval read command

        switch (code) {
        case TCL_OK:
            resptr = Tcl_GetStringResult(interp);
            if (*resptr)
                printf("%s\n", resptr);
            break;

        case TCL_ERROR:
            resptr = Tcl_GetStringResult(interp);
            fprintf(stderr, "TCL_ERROR: %s\n", resptr);
            break;

        case TCL_RETURN:
        case TCL_BREAK:
        case TCL_CONTINUE:
            fprintf(stderr, "Unexpected exit code %d\n", code);
            break;

        default:
            fprintf(stderr, "Unknown exit code %d\n", code);
        }

        fputs(prompt, stdout);
    }

    putchar('\n');
}

// We can add a command, implemented by a function of a certain signature
// A command returns a code, takes in user data, interp, argc & argv
static int proc_triple(
    void *cd, Tcl_Interp *interp, int argc, char const **argv)
{
    int len;
    char *res;

    // Tcl_SetResult is used to give back the result. Based on flags
    // we can make it use the string by ptr, make a copy or assume ownership.
    // For assumed ownership, it must be allocated through tcl func

    if (argc != 2) {
        Tcl_SetResult(interp, "must give exactly one parameter", TCL_STATIC);
        return TCL_ERROR;
    }

    len = strlen(argv[1]);
    res = Tcl_Alloc(len * 3 + 1);
    memcpy(res, argv[1], len);
    memcpy(res + len, argv[1], len);
    memcpy(res + 2 * len, argv[1], len);
    res[len * 3] = '\0';

    Tcl_SetResult(interp, res, TCL_DYNAMIC);
    return TCL_OK;
}

// Tcl_Eval at top level turns all error codes to 1.
// To circumvent that, lets make an ENTER command, which launches a repl
// inside of a repl with proper codes.

static int proc_enter(
    void *cd, Tcl_Interp *interp, int argc, char const **argv)
{
    char const *prompt = argc > 1 ? argv[1] : ">>>> ";
    rep_loop(interp, prompt);
    clearerr(stdin); // Unset EOF to only break out of one
    Tcl_SetResult(interp, "", TCL_STATIC);
    return TCL_OK;
}

// One can extend existing tcl interpreters with new functionality through
// dynamic library plugins.

#ifdef PLUGIN

int Tclembed_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "ENTER", proc_enter, NULL, NULL);
    Tcl_CreateCommand(interp, "triple", proc_triple, NULL, NULL);
    return TCL_OK;
}

#else

int main()
{
    // We need to
    // 1) Create an interpreter. It will keep all context, vars, frames, etc.
    // 1.5) Register user commands' function pointers
    // 2) Read commands. Tcl lib can tell us once we've read a lex-complete cmd
    // 3) Call eval on it and process code and result
    //
    // Of course, we'd need init files & files to run for this to be a proper
    // interp, but this is a demo

    Tcl_Interp *interp = Tcl_CreateInterp();
    Tcl_CreateCommand(interp, "ENTER", proc_enter, NULL, NULL);
    Tcl_CreateCommand(interp, "triple", proc_triple, NULL, NULL);

    rep_loop(interp, ">> ");
    return 0;
}

#endif
