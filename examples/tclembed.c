/*
   Tcl embedding/extending demo
   Copyright (C) by Andrey Vikt. Stolyarov, 2019.

   You can do whatever you want with this code; you're responsible
   for any consequences.


   To compile this as a Tcl interpreter:

        gcc -Wall -g -ltcl tclembed.c -o tclembed

   To compile this as a plugin for tclsh:

        gcc -shared -fpic -Wall -g -DPLUGIN tclembed.c -o tclembed.so

   The resulting plugin can be loaded with

        load ./tclembed.so

   from within tclsh
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tcl/tcl.h>

enum { maxcmd = 32 * 1024 };

static int read_command(char *cmd_buf, int bufsize)
{
    int len = 0;
    *cmd_buf = 0;
    do {
        const char *r;
        if(len > bufsize-3) {  /* nowhere to read "}\n" to */
            fprintf(stderr, "Command too long\n");
            return 0;
        }
        cmd_buf[bufsize-2] = 0;
        r = fgets(cmd_buf + len, bufsize-len, stdin);
        if(!r)    /* EOF */
            return 0;
        if(cmd_buf[bufsize-2]) {
            int c;
            fprintf(stderr, "Command too long\n");
            while((c = getchar()) != EOF && c != '\n')
               ;
            return 0;
        }
        len += strlen(cmd_buf+len);
    } while(!Tcl_CommandComplete(cmd_buf));
    return 1;
}

static void rep_loop(Tcl_Interp *interp, const char *prompt)
{
    char *buf;
    buf = malloc(maxcmd);
    fputs(prompt, stdout);
    while(read_command(buf, maxcmd)) {
        int code;
        const char *resptr;
        Tcl_ResetResult(interp);
        code = Tcl_Eval(interp, buf);
        switch(code) {
        case TCL_OK:
            resptr = Tcl_GetStringResult(interp);
            if(*resptr)
                printf("%s\n", resptr);
            break;
        case TCL_ERROR:
            resptr = Tcl_GetStringResult(interp);
            fprintf(stderr, "TCL ERROR: %s\n", resptr);
            break;
        case TCL_RETURN:
            fprintf(stderr, "return?! what's the hell?\n");
            break;
        case TCL_BREAK:
        case TCL_CONTINUE:
            fprintf(stderr,
                    "it's a bit strange to use break/continue this way\n");
            break;
        default:
            fprintf(stderr, "unknown Tcl return code %d\n", code);
        }
        fputs(prompt, stdout);
    }
}

static int
proc_triple(ClientData cd, Tcl_Interp *interp, int argc, const char **argv)
{
    int len;
    char *res;
    if(argc != 2) {
        Tcl_SetResult(interp, "must specify exactly 1 parameter", TCL_STATIC);
        return TCL_ERROR;
    }
    len = strlen(argv[1]);
    res = Tcl_Alloc(len*3+1);
    strcpy(res, argv[1]);
    strcpy(res+len, argv[1]);
    strcpy(res+2*len, argv[1]);
    Tcl_SetResult(interp, res, TCL_DYNAMIC);
    return TCL_OK;
}

static int
proc_enterloop(ClientData cd, Tcl_Interp *interp, int argc, const char **argv)
{
    const char *prompt = argc > 1 ? argv[1] : ">> ";
    rep_loop(interp, prompt);
    clearerr(stdin);
    Tcl_SetResult(interp, "", TCL_STATIC);
    return TCL_OK;
}

#ifndef PLUGIN

int main()
{
    Tcl_Interp *interp = Tcl_CreateInterp();
    Tcl_CreateCommand(interp, "triple", proc_triple, NULL, NULL);
    Tcl_CreateCommand(interp, "ENTER", proc_enterloop, NULL, NULL);
    rep_loop(interp, "> ");
    return 0;
}

#else

int Tclembed_Init(Tcl_Interp *interp)
{
    Tcl_CreateCommand(interp, "triple", proc_triple, NULL, NULL);
    Tcl_CreateCommand(interp, "ENTER", proc_enterloop, NULL, NULL);
    return TCL_OK;
}

#endif
