/* 11_13/test/main.c */
#include <chicken.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    if (!CHICKEN_initialize(0, 0, 0, C_toplevel)) {
        fprintf(stderr, "Internal error: failed to init chicken context\n");
        return 1;
    }

    CHICKEN_run(NULL);

    void *obj = CHICKEN_global_lookup("palindrome?");
    if (!obj) {
        fprintf(stderr, "Internal error: failed fetch function\n");
        return 1;
    }

    C_word is_palindrome_closure = CHICKEN_global_ref(obj);

    for (int i = 1; i < argc; ++i) {
        C_word arg_closure[2], *acp = arg_closure;
        C_word arg_closure_obj = C_string2(&acp, argv[i]);
        C_word list_closure[C_SIZEOF_LIST(1)], *lcp = list_closure;
        C_word list_closure_obj = C_list(&lcp, 1, arg_closure_obj);

        C_word is;
        if (!CHICKEN_apply(is_palindrome_closure, list_closure_obj, &is)) {
            fprintf(stderr, "Internal error: failed function call\n");
            return 1;
        }

        if (is == C_SCHEME_TRUE)
            printf("\"%s\" is a palindrome!\n", argv[i]);
        else if (is == C_SCHEME_FALSE)
            printf("\"%s\" is not a palindrome\n", argv[i]);
        else {
            fprintf(stderr, "Internal error: invalid return from checker\n");
            return 1;
        }
    }

    return 0;
}
