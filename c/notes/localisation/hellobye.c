/* hellobye.c */
#include <stdio.h>
#include <locale.h>
#include <libintl.h>

/* params for gettext */
#define LOCALEBASEDIR "."
#define TEXTDOMAIN "hellobye"

/* macro for localizing all string literals */
#define _(STR) gettext(STR)

/* macro for no-localization, for clarity */
#define N_(STR) (STR)

int main()
{
    char words[2][6] = { N_("word1"), N_("word2") };

    /* init setup */
    setlocale(LC_CTYPE, "");
    setlocale(LC_MESSAGES, "");
    bindtextdomain(TEXTDOMAIN, LOCALEBASEDIR);
    textdomain(TEXTDOMAIN);

    printf(_("Hello, world!\n"));
    printf(_("We are speaking\n"));
    printf(_("Goodbye, world!\n"));

    return 0;
}
