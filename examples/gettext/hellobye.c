#include <stdio.h>
#include <locale.h>
#include <libintl.h>

#define LOCALEBASEDIR "."
#define TEXTDOMAIN "hellobye"

#define _(STR) gettext(STR)
#define N_(STR) (STR)

int main()
{
    setlocale(LC_CTYPE, "");
    setlocale(LC_MESSAGES, "");
    bindtextdomain(TEXTDOMAIN, LOCALEBASEDIR);
    textdomain(TEXTDOMAIN);
    printf(_("Hello, world!\n"));
    printf(_("We are speaking\n"));
    printf(_("Good bye, world!\n"));
    return 0;
}
