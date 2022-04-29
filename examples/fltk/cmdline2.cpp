#include <stdio.h>
#include <stdlib.h>
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>

enum {
    spacing = 5,
    button_w = 200,
    button_h = 40,
    font_size = 20
};

static void say_callback(Fl_Widget *w, void *user)
{
    printf("%s\n", (const char*)user);
}

int main(int argc, char **argv)
{
    int buttons_count = 3;
    int idx = 1;
    bool fltk_args = false;

    while(idx < argc) {
        if(0 == strcmp(argv[idx], "-count")) {
            if(argc < idx + 2) {
                fprintf(stderr, "Count (number) expected\n");
                exit(1);
            }

            char *errptr;
            buttons_count = strtol(argv[idx], &errptr, 10);
            if(errptr == argv[idx] || *errptr) {
                fprintf(stderr, "Invalid count %s\n", argv[idx]);
                exit(1);
            }
            idx += 2;
            continue;
        }
        if(argv[idx][0] == '-') {
            int res = Fl::arg(argc, argv, idx);
            fltk_args = true;
            if(res == 0) {
                fprintf(stderr, "Flag <<%s>> unknown\n", argv[idx]);
                exit(1);
            }
            continue;
        }
        break;
    }

    int win_w = button_w + spacing * 2;
    int win_h = (button_h + spacing) * buttons_count + spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "command line demo");

    int i;
    int y = spacing;
    for(i = 0; i < buttons_count; i++) {
        const char *msg =
            idx + i < argc ? argv[idx + i] : "Not set";
        Fl_Button *b =
            new Fl_Button(spacing, y, button_w, button_h, msg);
        b->labelsize(font_size);
        b->callback(say_callback, (void*)msg);
        y += button_h + spacing;
    }
    win->end();
    if(fltk_args)
        win->show(argc, argv);
    else
        win->show();
    return Fl::run();
}
