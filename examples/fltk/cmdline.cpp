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

static int buttons_count = 3;

static void set_buttons_count(const char *arg)
{
    char *errptr;
    buttons_count = strtol(arg, &errptr, 10);
    if(errptr == arg || *errptr) {
        fprintf(stderr, "Invalid count %s\n", arg);
        exit(1);
    }
}

static int cmdline_callback(int argc, char **argv, int &idx)
{
    if(0 == strcmp(argv[idx], "-count")) {
        if(argc < idx + 2) {
            fprintf(stderr, "Count (number) expected\n");
            exit(1);
        }
        set_buttons_count(argv[idx+1]);
        idx += 2;
        return 2;  /* we use the current and the next one */
    }
#if 0
    if(0 == strcmp(argv[idx], "-bg")) {
        // just ignore
        idx += 2;
        return 2;
    }
#endif
    return 0;  /* we don't know flags other than '-count' */
}


int main(int argc, char **argv)
{
    int arg_idx, res;
    res = Fl::args(argc, argv, arg_idx, cmdline_callback);
    if(!res) {
        fprintf(stderr, "Malformed command line\n");
        return 1;
    }

    int win_w = button_w + spacing * 2;
    int win_h = (button_h + spacing) * buttons_count + spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "command line demo");

    int i;
    int y = spacing;
    for(i = 0; i < buttons_count; i++) {
        const char *msg =
            arg_idx + i < argc ? argv[arg_idx + i] : "Not set";
        Fl_Button *b =
            new Fl_Button(spacing, y, button_w, button_h, msg);
        b->labelsize(font_size);
        b->callback(say_callback, (void*)msg);
        y += button_h + spacing;
    }
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
