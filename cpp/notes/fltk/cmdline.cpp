/* fltk/cmdline.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>

#include <stdio.h>

// In this example it is shown, how cmdline args can be shared between
// user code and FLTK (for example, standard args like -display and -geometry..)

enum {
    spacing = 15,
    button_w = 600,
    button_h = 120,
    font_size = 40
};

// Arg processing callback doesn't take user data, so it needs globals
static int buttons_cnt = 3;

static void set_buttons_count(const char *arg)
{
    char *errptr;
    buttons_cnt = strtol(arg, &errptr, 10);
    if (errptr == arg || *errptr) {
        fprintf(stderr, "Invalid btn count %s\n", arg);
        exit(1);
    }
}

// The cmdline arg processing callback. Recieved on ecountering a 
// -xxx flag and allows processing it and it's args (if a non - arg found,
// or a sole -, or an unknown arg not eaten by the callback is encountered,
// the processing terminates. the case 3 is an error). The callback function
// is called before the FLTK processing
static int cmdline_callback(int argc, char **argv, int &idx)
{
    if (strcmp(argv[idx], "-count") == 0) {
        if (argc < idx + 2) {
            fprintf(stderr, 
                    "Count value (number) expecter after -count flag\n");
            exit(1);
        }

        set_buttons_count(argv[idx+1]);
        idx += 2;
        return 2; // "eaten" args
    }
    
    return 0;
}

static void say_callback(Fl_Widget *, void *user)
{
    printf("%s\n", (const char *)user);
}

int main(int argc, char **argv)
{
    int arg_idx, res;
    // The arg processing cycle
    res = Fl::args(argc, argv, arg_idx, cmdline_callback);
    if (!res) {
        fprintf(stderr, "Malformed command line\n");
        return 1;
    }
    // Alternatively, we could run the cycle ourselves, and call Fl::arg
    // to feed args to FLTK one by one (this function has the same profile
    // as the callback, so it is literally a mirrored situation)

    int win_w = button_w + 2*spacing;
    int win_h = (button_h + spacing)*buttons_cnt + spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "cmdline");

    int y = spacing;
    for (int i = 0; i < buttons_cnt; i++) {
        const char *msg = arg_idx + i < argc ? argv[arg_idx+i] : "Not set";
        Fl_Button *b = new Fl_Button(spacing, y, button_w, button_h, msg);
        b->labelsize(font_size);
        b->callback(say_callback, (void *)msg);
        y += button_h + spacing;
    }

    win->end();
    // This passing is still needed. If we did not do a cycle, we could
    // just pass these over. If we organise the cycle with Fl::arg, we
    // should either call it at least once (thus letting FLTK know that we
    // processed the args), or call show without args
    win->show(argc, argv);
    return Fl::run();
}
