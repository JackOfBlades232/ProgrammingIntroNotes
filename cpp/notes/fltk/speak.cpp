/* fltk/speak.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>

#include <stdio.h>

enum {
    spacing = 15,
    button_w = 600,
    button_h = 120,
    font_size = 60
};

static const char *msg[] = {
    "Say hello", "Say goodbye", "Quit"
};

// All widgets inherit from Fl_Widget
static void say_callback(Fl_Widget *, void *user)
{
    printf("%s\n", (const char *)user);
}

// This is how you can omit not wanted params
static void exit_callback(Fl_Widget *w, void *)
{
    // Fl::run() finishes when at least one window is visible. We have only
    // one window, thus if we find it, we can just hide it. It is definitly
    // somewhere in the ancestors of w, so we can just find it
    Fl_Widget *p;
    do {
        p = w->parent();
        if (p)
            w = p;
    } while (p);
    w->hide();
}

int main()
{
    int win_w = button_w + 2*spacing;
    int win_h = 3*button_h + 4*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "speak");

    Fl_Button *b[3];
    int y = spacing;
    for (int i = 0; i < 3; i++) {
        b[i] = new Fl_Button(spacing, y, button_w, button_h, msg[i]);
        b[i]->labelsize(font_size);
        y += button_h + spacing;
    }
    win->end();

    // Set the callbacks
    b[0]->callback(say_callback, (void *)"Hello, world!");
    b[1]->callback(say_callback, (void *)"Goodbye, world!");
    b[2]->callback(exit_callback, 0);

    win->show();
    return Fl::run();
}
