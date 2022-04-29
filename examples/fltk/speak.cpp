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

static const char *msg[] = {
    "Say hello", "Say goodbye", "Quit"
};

static void say_callback(Fl_Widget *w, void *user)
{
    printf("%s\n", (const char*)user);
}

static void exit_callback(Fl_Widget *, void *)
{
    exit(0);
}

int main(int argc, char **argv)
{
    int win_w = button_w + spacing * 2;
    int win_h = button_h * 3 + spacing * 4;
    Fl_Window *win = new Fl_Window(win_w, win_h, "buttons demo");

    Fl_Button *b[3];
    int i;
    int y = spacing;
    for(i = 0; i < 3; i++) {
        b[i] = new Fl_Button(spacing, y, button_w, button_h, msg[i]);
        b[i]->labelsize(font_size);
        y += button_h + spacing;
    }
    win->end();

    b[0]->callback(say_callback, (void*)"Hello, world!");
    b[1]->callback(say_callback, (void*)"Goodbye, world!");
    b[2]->callback(exit_callback, 0);

    win->show();
    return Fl::run();
}
