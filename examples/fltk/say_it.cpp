#include <stdio.h>
#include <stdlib.h>
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Button.H>

enum {
    spacing = 20,
    input_h = 30,
    label_w = 50,
    button_w = 100,
    button_h = 40
};

static void say_callback(Fl_Widget *w, void *user)
{
    printf("%s\n", ((Fl_Input*)user)->value());
    ((Fl_Input*)user)->take_focus();
}

static void clear_callback(Fl_Widget *w, void *user)
{
    ((Fl_Input*)user)->value("");
    ((Fl_Input*)user)->take_focus();
}

static void exit_callback(Fl_Widget *w, void *)
{
    Fl_Widget *p;
    do {
        p = w->parent();
        if(p)
            w = p;
    } while(p);
    w->hide();
}

int main(int argc, char **argv)
{
    int win_w = button_w * 3 + spacing * 4;
    int win_h = input_h + button_h + spacing * 3;
    Fl_Window *win = new Fl_Window(win_w, win_h, "input demo");

    int inp_w = button_w * 3 + 2 * spacing - label_w;
    Fl_Input *inp = new Fl_Input(spacing + label_w,
                                 spacing, inp_w, input_h, "Type:");
    inp->callback(say_callback, (void*)inp);
    inp->when(FL_WHEN_ENTER_KEY|FL_WHEN_NOT_CHANGED);

    int buttons_y = 2 * spacing + input_h;
    Fl_Button *say_b =
        new Fl_Button(spacing, buttons_y, button_w, button_h, "Say it!");
    say_b->callback(say_callback, (void*)inp);

    Fl_Button *clear_b =
        new Fl_Button(2 * spacing + button_w,
                      buttons_y, button_w, button_h, "Clear");
    clear_b->callback(clear_callback, (void*)inp);

    Fl_Button *close_b =
        new Fl_Button(3 * spacing + 2 * button_w,
                      buttons_y, button_w, button_h, "Quit");
    close_b->callback(exit_callback, 0);

    win->end();
    win->show();
    return Fl::run();
}
