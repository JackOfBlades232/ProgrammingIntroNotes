/* 10_20.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>

#include <stdio.h>
#include <stdlib.h>

enum {
    spacing = 30,
    btn_w = 200,
    elem_h = 80,
    font_size = 36
};

static void hide_main_window(Fl_Widget *w)
{
    Fl_Widget *p;
    do {
        p = w->parent();
        if (p)
            w = p;
    } while (p);
    w->hide();
}

static void yes_callback(Fl_Widget *w, void *)
{
    hide_main_window(w);
}

static void no_callback(Fl_Widget *w, void *)
{
    hide_main_window(w);
    exit(1); 
}

static void win_callback(Fl_Widget *w, void *)
{
    w->hide();
    if (Fl::event() == FL_SHORTCUT && Fl::event_key() == FL_Escape)
        exit(1);
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Args: <message>\n");
        return 1;
    }

    int win_w = 2*btn_w + 3*spacing;
    int win_h = 2*elem_h + 3*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "10_20");

    Fl_Box *text_box = 
        new Fl_Box(spacing, spacing, 2*btn_w + spacing, elem_h, argv[1]);
    text_box->labelsize(font_size);

    int btn_y = elem_h + 2*spacing;
    Fl_Button *yes_btn = 
        new Fl_Button(spacing, btn_y, btn_w, elem_h, "Yes");
    Fl_Button *no_btn = 
        new Fl_Button(btn_w + 2*spacing, btn_y, btn_w, elem_h, "No");

    yes_btn->labelsize(font_size);
    no_btn->labelsize(font_size);

    win->callback(win_callback, 0);
    yes_btn->callback(yes_callback, 0);
    no_btn->callback(no_callback, 0);

    win->end();
    win->show();
    return Fl::run();
}
