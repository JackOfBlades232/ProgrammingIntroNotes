/* 10_21.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>

#include <stdio.h>
#include <stdlib.h>

enum {
    row_size = 5,
    spacing = 20,
    btn_w = 200,
    btn_h = 80,
    font_size = 36
};

static void btn_callback(Fl_Widget *w, void *user)
{
    Fl_Widget *p;
    do {
        p = w->parent();
        if (p)
            w = p;
    } while (p);
    w->hide();

    printf("%d\n", *((int *)user));
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
        fprintf(stderr, "Args: <button labels>\n");
        return 1;
    }

    int btn_cnt = argc-1;
    char **btn_labels = argv+1;

    int win_cols = btn_cnt < row_size ? btn_cnt : row_size;
    int win_rows = (btn_cnt-1)/row_size + 1;

    int win_w = win_cols*btn_w + (win_cols+1)*spacing;
    int win_h = win_rows*btn_h + (win_rows+1)*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "10_21");
    win->callback(win_callback, 0);

    int *indices = new int[btn_cnt];
    for (int i = 0; i < btn_cnt; i++) {
        int btn_i = i / row_size;
        int btn_j = i % row_size;
        int btn_x = btn_j*btn_w + (btn_j+1)*spacing;
        int btn_y = btn_i*btn_h + (btn_i+1)*spacing;

        Fl_Button *btn = 
            new Fl_Button(btn_x, btn_y, btn_w, btn_h, btn_labels[i]);
        btn->labelsize(font_size);

        indices[i] = i+1;
        btn->callback(btn_callback, &indices[i]);
    }

    win->end();
    win->show();
    return Fl::run();
}
