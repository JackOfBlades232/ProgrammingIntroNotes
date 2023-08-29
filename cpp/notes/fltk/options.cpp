/* fltk/options.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Radio_Round_Button.H>
#include <FL/Fl_Check_Button.H>

#include <stdio.h>

// Here are some more button types -- radio buttons for choices and
// checkboxes. Radiobuttons work by Fl_Group, thus 

struct controls {
    Fl_Radio_Round_Button *rb[3];
    Fl_Check_Button *cb;
    Fl_Box *box;
};

enum {
    spacing = 6,
    button_w = 450,
    button_h = 120,
    option_w = 360,
    option_h = 60,
    letter_size = 270,
    font_size = 45
};

static const int colors[] = { FL_RED, FL_GREEN, FL_BLUE };
static const char *const colnames[] = { "red", "green", "blue" };

static void set_callback(Fl_Widget *w, void *user)
{
    controls *c = (controls *)user;
    for (int i = 0; i < 3; i++) {
        if (c->rb[i]->value()) {
            c->box->color(colors[i]);
            break;
        }
    }
    c->box->label(c->cb->value() ? "A" : "");
}

static void exit_callback(Fl_Widget *w, void *)
{
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
    int win_w = 2*button_w + 3*spacing;
    int win_h = 4*option_h + button_h + 7*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "options");

    controls *ctrl = new controls;

    for (int i = 0; i < 3; i++) {
        int y = spacing * (i+1) + option_h * i;
        ctrl->rb[i] = new Fl_Radio_Round_Button(spacing, y, 
                                                option_w, option_h,
                                                colnames[i]);
        ctrl->rb[i]->labelsize(font_size);
    }
    ctrl->cb = new Fl_Check_Button(spacing, 5*spacing + 3*option_h,
                                   option_w, option_h, "show letter");
    ctrl->cb->labelsize(font_size);

    int box_x = 2*spacing + option_w;
    int box_w = 2*button_w - option_w;
    int box_h = 4*option_h + 4*spacing;
    ctrl->box = new Fl_Box(box_x, spacing, box_w, box_h);
    ctrl->box->labelsize(letter_size);
    ctrl->box->labelcolor(FL_WHITE);
    ctrl->box->box(FL_FLAT_BOX); // Set box type, for opaque bg

    int buttons_y = 6*spacing + 4*option_h;
    Fl_Button *set_b = new Fl_Button(spacing, buttons_y,
                                     button_w, button_h, "Set!");
    set_b->labelsize(font_size);
    set_b->callback(set_callback, (void *)ctrl);

    Fl_Button *close_b = new Fl_Button(2*spacing + button_w, buttons_y,
                                       button_w, button_h, "Quit");
    close_b->labelsize(font_size);
    close_b->callback(exit_callback, 0);

    win->end();
    win->show();
    return Fl::run();
}
