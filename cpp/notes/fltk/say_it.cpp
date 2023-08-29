/* fltk/say_it.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Input.H>

#include <stdio.h>

// There is also Fl_Multiline input for dumb multiline input, and
// Fl_Text_Editor, which has scrolling and so on. This one works not 
// with a single C-string, but with a Fl_Text_Buffer object. This object
// can be shared between editors, switched out and what not

enum {
    spacing = 60,
    input_h = 90,
    label_w = 150,
    button_w = 300,
    button_h = 120,
    font_size = 45
};

static void say_callback(Fl_Widget *, void *user)
{
    printf("%s\n", ((Fl_Input *)user)->value());
    ((Fl_Input *)user)->take_focus();
    // At each moment only one widget is in focus, this allows kb interaction
}

static void clear_callback(Fl_Widget *, void *user)
{
    ((Fl_Input *)user)->value("");
    ((Fl_Input *)user)->take_focus();
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
    int win_w = 3*button_w + 4*spacing;
    int win_h = input_h + button_h + 3*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "say it");

    // Input field label is placed outside the inp box to the left
    int inp_w = 3*button_w + 2*spacing - label_w;
    Fl_Input *inp = new Fl_Input(spacing + label_w, spacing, 
                                 inp_w, input_h, "Type:");
    inp->labelsize(font_size);
    inp->textsize(font_size);

    int button_y = input_h + 2*spacing;
    Fl_Button *say_b = new Fl_Button(spacing, button_y,
                                     button_w, button_h, "Say it!");
    Fl_Button *clear_b = new Fl_Button(2*spacing + button_w, button_y,
                                       button_w, button_h, "Clear");
    Fl_Button *quit_b = new Fl_Button(3*spacing + 2*button_w, button_y,
                                      button_w, button_h, "Quit");
    say_b->labelsize(font_size);
    clear_b->labelsize(font_size);
    quit_b->labelsize(font_size);

    say_b->callback(say_callback, (void *)inp);
    clear_b->callback(clear_callback, (void *)inp);
    quit_b->callback(exit_callback, 0);

    // In order to activate callback on Enter press when Input is in focus,
    // we do this (when a widget is in focus, we can make it do callbacks on
    // many different conditions)
    inp->callback(say_callback, (void *)inp);
    // When enter pressed, even if contents not changed
    inp->when(FL_WHEN_ENTER_KEY|FL_WHEN_NOT_CHANGED);

    win->end();
    win->show();
    return Fl::run();
}
