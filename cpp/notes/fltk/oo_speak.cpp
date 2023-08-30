/* fltk/oo_speak.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>

#include <stdio.h>

// Here we introduce another layer: we will try to be more OOP, satying in
// the world of objects and messages. For this, we have to replace callbacks
// with some oop implementation. For this we inherit a class from Fl_Button,
// and redefine methods for callbacks, so that vtable funcs get used instead.

enum {
    spacing = 15,
    button_w = 600,
    button_h = 120,
    font_size = 60
};

// For a full layer, we'll have to do this for all widgets
class OOFLButton : public Fl_Button {
public:
    OOFLButton(int x, int y, int w, int h, const char *lbl = 0)
        : Fl_Button(x, y, w, h, lbl)
        { callback(CallbackFunction, 0); }
    virtual ~OOFLButton() {}
    virtual void OnPress() {}

private:
    // Thus we cunningly implant redefinable OnPress as the callback for
    // every single OOFLButton and descendants on construction
    static void CallbackFunction(Fl_Widget *w, void *user)
        { static_cast<OOFLButton *>(w)->OnPress(); }
}; 

class ButtonCommon : public OOFLButton {
public:
    ButtonCommon(int y, const char *lbl)
        : OOFLButton(spacing, y, button_w, button_h, lbl)
        { labelsize(font_size); }
};

// Now we have these guys instead of callbacks, compliant with OOP(sie daisy)

class SayButton : public ButtonCommon {
    const char *msg;
public:
    SayButton(int y, const char *lbl, const char *a_msg)
        : ButtonCommon(y, lbl), msg(a_msg) {}
    virtual void OnPress() { printf("%s\n", msg); }
};
class ExitButton : public ButtonCommon {
public:
    ExitButton(int y) : ButtonCommon(y, "Quit") {}
    virtual void OnPress() { exit(0); } 
};

int main()
{
    int win_w = button_w + 2*spacing;
    int win_h = 3*button_h + 4*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "speak");
    // Now, we just create our versions of the buttons, and it is enough
    // we don't even need to save them. 
    // @HUH: the ancestor Fl_ classes might not have virtual destructors:
    // when FLTK destroys such objects, our virtual destructors in OOFL..
    // stuf may not get called. Thus we hope, that at runtime only we will
    // be destroying objects at our level
    new SayButton(spacing, "Say hello", "Hello, world!");
    new SayButton(2*spacing + button_h, "Say goodbye", "Goodbye, world!");
    new ExitButton(3*spacing + 2*button_h);

    win->end();
    win->show();
    return Fl::run();
}
