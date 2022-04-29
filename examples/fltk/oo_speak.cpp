#include <stdio.h>
#include <stdlib.h>
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>


class OOFLButton : public Fl_Button {
public:
    OOFLButton(int x, int y, int w, int h, const char *lb = 0)
        : Fl_Button(x, y, w, h, lb) { callback(CallbackFunction, 0); }
    virtual ~OOFLButton() {}
    virtual void OnPress() {}
private:
    static void CallbackFunction(Fl_Widget *w, void *user)
        { static_cast<OOFLButton*>(w)->OnPress(); }
};

enum {
    spacing = 5,
    button_w = 200,
    button_h = 40,
    font_size = 20
};

class ButtonCommon : public OOFLButton {
public:
    ButtonCommon(int y, const char *lb)
        : OOFLButton(spacing, y, button_w, button_h, lb)
    { labelsize(font_size); }
};

class SayButton : public ButtonCommon {
    const char *msg;
public:
    SayButton(int y, const char *lb, const char *amsg)
        : ButtonCommon(y, lb), msg(amsg) {}
    virtual void OnPress() { printf("%s\n", msg); }
};

class ExitButton : public ButtonCommon {
public:
    ExitButton(int y) : ButtonCommon(y, "Quit") {}
    virtual void OnPress() { exit(0); }
};

int main(int argc, char **argv)
{
    int win_w = button_w + spacing * 2;
    int win_h = button_h * 3 + spacing * 4;
    Fl_Window *win = new Fl_Window(win_w, win_h, "buttons demo");
    new SayButton(spacing, "Say hello", "Hello, world!");
    new SayButton(2 * spacing + button_h, "Say goodbye", "Goodbye, world!");
    new ExitButton(3 * spacing + 2 * button_h);
    win->end();
    win->show();
    return Fl::run();
}
