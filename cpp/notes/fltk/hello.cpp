/* fltk/hello.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

int main()
{
    Fl_Window *win = new Fl_Window(300, 100, "hello");
    Fl_Box *b = new Fl_Box(0, 0, 300, 100, "Hello, World!");

    // Sets the font size on box
    b->labelsize(36);

    // Fl_Window is inherited from Fl_Group, and for such an object all
    // widgets that are created later are included in the group, until
    // the "end" method is called
    win->end();

    // This is where the connection to XServer happens
    win->show();
    return Fl::run();
}
