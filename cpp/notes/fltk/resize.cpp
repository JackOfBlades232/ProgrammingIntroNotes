/* fltk/resize.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

inline void prep_box(Fl_Box *b) 
{ 
    b->box(FL_FLAT_BOX); b->color(FL_WHITE); b->labelsize(48);
}

int main()
{
    const int spacing = 15;
    Fl_Window *win = new Fl_Window(600+5*spacing, 600+5*spacing, "hello");
    Fl_Box *a = new Fl_Box(spacing, spacing, 300+spacing, 150, "A");
    prep_box(a);
    Fl_Box *b = new Fl_Box(300+3*spacing, spacing, 150, 150, "B");
    prep_box(b);
    Fl_Box *c = new Fl_Box(450+4*spacing, spacing, 150, 150, "C");
    prep_box(c);
    Fl_Box *d = new Fl_Box(spacing, 150+2*spacing, 150, 300+spacing, "D");
    prep_box(d);
    Fl_Box *e = new Fl_Box(450+4*spacing, 150+2*spacing, 150, 150, "E");
    prep_box(e);
    Fl_Box *f = new Fl_Box(450+4*spacing, 300+3*spacing, 150, 300+spacing, "F");
    prep_box(f);
    Fl_Box *g = new Fl_Box(spacing, 450+4*spacing, 150, 150, "G");
    prep_box(g);
    Fl_Box *h = new Fl_Box(150+2*spacing, 450+4*spacing, 300+spacing, 150, "H");
    prep_box(h);

    Fl_Box *r = new Fl_Box(150+2*spacing, 150+2*spacing, 
                           300+spacing, 300+spacing, "R");
    prep_box(r);

    // Every Fl_Group has One resizable element -- if it is itself, everything
    // scales, if it is 0 -- it can't be resized, if it is a single element, 
    // everything is resized with respect to it (if elements intersect with it
    // horiz or vert, they will also resize on that axis). In order to
    // restrict some axis resizings, use dummy-no-box widgets.
    win->resizable(r);

    // Min-max dimesions are chosen extremely strangely, some calc from
    // init resizable dimensions and 100 (if dim is <100, it is min, else --
    // 100 is min)
    // One can also set this with size_range (analagous to WM Size Hints in X11)
    win->size_range(450, 450, 900, 900, 50, false); // Most params are optional

    win->end();
    win->show();
    return Fl::run();
}
