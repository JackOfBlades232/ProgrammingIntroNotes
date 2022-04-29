#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Group.H>
#include <FL/Fl_Box.H>

enum {
    spacing = 5,
    cell_w = 100,
    cell_h = 100,
    num_x = 4,
    num_y = 4,
    labelsz = 50
};

static int cell_x(int n) { return n * (cell_w + spacing) + spacing; }
static int cell_y(int m) { return m * (cell_h + spacing) + spacing; }

static Fl_Box *mkwidget(int n, int m, int cn, int cm, const char *lab)
{
    Fl_Box *b = new Fl_Box(cell_x(n), cell_y(m),
                           cn * cell_w + spacing * (cn-1),
                           cm * cell_h + spacing * (cm-1),
                           lab);
    b->box(FL_EMBOSSED_BOX);
    b->color(FL_WHITE);
    b->labelsize(labelsz);
    return b;
}

int main(int argc, char **argv)
{
    Fl_Window *win =
        new Fl_Window(cell_x(num_x), cell_y(num_y), "resize demo");

    mkwidget(0, 0, 2, 1, "A");
    mkwidget(2, 0, 1, 1, "B");
    mkwidget(3, 0, 1, 1, "C");

    mkwidget(0, 1, 1, 2, "D");
    mkwidget(3, 1, 1, 1, "E");

    mkwidget(3, 2, 1, 2, "F");

    mkwidget(0, 3, 1, 1, "G");
    mkwidget(1, 3, 2, 1, "H");

    Fl_Box *b = mkwidget(1, 1, 2, 2, "R");
    win->resizable(b);

    win->end();
    win->show();
    return Fl::run();
}
