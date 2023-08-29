/* fltk/labelpics.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

enum {
    boxes_in_row = 3,
    boxes_in_col = 4,
    spacing = 30,
    box_w = 180,
    box_h = 60,
    font_size = 22
};

static const char *labels[boxes_in_col][boxes_in_row] = {
    { "Helvetica", "Courier", "Times" },
    { "Helvetica BOLD", "Courier BOLD", "Times BOLD" },
    { "Helvetica ITALIC", "Courier ITALIC", "Times ITALIC" },
    { "Helvetica B+I", "Courier B+I", "Times B+I" }
};

static const int fonts[boxes_in_col][boxes_in_row] = {
    { FL_HELVETICA, FL_COURIER, FL_TIMES },
    { FL_HELVETICA_BOLD, FL_COURIER_BOLD, FL_TIMES_BOLD },
    { FL_HELVETICA_ITALIC, FL_COURIER_ITALIC, FL_TIMES_ITALIC },
    { FL_HELVETICA_BOLD_ITALIC, FL_COURIER_BOLD_ITALIC, FL_TIMES_BOLD_ITALIC }
};

int main()
{
    int win_w = boxes_in_row*box_w + (boxes_in_row+1)*spacing;
    int win_h = boxes_in_col*box_h + (boxes_in_col+1)*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "labelpics");

    for (int y = 0; y < boxes_in_col; y++)
        for (int x = 0; x < boxes_in_row; x++) {
            int box_x = x*box_w + (x+1)*spacing;
            int box_y = y*box_h + (y+1)*spacing;
            Fl_Box *b = new Fl_Box(box_x, box_y, box_w, box_h);

            b->label(labels[y][x]);
            b->labelfont(fonts[y][x]);

            b->labelsize(font_size);
            // Method for aligning lables inside widget
            b->align(FL_ALIGN_INSIDE|FL_ALIGN_TOP);
        }

    win->end();
    win->show();
    return Fl::run();
}
