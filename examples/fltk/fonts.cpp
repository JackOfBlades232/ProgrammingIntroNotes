#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>


Fl_Box **make_box_grid(int w, int h, int spc, int nx, int ny)
{
    int i, j;
    Fl_Box **r = new Fl_Box*[nx*ny];
    for(i = 0; i < nx; i++)
        for(j = 0; j < ny; j++) {
             r[i * ny + j] = new Fl_Box((w+spc)*i+spc, (h+spc)*j+spc, w, h);
        }
    return r;
}

enum {
    spacing = 10,
    box_h = 45,
    box_w = 330,
    count_x = 3,
    count_y = 4,
    labelsize = 35
};

const char * const lab_main[count_x] = { "Helvetica", "Courier", "Times" };
const char * const lab_aux[count_y] =
                              { "", " BOLD", " ITALIC", " B+I" };
const int font_codes[count_x] = { FL_HELVETICA, FL_COURIER, FL_TIMES };
const int font_add[count_y] = { 0, FL_BOLD, FL_ITALIC, FL_BOLD+FL_ITALIC };


int main(int argc, char **argv)
{
    int win_w = (box_w+spacing)*count_x+spacing;
    int win_h = (box_h+spacing)*count_y+spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "fonts demo (fonts.cpp)");

    Fl_Box **grid = make_box_grid(box_w, box_h, spacing, count_x, count_y);

    int n, k;
    char buf[80];
    for(n = 0; n < count_x; n++) {
        for(k = 0; k < count_y; k++) {
            sprintf(buf, "%s%s", lab_main[n], lab_aux[k]);
            int idx = n*count_y + k;
            grid[idx]->copy_label(buf);
            grid[idx]->labelfont(font_codes[n] + font_add[k]);
            grid[idx]->labelsize(labelsize);
            grid[idx]->align(FL_ALIGN_CENTER);
        }
    }
    win->end();
    win->show();
    return Fl::run();
}
