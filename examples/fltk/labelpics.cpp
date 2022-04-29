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
    box_w = 200,
    count_x = 4,
    count_y = 5,
    labelsize = 20
};

const char * const labels[] = {
    "@>>", "@2>>", "@7>>", "@+9>>", "@-200097>>",
    "@fileprint", "@%fileprint", "@3fileprint", "@+5fileprint", "@-28fileprint",
    //"@-->", "@$-->", "@00015-->", "@9-->", "@+99-->",
    "@DnArrow", "@$DnArrow", "@00015DnArrow", "@9DnArrow", "@+99DnArrow",
    "@->|", "@-100345->|", "@00015->|", "@9->|", "@+99->|",
    0
};

int main(int argc, char **argv)
{
    int win_w = (box_w+spacing)*count_x+spacing;
    int win_h = (box_h+spacing)*count_y+spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h,
                                   "label icons demo (labelpics.cpp)");

    Fl_Box **grid = make_box_grid(box_w, box_h, spacing, count_x, count_y);

    int n;
    char buf[80];
    for(n = 0; n < count_x * count_y; n++) {
        if(!labels[n])
            break;
        if(!*labels[n])
            continue;
        sprintf(buf, "%s  @%s", labels[n], labels[n]);
        grid[n]->labelfont(FL_COURIER);
        grid[n]->labelsize(labelsize);
        grid[n]->copy_label(buf);
        grid[n]->align(FL_ALIGN_LEFT|FL_ALIGN_INSIDE);
//        grid[n]->box(FL_BORDER_FRAME);
//        grid[n]->color(FL_RED);
    }
    win->end();
    win->show();
    return Fl::run();
}
