/* fltk/labelpics.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>

enum {
    pics_in_row = 4,
    pics_in_col = 5,
    spacing = 30,
    pic_w = 180,
    pic_h = 60,
    font_size = 16
};

// @ in label means pictogram code, @@ for single @ in text.
// +-X -- change scale. $ -- vert mirror, % -- horiz mirror
// single digit -- direction (as on numpad)
// 0xxxx -- degrees (must be padded with 0oes)
static const char *labels[pics_in_col][pics_in_row] = {
    { "@>>", "@fileprint", "@DnArrow", "@->|" },
    { "@2>>", "@%fileprint", "@$DnArrow", "@-100345->|" },
    { "@7>>", "@3fileprint", "@00015DnArrow", "@00015->|" },
    { "@+9>>", "@+5fileprint", "@9DnArrow", "@9->|" },
    { "@-200097>>", "@-28fileprint", "+99@DnArrow", "@+99->|" },
};

int main()
{
    int win_w = pics_in_row*pic_w + (pics_in_row+1)*spacing;
    int win_h = pics_in_col*pic_h + (pics_in_col+1)*spacing;
    Fl_Window *win = new Fl_Window(win_w, win_h, "labelpics");

    for (int y = 0; y < pics_in_col; y++)
        for (int x = 0; x < pics_in_row; x++) {
            int box_x = x*pic_w + (x+1)*spacing;
            int box_y = y*pic_h + (y+1)*spacing;
            Fl_Box *b = new Fl_Box(box_x, box_y, pic_w, pic_h);

            char labelbuf[128];
            const char *label = labels[y][x];
            snprintf(labelbuf, sizeof(labelbuf), "%s   @%s", label, label);
            b->copy_label(labelbuf);

            b->labelsize(font_size);
        }

    win->end();
    win->show();
    return Fl::run();
}
