/* fltk/output.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Multiline_Output.H>

// Output (and Multiline_Output, inherited from it), are inherited from input
// and are used to output text that you can copy (and so on)
// For scrolling and text_buffer usage Fl_Text_Display can be used

enum {
    padding = 40,
    win_w = 600,
    win_h = 300,
    font_size = 22
};

int main()
{
    Fl_Window *win = new Fl_Window(win_w, win_h, "say it");
    Fl_Multiline_Output *outp = 
        new Fl_Multiline_Output(padding, padding, 
                                win_w - 2*padding, win_h - 2*padding,
                                "Output:");
    outp->align(FL_ALIGN_TOP);
    outp->labelsize(font_size);
    outp->textsize(font_size);
    outp->wrap(true); // Set line wrapping for multiline;
    // always copies for output
    outp->value("Humpty Dumpty sat on a wall, "
                "Humpty Dumpty had a great fall; "
                "All the king's horses and all the king's men "
                "Couldn't put Humpty together again.");

    // Setting NO_BOX or the likes on Fl_Input_ descendants is not to be done,
    // this leads to visual bugs since these elements are redrawn via 
    // redrawing the box
    outp->box(FL_FLAT_BOX);
    outp->color(FL_BACKGROUND_COLOR);

    win->end();
    win->show();
    return Fl::run();
}
