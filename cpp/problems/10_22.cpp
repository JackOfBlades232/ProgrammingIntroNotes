/* 10_22.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Box.H>
#include <FL/Fl_Input.H>
#include <FL/Fl_Button.H>

#include <stdio.h>
#include <errno.h>
#include <time.h>

#define RETURN_DEFER(_code) do { result = _code; goto defer; } while (0)

enum {
    spacing = 20,
    elem_w = 600,
    elem_h = 100,
    btn_w = 300,
    label_w = 100,
    font_size = 36
};

struct text_exerciser_t {
    Fl_Window *win;
    Fl_Box *text_box;
    Fl_Input *input;
    Fl_Button *ok_btn;
    FILE *f;
    char *line;
    size_t max_line_len;
    double start_ms;
};

static double get_ms()
{
    struct timespec ts = { 0 };
    clock_gettime(CLOCK_MONOTONIC, &ts);
    unsigned long long nsec = 
        (unsigned long long)ts.tv_sec*1000000000 + ts.tv_nsec;
    return (double)nsec * 1e-6;
}

static void display_next_line(text_exerciser_t *ex)
{
    size_t line_len = ex->max_line_len;
    // This can theoretically realloc ex->line, but it should not happed
    // unless the file gets modified while the program is running
    if (getline(&ex->line, &line_len, ex->f) == -1) {
        char buf[32];
        snprintf(buf, sizeof(buf), "%.3lf sec", (get_ms() - ex->start_ms)/1000);
        ex->text_box->copy_label(buf);

        ex->input->hide();
        ex->ok_btn->show();
        return;
    }

    for (char *p = ex->line; *p; p++) {
        if (*p == '\n') {
            *p = '\0';
            break;
        }
    }

    ex->text_box->label(ex->line);
    return;
}

void enter_callback(Fl_Widget *w, void *user)
{
    text_exerciser_t *ex = (text_exerciser_t *)user;

    if (strcmp(ex->input->value(), ex->line) == 0)
        display_next_line(ex);

    ex->input->value("");
    ex->input->take_focus();
}

void quit_callback(Fl_Widget *w, void *user)
{
    text_exerciser_t *ex = (text_exerciser_t *)user;
    ex->win->hide();
    if (ex->f) fclose(ex->f);
    if (ex->line) free(ex->line);
}

static bool open_and_check_ex_file(text_exerciser_t *ex, const char *filename)
{
    bool result = true;
    size_t cur_line_len;
    bool has_lines;
    char c;

    ex->f = fopen(filename, "r");
    if (!ex->f) {
        perror(filename);
        RETURN_DEFER(false);
    }

    cur_line_len = 0;
    has_lines = false;
    while ((c = fgetc(ex->f)) != EOF) {
        if (c >= '!' && c <= '~')
            cur_line_len++;
        else if (c == '\n') {
            if (cur_line_len == 0) {
                fprintf(stderr,
                        "File is invalid (All lines must be non-empty)\n");
                RETURN_DEFER(false);
            }
            cur_line_len = 0;
            has_lines = true;
        }

        if (cur_line_len > ex->max_line_len) {
            fprintf(stderr,
                    "File is invalid (all exercise lines "
                    "must be at most %ld chars long)\n", 
                    ex->max_line_len);
            RETURN_DEFER(false);
        }
    }
    if (!has_lines) {
        fprintf(stderr, "File is invalid (it contains no lines)\n");
        RETURN_DEFER(false);
    }

defer:
    if (result && ex->f)
        rewind(ex->f);
    else {
        if (ex->f) fclose(ex->f);
        if (ex->line) free(ex->line);
    }
    return result;
}

int main(int argc, char **argv)
{
    if (argc < 2) {
        fprintf(stderr, "Args: <filename>\n");
        return 1;
    }

    text_exerciser_t ex = { 0 };
    ex.max_line_len = (elem_w-label_w) / font_size;
    ex.line = (char *)malloc((ex.max_line_len+1) * sizeof(*ex.line));
    if (!open_and_check_ex_file(&ex, argv[1]))
        return 2;

    int win_w = elem_w + 2*spacing;
    int win_h = 2*elem_h + 3*spacing;
    ex.win = new Fl_Window(win_w, win_h, "10_22");
    ex.win->callback(quit_callback, (void *) &ex);

    ex.text_box = new Fl_Box(spacing, spacing, elem_w, elem_h, "");
    ex.text_box->labelsize(font_size);

    int sec_row_y = elem_h + 2*spacing;
    ex.input = new Fl_Input(spacing + label_w, sec_row_y,
                             elem_w - label_w, elem_h, "Type:");
    ex.input->labelsize(font_size);
    ex.input->textsize(font_size);
    ex.input->callback(enter_callback, (void *) &ex);
    ex.input->when(FL_WHEN_ENTER_KEY|FL_WHEN_NOT_CHANGED);

    int btn_x = spacing + (elem_w-btn_w)/2;
    ex.ok_btn = new Fl_Button(btn_x, sec_row_y, btn_w, elem_h, "Ok");
    ex.ok_btn->labelsize(font_size);
    ex.ok_btn->callback(quit_callback, (void *) &ex); 

    ex.ok_btn->hide();
    display_next_line(&ex);
    ex.start_ms = get_ms();

    ex.win->end();
    ex.win->show();
    return Fl::run();
}
