/* 10_23/main.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

#define ASSERT(_e, _fmt, ...) \
    if(!(_e)) { \
        fprintf(stderr, \
                "Assertion failed at %s:%d : " _fmt "\n", \
                __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1); \
    }

#define ABS(_a) ((_a) < 0 ? -(_a) : (_a))

#define FONT_SIZE_TO_BTN 0.35

enum {
    btn_sz = 100,
    spacing = 10,
    padding = 50
};

const int font_size = (int)((float)btn_sz * FONT_SIZE_TO_BTN);

static inline int randint(int min, int max)
{
    return min + (int) ((float) (max-min+1) * rand() / (RAND_MAX+1.0));
}

static void quit_app(Fl_Widget *w);
static void button_callback(Fl_Widget *w, void *user);

class FifteenButton;

class FifteenBoard {
    int null_x, null_y;
    FifteenButton *btns[4][4];
    FifteenButton *ordered_btn_refs[4*4]; // for ease of sorting

public:
    FifteenBoard();
    void Reset();
    bool IsBeaten();
    int GetNullX() { return null_x; }
    int GetNullY() { return null_y; }
    void MoveBtnToFreeSpot(FifteenButton *btn);
};

class FifteenButton : public Fl_Button {
    int number;
    int x_idx, y_idx;

public:
    FifteenButton(FifteenBoard *brd,
            int a_number, int a_xidx, int a_yidx);
    void Move(int new_xidx, int new_yidx);
    int GetNumber() { return number; }
    int GetXIdx() { return x_idx; }
    int GetYIdx() { return y_idx; }

private:
    void UpdateLabel();
    static int IdxToCoord(int idx)
        { return padding + idx*btn_sz + idx*spacing; }
};

static void button_callback(Fl_Widget *w, void *user)
{
    FifteenButton *btn = static_cast<FifteenButton *>(w);
    FifteenBoard *brd = (FifteenBoard *)user;

    brd->MoveBtnToFreeSpot(btn);

    if (brd->IsBeaten()) {
        // TODO: impl message and restart
        quit_app(w);        
    }
}

static void quit_app(Fl_Widget *w)
{
    Fl_Widget *p;
    do {
        p = w->parent();
        if (p)
            w = p;
    } while (p);
    w->hide();
}

FifteenBoard::FifteenBoard() : null_x(3), null_y(3)
{
    for (int y = 0; y < 4; y++)
        for (int x = 0; x < 4; x++) {
            if (x == null_x && y == null_y)
                btns[y][x] = 0;
            else
                btns[y][x] = new FifteenButton(this, 4*y + x + 1, x, y);
            ordered_btn_refs[4*y + x] = btns[y][x];
        }

    Reset();
}

static bool permutation_is_id(int *arr, int size)
{
    for (int i = 0; i < size; i++) {
        if (arr[i] != i+1)
            return false;
    }

    return true;
}

static bool permutation_is_even(int *arr, int size)
{
    // TODO: implement
    return false;
}

void FifteenBoard::Reset()
{
    int perm[15];
    for (int i = 0; i < 15; i++)
        perm[i] = i+1;

    while (permutation_is_id(perm, 15) || permutation_is_even(perm, 15)) {
        for (int i = 0; i < 15-1; i++) {
            int xchg_idx = randint(i, 15-1);
            int tmp = perm[i];
            perm[i] = perm[xchg_idx];
            perm[xchg_idx] = tmp;
        }
    }

    for (int y = 0; y < 4; y++)
        for (int x = 0; x < 4; x++) {
            int i = 4*y + x; 
            if (i < 15) {
                btns[y][x] = ordered_btn_refs[perm[i]-1];
                btns[y][x]->Move(x, y);
            }
            else 
                btns[y][x] = 0;
        }
}

bool FifteenBoard::IsBeaten()
{
    for (int i = 0; i < 15; i++) {
        FifteenButton *btn = btns[i/4][i%4];
        if (!btn || btn->GetNumber() != i+1)
            return false;
    }

    return true;
}

void FifteenBoard::MoveBtnToFreeSpot(FifteenButton *btn)
{
    int btn_x = btn->GetXIdx();
    int btn_y = btn->GetYIdx();
    if (ABS(btn_x - null_x) + ABS(btn_y - null_y) > 1)
        return;

    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = btn_x;
    null_y = btn_y;
    btns[null_y][null_x] = 0;
}

FifteenButton::FifteenButton(FifteenBoard *brd,
        int a_number, int a_xidx, int a_yidx)
    : Fl_Button(IdxToCoord(a_xidx), IdxToCoord(a_yidx), btn_sz, btn_sz),
    number(a_number), x_idx(a_xidx), y_idx(a_yidx)
{
    labelsize(font_size);
    UpdateLabel();
    callback(button_callback, (void *)brd);
}

void FifteenButton::Move(int new_xidx, int new_yidx)
{
    x_idx = new_xidx;
    y_idx = new_yidx;
    hide();
    position(IdxToCoord(x_idx), IdxToCoord(y_idx));
    show();
}

void FifteenButton::UpdateLabel()
{
    char buf[32];
    snprintf(buf, sizeof(buf), "%d", 4*y_idx + x_idx + 1);
    copy_label(buf);
}

int main(int argc, char **argv)
{
    srand(time(0));

    int win_sz = btn_sz*4 + spacing*3 + padding*2;
    Fl_Window *win = new Fl_Window(win_sz, win_sz, "10_23 (fifteen)");

    FifteenBoard board;

    win->resizable(win);
    win->size_range(win_sz/2, win_sz/2, 2*win_sz, 2*win_sz, 0, 0, true);
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
