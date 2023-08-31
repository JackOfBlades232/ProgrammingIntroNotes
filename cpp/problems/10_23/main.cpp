/* 10_23/main.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>

#include <stdio.h>
#include <stdlib.h>
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

static void button_callback(Fl_Widget *w, void *user);

class FifteenButton;

class FifteenBoard {
    FifteenButton *btns[4][4];
    int null_x, null_y;

public:
    FifteenBoard(int a_null_x, int a_null_y);
    void Reset();
    bool IsBeaten();
    int GetNullX() { return null_x; }
    int GetNullY() { return null_y; }
    void SetNullCoords(int x, int y) { null_x = x; null_y = y; }
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

    int prev_x = btn->GetXIdx();
    int prev_y = btn->GetYIdx();
    int null_x = brd->GetNullX();
    int null_y = brd->GetNullY();
    ASSERT(prev_x != null_x || prev_y != null_y, "The null field got clicked");

    if (ABS(prev_x - null_x) + ABS(prev_y - null_y) > 1)
        return;

    btn->Move(null_x, null_y);
    brd->SetNullCoords(prev_x, prev_y);
}

FifteenBoard::FifteenBoard(int a_null_x, int a_null_y)
    : null_x(a_null_x), null_y(a_null_y)
{
    for (int y = 0; y < 4; y++)
        for (int x = 0; x < 4; x++) {
            if (x == null_x && y == null_y)
                break;
            btns[y][x] = new FifteenButton(this, 4*y + x + 1, x, y);
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
    // TODO: reset buttons to 1..15

    int arr[15];
    for (int i = 0; i < 15; i++)
        arr[i] = i+1;

    while (permutation_is_id(arr, 15) || permutation_is_even(arr, 15)) {
        for (int i = 0; i < 15-1; i++) {
            int xchg_idx = randint(i, 15-1);
            int tmp = arr[i];
            arr[i] = arr[xchg_idx];
            arr[xchg_idx] = tmp;
        }
    }

    // TODO: reorder buttons by generated permutation
}

bool FifteenBoard::IsBeaten()
{
    for (int i = 0; i < 15; i++) {
        if (btns[i/4][i%4]->GetNumber() != i+1)
            return false;
    }

    return true;
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

    FifteenBoard board(3, 3);

    win->resizable(win);
    win->size_range(win_sz/2, win_sz/2, 2*win_sz, 2*win_sz, 0, 0, true);
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
