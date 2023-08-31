/* 10_23/main.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>
#include <FL/fl_ask.H>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <time.h>

/* TODO
 * Implement one quit function an wire it to game end, ESC and CTRL-C
 */

#define ASSERT(_e, _fmt, ...) \
    if(!(_e)) { \
        fprintf(stderr, \
                "Assertion failed at %s:%d : " _fmt "\n", \
                __FILE__, __LINE__, ##__VA_ARGS__); \
        exit(1); \
    }

#define ABS(_a) ((_a) < 0 ? -(_a) : (_a))

enum {
    btn_sz = 150,
    spacing = 15,
    padding = 75,
    font_size = 50,

    space_before_btns = 45,
    btn_w = 150,
    btn_h = 90,
    btn_spacing = 150,

    board_sz = 4,
    board_area = board_sz*board_sz,
    filled_board_area = board_area-1
};

static inline int randint(int min, int max)
{
    return min + (int)((float)(max-min+1) * rand() / (RAND_MAX+1.0));
}

static inline int idx_to_coord(int idx)
{ 
    return padding + idx*btn_sz + idx*spacing; 
}

static inline int xy_to_lin_idx(int x, int y)
{ 
    return board_sz*y + x; 
}

static void btn_callback(Fl_Widget *w, void *user);
static void undo_callback(Fl_Widget *w, void *user);
static void reset_callback(Fl_Widget *w, void *user);

class FifteenButton;

class FifteenBoard {
    int null_x, null_y;
    FifteenButton *btns[board_sz][board_sz];
    FifteenButton *ordered_btn_refs[board_area]; // for ease of sorting


    struct turn_t {
        int dest_x, dest_y;
        turn_t *prev;
    };
    int init_board_state[board_sz][board_sz] = { 0 };
    turn_t *turn_stack = 0;


public:
    FifteenBoard();
    void Reset();
    bool IsBeaten();
    void TryMoveBtnToFreeSpot(FifteenButton *btn);
    void TryUndoTurn();
};

class FifteenButton : public Fl_Button {
    friend class FifteenBoard;

    int number;
    int x_idx, y_idx;

public:
    FifteenButton(FifteenBoard *brd,
            int a_number, int a_xidx, int a_yidx);
    void Move(int new_xidx, int new_yidx);

private:
    void UpdateLabel();
};

static void quit_app(Fl_Widget *w);
static void btn_callback(Fl_Widget *w, void *user)
{
    FifteenButton *btn = static_cast<FifteenButton *>(w);
    FifteenBoard *brd = (FifteenBoard *)user;

    brd->TryMoveBtnToFreeSpot(btn);

    if (brd->IsBeaten()) {
        int ask_res = fl_choice("You've won! Restart?", fl_no, fl_yes, 0);
        switch (ask_res) {
            case 0: // No
                quit_app(w);        
                break;
            case 1: // Yes
            default:
                brd->Reset();
        }
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

static void undo_callback(Fl_Widget *w, void *user)
{
    ((FifteenBoard *)user)->TryUndoTurn();
}

static void reset_callback(Fl_Widget *w, void *user)
{
    ((FifteenBoard *)user)->Reset();
}

FifteenBoard::FifteenBoard() : null_x(board_sz-1), null_y(board_sz-1)
{
    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            if (x == null_x && y == null_y) {
                btns[y][x] = 0;
                continue;
            }

            int lin_idx = xy_to_lin_idx(x, y);
            btns[y][x] = new FifteenButton(this, lin_idx+1, x, y);
            ordered_btn_refs[lin_idx] = btns[y][x];
        }
}

static bool permutation_is_id(int *perm, int size)
{
    for (int i = 0; i < size; i++) {
        if (perm[i] != i)
            return false;
    }

    return true;
}

static bool permutation_is_odd(int *perm, int size)
{
    // Since the array is 15 el long, it's ok to use the O(n^2) inv counting
    int inv_cnt = 0;
    for (int i = 0; i < size-1; i++)
        for (int j = i+1; j < size; j++) {
            if (perm[i] > perm[j])
                inv_cnt++;
        }
            
    return inv_cnt % 2 == 1;
}

static void shuffle_permutation(int *perm, int size)
{
    for (int i = 0; i < size-1; i++) {
        int xchg_idx = randint(i, size-1);
        int tmp = perm[i];
        perm[i] = perm[xchg_idx];
        perm[xchg_idx] = tmp;
    }
}

static void generate_even_permutation(int *perm, int size)
{
    for (int i = 0; i < filled_board_area; i++)
        perm[i] = i;

    while (
            permutation_is_id(perm, filled_board_area) || 
            permutation_is_odd(perm, filled_board_area)
          )
    {
        shuffle_permutation(perm, filled_board_area);
    }
}

void FifteenBoard::Reset()
{
    int perm[filled_board_area];
    generate_even_permutation(perm, filled_board_area);

    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            int i = xy_to_lin_idx(x, y); 
            if (i < filled_board_area) {
                btns[y][x] = ordered_btn_refs[perm[i]];
                btns[y][x]->Move(x, y);
            }
            else 
                btns[y][x] = 0;


            init_board_state[y][x] = btns[y][x] ? btns[y][x]->number : 0;
        }
}

bool FifteenBoard::IsBeaten()
{
    for (int i = 0; i < filled_board_area; i++) {
        FifteenButton *btn = btns[i/board_sz][i%board_sz];
        if (!btn || btn->number != i+1)
            return false;
    }

    return true;
}

void FifteenBoard::TryMoveBtnToFreeSpot(FifteenButton *btn)
{
    if (ABS(btn->x_idx - null_x) + ABS(btn->y_idx - null_y) > 1)
        return;


    turn_t *turn = new turn_t;
    turn->dest_x = null_x;
    turn->dest_y = null_y;
    turn->prev = turn_stack;
    turn_stack = turn;


    int btn_prev_x = btn->x_idx;
    int btn_prev_y = btn->y_idx;

    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = btn_prev_x;
    null_y = btn_prev_y;
    btns[null_y][null_x] = 0;
}

void FifteenBoard::TryUndoTurn()
{
    if (!turn_stack)
        return;

    FifteenButton *btn = btns[turn_stack->dest_y][turn_stack->dest_x];
    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = turn_stack->dest_x;
    null_y = turn_stack->dest_y;
    btns[null_y][null_x] = 0;


    turn_t *tmp = turn_stack;
    turn_stack = turn_stack->prev;
    delete tmp;
}

FifteenButton::FifteenButton(FifteenBoard *brd,
        int a_number, int a_xidx, int a_yidx)
    : Fl_Button(idx_to_coord(a_xidx), idx_to_coord(a_yidx), btn_sz, btn_sz),
    number(a_number), x_idx(a_xidx), y_idx(a_yidx)
{
    labelsize(font_size);
    UpdateLabel();
    callback(btn_callback, (void *)brd);
}

void FifteenButton::Move(int new_xidx, int new_yidx)
{
    x_idx = new_xidx;
    y_idx = new_yidx;
    hide();
    position(idx_to_coord(x_idx), idx_to_coord(y_idx));
    show();
}

void FifteenButton::UpdateLabel()
{
    char buf[32];
    int lin_idx = xy_to_lin_idx(x_idx, y_idx);
    snprintf(buf, sizeof(buf), "%d", lin_idx + 1);
    copy_label(buf);
}

int main(int argc, char **argv)
{
    srand(time(0));

    int win_w = btn_sz*board_sz + spacing*(board_sz-1) + padding*2;
    int win_h = win_w + space_before_btns + btn_h;
    Fl_Window *win = new Fl_Window(win_w, win_h, "10_23 (fifteen)");

    // All the required widgets are set up inside board
    FifteenBoard board;
    board.Reset();


    int btn_x = win_w - 2*padding - 2*btn_w - btn_spacing;
    int btn_y = win_h - padding - btn_h;

    Fl_Button *undo_btn = 
        new Fl_Button(btn_x, btn_y, btn_w, btn_h, "@undo");

    btn_x += btn_w + btn_spacing;
    Fl_Button *reset_btn = 
        new Fl_Button(btn_x, btn_y, btn_w, btn_h, "@reload");
    
    undo_btn->labelsize(font_size);
    reset_btn->labelsize(font_size);
    undo_btn->callback(undo_callback, (void *) &board);
    reset_btn->callback(reset_callback, (void *) &board);


    win->resizable(0);
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
