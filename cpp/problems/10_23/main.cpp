/* 10_23/main.cpp */
#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/Fl_Box.H>
#include <FL/fl_ask.H>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>

/* TODO
 * Unify in-game history with file history (offset by 1)
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

static const char default_save_path[] = "s.sav";

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

static void quit_app(Fl_Widget *w);

static void btn_callback(Fl_Widget *w, void *user);
static void undo_callback(Fl_Widget *w, void *user);
static void reset_callback(Fl_Widget *w, void *user);

static void generate_even_permutation(int *perm, int size);
static bool permutation_is_id(int *perm, int size);
static bool permutation_is_odd(int *perm, int size);
static void shuffle_permutation(int *perm, int size);

class FifteenButton;
class FifteenHistory;

class FifteenBoard {
    int null_x, null_y;
    FifteenButton *btns[board_sz][board_sz];
    FifteenButton *ordered_btn_refs[filled_board_area]; // for ease of sorting
    FifteenHistory *history;    

public:
    FifteenBoard(const char *sv_filename = 0);
    ~FifteenBoard();
    void Init();
    void Reset();
    bool IsBeaten();
    bool TryMoveBtnToFreeSpot(FifteenButton *btn);
    void TryUndoTurn();
    int GetNullX() { return null_x; }
    int GetNullY() { return null_y; }

private:
    void ResetState();
    bool ReplayHistory();
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

class FifteenHistory {
    FifteenBoard *master;
    FILE *save_f;

    struct turn_t {
        int dest_x, dest_y;
        turn_t *prev;
    };
    int init_board_state[board_sz][board_sz] = { 0 };
    turn_t *turn_stack;

public:
    FifteenHistory(FifteenBoard *a_master, const char *sv_filename = 0);
    ~FifteenHistory();
    void ResetHistory();
    void PushTurn(int dest_x, int dest_y);
    bool PopTurn(int *dest_x, int *dest_y);
    int GetInitStateElem(int x, int y) { return init_board_state[y][x]; }

private:
    void WriteToSave();
    void WriteTurns(turn_t *head);
    bool ReadInitStateFromSave();
    bool ReadHistoryFromSave();
    void GenerateNewInitState();
};

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

FifteenBoard::FifteenBoard(const char *sv_filename) 
    : null_x(board_sz-1), null_y(board_sz-1)
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

    history = new FifteenHistory(this, sv_filename);
}

FifteenBoard::~FifteenBoard() 
{ 
    delete history;
}

void FifteenBoard::Init()
{
    ResetState();
    if (!ReplayHistory()) {
        fprintf(stderr, "Failed to replay history, reverting to init state\n");
        Reset();
    }
}

void FifteenBoard::Reset()
{
    ResetState();
    history->ResetHistory();
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

bool FifteenBoard::TryMoveBtnToFreeSpot(FifteenButton *btn)
{
    if (!btn)
        return false;
    if (ABS(btn->x_idx - null_x) + ABS(btn->y_idx - null_y) > 1)
        return false;

    history->PushTurn(null_x, null_y);

    int btn_prev_x = btn->x_idx;
    int btn_prev_y = btn->y_idx;

    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = btn_prev_x;
    null_y = btn_prev_y;
    btns[null_y][null_x] = 0;

    return true;
}

void FifteenBoard::TryUndoTurn()
{
    int dest_x, dest_y;

    bool res = history->PopTurn(&dest_x, &dest_y);
    if (!res)
        return;

    FifteenButton *btn = btns[dest_y][dest_x];
    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = dest_x;
    null_y = dest_y;
    btns[null_y][null_x] = 0;
}

void FifteenBoard::ResetState()
{
    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            int elem = history->GetInitStateElem(x, y);
            if (elem) {
                btns[y][x] = ordered_btn_refs[elem-1];
                btns[y][x]->Move(x, y);
            }
            else 
                btns[y][x] = 0;
        }

    null_x = board_sz-1;
    null_y = board_sz-1;
}

bool FifteenBoard::ReplayHistory()
{
    // A hacky recursive linked list "inversion"
    // The replayable history is different: it stores "played" buttons, 
    // not the history of null-buttons. It gets corrected as we unroll.
    int x, y;
    if (!history->PopTurn(&x, &y))
        return true;

    if (!ReplayHistory())
        return false;
    
    return TryMoveBtnToFreeSpot(btns[y][x]);
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

FifteenHistory::FifteenHistory(FifteenBoard *a_master, const char *sv_filename)
    : master(a_master), save_f(0), turn_stack(0) 
{
    if (sv_filename) {
        save_f = fopen(sv_filename, "r");
        if (!save_f) {
            fprintf(stderr, 
                    "Couldn't open save file, reverting to generation\n");
            goto generate;
        }

        int read_res = ReadInitStateFromSave() && ReadHistoryFromSave();
        if (!read_res) {
            fprintf(stderr, "Save file invalid, reverting to generation\n");
            goto generate;
        }

        return;
    }

    // else
generate:
    GenerateNewInitState();
}

FifteenHistory::~FifteenHistory()
{
    WriteToSave();
    ResetHistory();
}

void FifteenHistory::ResetHistory()
{
    while (turn_stack) {
        turn_t *tmp = turn_stack;
        turn_stack = turn_stack->prev;
        delete tmp;
    }

    turn_stack = 0;
}

void FifteenHistory::PushTurn(int dest_x, int dest_y)
{
    turn_t *turn = new turn_t;
    turn->dest_x = dest_x;
    turn->dest_y = dest_y;
    turn->prev = turn_stack;
    turn_stack = turn;
}

bool FifteenHistory::PopTurn(int *dest_x, int *dest_y)
{
    if (!turn_stack)
        return false;

    turn_t *tmp = turn_stack;
    *dest_x = tmp->dest_x;
    *dest_y = tmp->dest_y;
    turn_stack = turn_stack->prev;
    delete tmp;

    return true;
}

void FifteenHistory::WriteToSave()
{
    if (save_f)
        save_f = freopen(0, "w", save_f);
    else
        save_f = fopen(default_save_path, "w");

    if (!save_f) {
        fprintf(stderr, 
                "Failed to write save, your latest progress was lost\n");
        return;
    }
    
    for (int y = 0; y < board_sz; y++) {
        for (int x = 0; x < board_sz; x++) {
            fprintf(save_f, x == board_sz-1 ? "%d" : "%d ",
                    init_board_state[y][x]);
        }
        fputc('\n', save_f);
    }

    fputc('\n', save_f);

    WriteTurns(turn_stack);
    // Last turn is not depicted on the stack as such
    if (turn_stack)
        fprintf(save_f, "%d %d\n", master->GetNullX(), master->GetNullY());

    fclose(save_f);
}

void FifteenHistory::WriteTurns(turn_t *head)
{
    // We omit first turn, cause it holds info about init null pos,
    // not about player actions
    if (!head || !head->prev)
        return;

    WriteTurns(head->prev);
    fprintf(save_f, "%d %d\n", head->dest_x, head->dest_y);
}

bool FifteenHistory::ReadInitStateFromSave()
{
    bool number_indicators[board_area] = { 0 };
    int perm[filled_board_area] = { 0 };

    for (int y = 0; y < board_sz; y++) {
        char buf[64];
        if (!fgets(buf, sizeof(buf), save_f))
            return false;

        char *p = buf;
        char *endptr = p;
        for (int x = 0; x < board_sz; x++) {
            while (*p == ' ' || *p == '\t')
                p++;

            int n = strtol(p, &endptr, 10);

            // Correct format checks
            if (n == 0 && endptr == p)
                return false;
            if (n < 0 || n > filled_board_area)
                return false;
            if ((x != board_sz-1 || y != board_sz-1) && n == 0)
                return false;
            if (x == board_sz-1 && y == board_sz-1 && n != 0)
                return false;

            init_board_state[y][x] = n;

            if (number_indicators[n])
                return false;
            number_indicators[n] = true;
            if (x != board_sz || y != board_sz)
                perm[xy_to_lin_idx(x, y)] = n;

            p = endptr;
        }

        while (*p == ' ' || *p == '\t')
            p++;
        if (*p != '\n')
            return false;
    }

    // Init pos must be valid
    if (
            permutation_is_id(perm, filled_board_area) ||
            permutation_is_odd(perm, filled_board_area)
       )
    {
        return false;
    }

    return true;
}

bool FifteenHistory::ReadHistoryFromSave()
{
    int c;
    while ((c = fgetc(save_f)) != EOF) {
        if (c != ' ' && c != '\t' && c != '\r' && c != '\n')
            break;
    }
    if (c == EOF)
        return false;
    ungetc(c, save_f);

    for (;;) {
        char buf[64];
        if (!fgets(buf, sizeof(buf), save_f))
            break;

        int x, y;
        char *p = buf;
        char *endptr = p;

        while (*p == ' ' || *p == '\t')
            p++;
        x = strtol(p, &endptr, 10);
        if (x == 0 && endptr == p)
            return false;
        if (x < 0 || x >= board_sz)
            return false;
        p = endptr;

        while (*p == ' ' || *p == '\t')
            p++;
        y = strtol(p, &endptr, 10);
        if (y == 0 && endptr == p)
            return false;
        if (y < 0 || y >= board_sz)
            return false;
        p = endptr;

        PushTurn(x, y);

        while (*p == ' ' || *p == '\t')
            p++;
        if (*p != '\n')
            return false;
    }

    return true;
}

void FifteenHistory::GenerateNewInitState()
{
    int perm[filled_board_area];
    generate_even_permutation(perm, filled_board_area);

    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            int i = xy_to_lin_idx(x, y); 
            init_board_state[y][x] = i < filled_board_area ? perm[i]+1 : 0;
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

static void init_buttons(FifteenBoard *brd, int win_w, int win_h)
{
    int btn_x = (win_w - btn_spacing)/2 - btn_w;
    int btn_y = win_h - padding - btn_h;

    Fl_Button *undo_btn = 
        new Fl_Button(btn_x, btn_y, btn_w, btn_h, "@undo");

    btn_x += btn_w + btn_spacing;
    Fl_Button *reset_btn = 
        new Fl_Button(btn_x, btn_y, btn_w, btn_h, "@reload");
    
    undo_btn->labelsize(font_size);
    reset_btn->labelsize(font_size);
    undo_btn->callback(undo_callback, (void *)brd);
    reset_btn->callback(reset_callback, (void *)brd);
}

// Exit handling
static Fl_Window *glob_win_ref = 0;
void sigint_handler(int s)
{
    signal(SIGINT, sigint_handler);
    glob_win_ref->hide();
}

int main(int argc, char **argv)
{
    const char *sv_filename = 0;
    if (argc >= 2) {
        sv_filename = argv[1];
        argv++;
        argc--;
    }

    srand(time(0));
    signal(SIGINT, sigint_handler);

    int win_w = btn_sz*board_sz + spacing*(board_sz-1) + padding*2;
    int win_h = win_w + space_before_btns + btn_h;
    Fl_Window *win = new Fl_Window(win_w, win_h, "10_23 (fifteen)");

    glob_win_ref = win;

    // All the required widgets are set up inside board
    FifteenBoard board(sv_filename);
    board.Init();

    init_buttons(&board, win_w, win_h);

    win->resizable(0);
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
