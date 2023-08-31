/* 10_23/fifteen_board.cpp */
#include "fifteen.h"
#include "utils.h"

FifteenBoard::FifteenBoard(Fl_Callback btn_cbk, const char *sv_filename) 
    : null_x(board_sz-1), null_y(board_sz-1)
{
    for (int y = 0; y < board_sz; y++)
        for (int x = 0; x < board_sz; x++) {
            if (x == null_x && y == null_y) {
                btns[y][x] = 0;
                continue;
            }

            int lin_idx = xy_to_lin_idx(x, y);
            btns[y][x] = new FifteenButton(this, btn_cbk, lin_idx+1, x, y);
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

    int btn_prev_x = btn->x_idx;
    int btn_prev_y = btn->y_idx;

    btns[null_y][null_x] = btn;
    btn->Move(null_x, null_y);

    null_x = btn_prev_x;
    null_y = btn_prev_y;
    btns[null_y][null_x] = 0;

    history->PushTurn(btn_prev_x, btn_prev_y);

    return true;
}

void FifteenBoard::TryUndoTurn()
{
    int dest_x, dest_y;

    bool res = history->PopTurn();
    if (!res)
        return;

    if (!history->PeekTurn(&dest_x, &dest_y)) {
        dest_x = board_sz-1;
        dest_y = board_sz-1;
    }

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
