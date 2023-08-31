/* 10_23/fifteen.h */
#ifndef FIFTEEN_SENTRY
#define FIFTEEN_SENTRY

#include "constants.h"

#include <FL/Fl.H>
#include <FL/Fl_Button.H>

class FifteenButton;
class FifteenHistory;

class FifteenBoard {
    int null_x, null_y;
    FifteenButton *btns[board_sz][board_sz];
    FifteenButton *ordered_btn_refs[filled_board_area]; // for ease of sorting
    FifteenHistory *history;    

public:
    FifteenBoard(Fl_Callback btn_cbk, const char *sv_filename = 0);
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
    FifteenButton(FifteenBoard *brd, Fl_Callback cbk,
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
    bool PopTurn(int *dest_x = 0, int *dest_y = 0);
    bool PeekTurn(int *dest_x, int *dest_y);
    bool TurnStackIsEmpty() { return !turn_stack; }
    int GetInitStateElem(int x, int y) { return init_board_state[y][x]; }

private:
    void WriteToSave();
    void WriteTurns(turn_t *head);
    bool ReadInitStateFromSave();
    bool ReadHistoryFromSave();
    void GenerateNewInitState();
};

#endif
