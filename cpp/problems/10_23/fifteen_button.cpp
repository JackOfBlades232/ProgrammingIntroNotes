/* 10_23/fifteen_button.cpp */
#include "fifteen.h"

FifteenButton::FifteenButton(FifteenBoard *brd, Fl_Callback cbk,
        int a_number, int a_xidx, int a_yidx)
    : Fl_Button(idx_to_coord(a_xidx), idx_to_coord(a_yidx), btn_sz, btn_sz),
    number(a_number), x_idx(a_xidx), y_idx(a_yidx)
{
    labelsize(font_size);
    UpdateLabel();
    callback(cbk, (void *)brd);
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
