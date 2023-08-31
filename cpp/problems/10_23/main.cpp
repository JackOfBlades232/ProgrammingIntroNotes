/* 10_23/main.cpp */
#include "fifteen.h"
#include "utils.h"
#include "constants.h"

#include <FL/Fl.H>
#include <FL/Fl_Window.H>
#include <FL/Fl_Button.H>
#include <FL/fl_ask.H>

#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <time.h>

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

static void undo_callback(Fl_Widget *w, void *user)
{
    ((FifteenBoard *)user)->TryUndoTurn();
}

static void reset_callback(Fl_Widget *w, void *user)
{
    ((FifteenBoard *)user)->Reset();
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
    FifteenBoard board(btn_callback, sv_filename);
    board.Init();

    init_buttons(&board, win_w, win_h);

    win->resizable(0);
    win->end();
    win->show(argc, argv);
    return Fl::run();
}
