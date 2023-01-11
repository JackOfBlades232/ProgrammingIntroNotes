/* 4_25/2_27_remake.c */
#include <curses.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>

enum { 
    key_escape = 27,
    key_space = 32,
    base_delay = 500,
    min_delay = 100,
    max_delay = 2500,
    delay_step = 200,
    square_size = 10,
    min_term_size = 12
};

typedef struct tag_square {
    int min_x, min_y, max_x, max_y;
} square;

typedef struct tag_token {
    int cur_x, cur_y;
    int dx, dy;
} token;

static void show_square(square *sq)
{
    int x, y;

    for (y = sq->min_y; y <= sq->max_y; y++) {
        move(y, sq->min_x);
        for (x = sq->min_x; x <= sq->max_x; x++)
            addch('*');
    }

    refresh();
}

static void draw_token(token *t, int symbol)
{
    move(t->cur_y, t->cur_x);
    addch(symbol);
    refresh();
}

static void show_token(token *t)
{
    draw_token(t, '#');
}

static void hide_token(token *t)
{
    draw_token(t, '*');
}

static void try_pivot_token(token *t, square *sq)
{
    if ((t->dx < 0 && t->cur_x <= sq->min_x) ||
            (t->dx > 0 && t->cur_x >= sq->max_x)) {
        t->dx = 0;
        if (t->cur_y <= sq->min_y)
            t->dy = 1;
        else if (t->cur_y >= sq->max_y)
            t->dy = -1;
    } else if ((t->dy < 0 && t->cur_y <= sq->min_y) ||
            (t->dy > 0 && t->cur_y >= sq->max_y)) {
        t->dy = 0;
        if (t->cur_x <= sq->min_x)
            t->dx = 1;
        else if (t->cur_x >= sq->max_x)
            t->dx = -1;
    }
}

static void move_token(token *t, square *sq)
{
    try_pivot_token(t, sq);
    
    hide_token(t);
    t->cur_x += t->dx;
    t->cur_y += t->dy;
    show_token(t);
}

static void change_delay(int *delay, int amount)
{
    *delay += amount;

    if (*delay < min_delay)
        *delay = min_delay;
    else if (*delay > max_delay)
        *delay = max_delay;

    timeout(*delay);
}

static void check_terminal_size(int col, int row)
{
    if (row < min_term_size || col < min_term_size) {
        endwin();
        fprintf(stderr, "Terminal is too small for the square\n");
        exit(1);
    }
}

static void on_resize(int *col, int *row)
{
    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*col, *row);
}

static void init_ncurses(int *col, int *row)
{
    initscr();
    cbreak();
    keypad(stdscr, 1);
    noecho();
    curs_set(0);
    getmaxyx(stdscr, *row, *col);
}

int main()
{
    int row, col, key;
    int delay;
    square sq;
    token t;

    init_ncurses(&col, &row);
    check_terminal_size(col, row);

    delay = base_delay;
    timeout(base_delay);

    sq.min_x = (col-square_size)/2 - 1;
    sq.max_x = sq.min_x + square_size - 1;
    sq.min_y = (row-square_size)/2 - 1;
    sq.max_y = sq.min_y + square_size - 1;

    t.cur_x = sq.min_x;
    t.cur_y = sq.min_y;
    t.dx = 0;
    t.dy = 1;

    show_square(&sq);
    show_token(&t);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case key_space:
                t.dx *= -1;
                t.dy *= -1;
                break;
            case KEY_LEFT:
                change_delay(&delay, delay_step);
                break;
            case KEY_RIGHT:
                change_delay(&delay, -delay_step);
                break;
            case KEY_RESIZE:
                on_resize(&col, &row);
        }

        move_token(&t, &sq);
    }

    endwin();
    return 0;
}
