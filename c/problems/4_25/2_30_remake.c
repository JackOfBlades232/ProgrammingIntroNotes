/* 4_25/2_27_remake.c */
#include <curses.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>

enum { 
    key_escape = 27,

    min_square_size = 1,
    base_square_size = 3,
    min_square_padding = 1
};

typedef struct tag_square {
    int min_x, min_y, max_x, max_y;
} square;

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

static void fill_vertical_line(int x, int min_y, int max_y, int symbol)
{
    int y;
    for (y = min_y; y <= max_y; y++) {
        move(y, x);
        addch(symbol);
    }
    refresh();
}

static void add_vertical_line(square *sq, int to_min)
{
    if (to_min) {
        sq->min_x -= 1;
        fill_vertical_line(sq->min_x, sq->min_y, sq->max_y, '*');
    } else {
        sq->max_x += 1;
        fill_vertical_line(sq->max_x, sq->min_y, sq->max_y, '*');
    }
}

static void remove_vertical_line(square *sq, int from_min)
{
    if (from_min) {
        sq->min_x += 1;
        fill_vertical_line(sq->min_x, sq->min_y, sq->max_y, ' ');
    } else {
        sq->max_x -= 1;
        fill_vertical_line(sq->max_x, sq->min_y, sq->max_y, ' ');
    }
}

static void fill_horizontal_line(int y, int min_x, int max_x, int symbol)
{
    int x;
    move(y, min_x);
    for (x = min_x; x <= max_x; x++) 
        addch(symbol);
    refresh();
}

static void add_horizontal_line(square *sq, int to_min)
{
    if (to_min) {
        sq->min_x -= 1;
        fill_horizontal_line(sq->min_y, sq->min_x, sq->max_x, '*');
    } else {
        sq->max_x += 1;
        fill_horizontal_line(sq->max_y, sq->min_x, sq->max_x, '*');
    }
}

static void remove_horizontal_line(square *sq, int from_min)
{
    if (from_min) {
        sq->min_x += 1;
        fill_horizontal_line(sq->min_y, sq->min_x, sq->max_x, ' ');
    } else {
        sq->max_x -= 1;
        fill_horizontal_line(sq->max_y, sq->min_x, sq->max_x, ' ');
    }
}

static void check_terminal_size(int col, int row)
{
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
    square sq;

    init_ncurses(&col, &row);
    check_terminal_size(col, row);

    sq.min_x = (col-base_square_size)/2 - 1;
    sq.max_x = sq.min_x + base_square_size - 1;
    sq.min_y = (row-base_square_size)/2 - 1;
    sq.max_y = sq.min_y + base_square_size - 1;

    show_square(&sq);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case KEY_UP:
                add_horizontal_line(&sq, 1);
                add_horizontal_line(&sq, 0);
                break;
            case KEY_DOWN:
                remove_horizontal_line(&sq, 1);
                remove_horizontal_line(&sq, 0);
                break;
            case KEY_LEFT:
                add_vertical_line(&sq, 0);
                add_vertical_line(&sq, 1);
                break;
            case KEY_RIGHT:
                remove_vertical_line(&sq, 0);
                remove_vertical_line(&sq, 1);
                break;
            case KEY_RESIZE:
                on_resize(&col, &row);
        }
    }

    endwin();
    return 0;
}
