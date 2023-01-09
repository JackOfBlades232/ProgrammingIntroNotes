/* 4_25/2_27_remake.c */
#include <curses.h>
#include <ncurses.h>

enum { 
    delay_duration = 100,
    key_escape = 27,
    square_size = 10
};

typedef struct tag_square {
    int min_x, min_y, max_x, max_y;
} square;

typedef struct tag_token {
    int cur_x, cur_y;
    int dx, dy;
} token;

static void draw_square(square *sq, int symbol)
{
    int x, y;

    for (y = sq->min_y; y <= sq->max_y; y++) {
        move(y, sq->min_x);
        for (x = sq->min_x; x <= sq->max_x; x++)
            addch(symbol);
    }
}
        
static void show_square(square *sq)
{
    draw_square(sq, '*');
}

static void hide_square(square *sq)
{
    draw_square(sq, ' ');
}

static void on_resize(int *col, int *row)
{
}

static void init_ncurses(int *col, int *row)
{
    initscr();
    cbreak();
    timeout(delay_duration);
    keypad(stdscr, 1);
    noecho();
    curs_set(0);
    getmaxyx(stdscr, *row, *col);
}

int main()
{
    int row, col;

    init_ncurses(&col, &row);

    endwin();
    return 0;
}
