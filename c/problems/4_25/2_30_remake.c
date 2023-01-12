/* 4_25/2_30_remake.c */
#include <curses.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>

enum {
    key_escape = 27,

    min_square_size = 1,
    min_square_padding = 1
};

struct square {
    int x, y;
    int width, height;
};

static void clamp_square(struct square *sq, int row, int col)
{
    if (sq->width < min_square_size)
        sq->width = min_square_size;
    if (sq->height < min_square_size)
        sq->height = min_square_size;

    if (sq->x < min_square_padding)
        sq->x = min_square_padding;
    if (sq->x > col - min_square_padding - 1)
        sq->x =  col - min_square_padding - 1;
    if (sq->x + sq->width > col - min_square_padding)
        sq->width = col - min_square_padding - sq->x;

    if (sq->y < min_square_padding)
        sq->y = min_square_padding;
    if (sq->y > row - min_square_padding - 1)
        sq->y =  row - min_square_padding - 1;
    if (sq->y + sq->height > row - min_square_padding)
        sq->height = row - min_square_padding - sq->y;
}

static void show_square(struct square *sq)
{
    int x, y; 

    for (y = sq->y; y < sq->y + sq->height; y++) {
        move(y, sq->x);
        for (x = 0; x < sq->width; x++)
            addch('*');
    }
}

static void on_resize(struct square *sq, int *row, int *col)
{
    getmaxyx(stdscr, *row, *col);
    clamp_square(sq, *row, *col);

    erase();
    show_square(sq);
    refresh();
}

static void check_delta_size(int delta_size)
{
    if (delta_size != 1 && delta_size != -1) {
        fprintf(stderr, "Attempted to resize square not by modulo 1\n");
        endwin();
        exit(1);
    }
}

static void resize_square_vertical(struct square *sq, 
        int row, int col, int delta_size)
{
    check_delta_size(delta_size);

    sq->height += 2 * delta_size;
    sq->y -= delta_size;
    clamp_square(sq, row, col);
    
    erase();
    show_square(sq);
    refresh();
}

static void expand_square_vertical(struct square *sq, int row, int col)
{
    resize_square_vertical(sq, row, col, 1);
}

static void contract_square_vertical(struct square *sq, int row, int col)
{
    resize_square_vertical(sq, row, col, -1);
}

static void resize_square_horizontal(struct square *sq, 
        int row, int col, int delta_size)
{
    check_delta_size(delta_size);

    sq->width += 2 * delta_size;
    sq->x -= delta_size;
    clamp_square(sq, row, col);

    erase();
    show_square(sq);
    refresh();
}

static void expand_square_horizontal(struct square *sq, int row, int col)
{
    resize_square_horizontal(sq, row, col, 1);
}

static void contract_square_horizontal(struct square *sq, int row, int col)
{
    resize_square_horizontal(sq, row, col, -1);
}

static void init_ncurses(int *row, int *col)
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
    struct square sq;

    init_ncurses(&row, &col);

    sq.x = (col - min_square_size)/2;  
    sq.y = (row - min_square_size)/2;  
    sq.width = sq.height = min_square_size;
    show_square(&sq);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case KEY_UP:
                expand_square_vertical(&sq, row, col);
                break;
            case KEY_DOWN:
                contract_square_vertical(&sq, row, col);
                break;
            case KEY_LEFT:
                contract_square_horizontal(&sq, row, col);
                break;
            case KEY_RIGHT:
                expand_square_horizontal(&sq, row, col);
                break;
            case KEY_RESIZE:
                on_resize(&sq, &row, &col);
                break;
        }
    }

    endwin();
    return 0;
}
