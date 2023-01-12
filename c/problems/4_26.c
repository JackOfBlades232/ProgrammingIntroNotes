/* 4_26.c */
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>

enum { key_escape = 27 };
enum { square_size = 5 };
enum { color_count = 8 };

static const int all_colors[color_count] = {
    COLOR_BLACK, COLOR_RED, COLOR_GREEN, COLOR_YELLOW,
    COLOR_BLUE, COLOR_MAGENTA, COLOR_CYAN, COLOR_WHITE
};

static void set_pair(int fg_idx, int bg_idx)
{
    init_pair(1, all_colors[fg_idx], all_colors[bg_idx]);
}

static int modify_color_index(int idx, int delta)
{
    idx += delta;
    while (idx < 0)
        idx += color_count;
    while (idx >= color_count)
        idx -= color_count;

    return idx;
}

static void change_fg(int *fg_idx, int *bg_idx, int delta)
{
    *fg_idx = modify_color_index(*fg_idx, delta);
    set_pair(*fg_idx, *bg_idx);
    refresh();
}

static void change_bg(int *fg_idx, int *bg_idx, int delta)
{
    *bg_idx = modify_color_index(*bg_idx, delta);
    set_pair(*fg_idx, *bg_idx);
    refresh();
}

static void draw_square(int row, int col)
{
    int x, y;
    int min_x, max_x, min_y, max_y;

    min_x = (col - square_size)/2;
    min_y = (row - square_size)/2;
    max_x = min_x + square_size - 1;
    max_y = min_y + square_size - 1;

    for (y = min_y; y <= max_y; y++) {
        for (x = min_x; x <= max_x; x++) {
            move(y, x);
            addch('*');
        }
    }

    refresh();
}

static void check_terminal_size(int row, int col)
{
    if (row < square_size || col < square_size) {
        endwin();
        fprintf(stderr, "Terminal must be at least %dx%d\n",
                square_size, square_size);
        exit(1);
    }
}

static void on_resize(int *row, int *col)
{
    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*row, *col);

    erase();
    draw_square(*row, *col);
}

static void init_curses(int *row, int *col)
{
    initscr();
    if (!has_colors()) {
        endwin();
        fprintf(stderr, "Can not show colors on a BW screen\n");
        exit(1);
    }
    cbreak();
    keypad(stdscr, 1);
    noecho();
    curs_set(0);
    start_color();

    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*row, *col);
}

int main()
{
    int row, col, key;
    int fg_idx, bg_idx;

    init_curses(&row, &col);
    
    fg_idx = color_count - 1;
    bg_idx = 0;
    set_pair(fg_idx, bg_idx);
    attrset(COLOR_PAIR(1));

    draw_square(row, col);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case KEY_UP:
                change_fg(&fg_idx, &bg_idx, 1);
                break;
            case KEY_DOWN:
                change_fg(&fg_idx, &bg_idx, -1);
                break;
            case KEY_LEFT:
                change_bg(&fg_idx, &bg_idx, -1);
                break;
            case KEY_RIGHT:
                change_bg(&fg_idx, &bg_idx, 1);
                break;
            case KEY_RESIZE:
                on_resize(&row, &col);
                break;
        }
    }

    endwin();
    return 0;
}
