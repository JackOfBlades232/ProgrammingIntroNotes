/* 4_27.c */
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>

enum { key_escape = 27, key_enter = 13 };
enum { item_spacing = 1, vertical_padding = 2, left_padding = 4 };
enum { reg_color_pair = 1, selected_color_pair = 2 };
enum { fg = COLOR_WHITE, reg_bg = COLOR_BLACK, selected_bg = COLOR_BLUE };

static int min(int a, int b)
{
    return a < b ? a : b;
}

static int calculate_screen_capacity(int row)
{
    return (row - 2*vertical_padding + item_spacing) / (item_spacing + 1);
}

static void show_item(const char *item, int y, int c_pair)
{
    attrset(COLOR_PAIR(c_pair));
    move(y, left_padding);
    addstr(item);
}

static void draw_menu(char **items, int items_cnt,
        int first_idx, int selected_idx, int screen_capacity)
{
    int i, y, max_i;

    max_i = min(items_cnt, first_idx + screen_capacity);
    for (
            i = first_idx, y = vertical_padding; 
            i < max_i;
            i++, y += 1 + item_spacing
        ) {
        show_item(items[i], y, 
                i == selected_idx ? selected_color_pair : reg_color_pair);
    }

    refresh();
}

static void check_terminal_size(int row)
{
    if (row < 2 * vertical_padding + 1) {
        endwin();
        fprintf(stderr, "Terminal must fit at least 1 item\n");
        exit(-3);
    }
}

static void on_resize(int *row, int *col)
{
    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*row);
    erase();
}

static void init_curses(int *row, int *col)
{
    initscr();
    if (!has_colors()) {
        endwin();
        fprintf(stderr, "Can not show colors on a BW screen\n");
        exit(-2);
    }
    cbreak();
    keypad(stdscr, 1);
    noecho();
    curs_set(0);
    start_color();

    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*row);
}

static void init_colors()
{
    init_pair(reg_color_pair, fg, reg_bg);
    init_pair(selected_color_pair, fg, selected_bg);
}

int main(int argc, char **argv)
{
    int row, col, key;
    int screen_capacity, first_idx, selected_idx;

    if (argc < 3 || argc > 101) {
        fprintf(stderr, "Provide from 2 to 100 arguments\n");
        return -1;
    }

    init_curses(&row, &col);
    init_colors();
    screen_capacity = calculate_screen_capacity(row);

    /* testing */
    first_idx = 1;
    selected_idx = 1;
    draw_menu(argv, argc, first_idx, selected_idx, screen_capacity);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case KEY_RESIZE:
                on_resize(&row, &col);
                draw_menu(argv, argc, first_idx, selected_idx, screen_capacity);
        }
    }

    endwin();
    return 0;
}
