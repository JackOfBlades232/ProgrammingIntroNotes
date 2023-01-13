/* 4_27.c */
#include <curses.h>
#include <stdio.h>
#include <stdlib.h>

enum { key_escape = 27, key_enter = '\n' };
enum { item_spacing = 1, vertical_padding = 2, left_padding = 4 };
enum { reg_color_pair = 1, selected_color_pair = 2 };
enum { fg = COLOR_WHITE, reg_bg = COLOR_BLACK, selected_bg = COLOR_BLUE };

struct menu {
    char **items;
    int f_idx, s_idx;
    int items_cnt, scr_cap;
};

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

static void draw_menu(struct menu *m)
{
    int i, y, max_i;

    max_i = min(m->items_cnt, m->f_idx + m->scr_cap);
    for (
            i = m->f_idx, y = vertical_padding; 
            i < max_i;
            i++, y += 1 + item_spacing
        ) {
        show_item(m->items[i], y, 
                i == m->s_idx ? selected_color_pair : reg_color_pair);
    }

    refresh();
}

static void switch_selected_item(struct menu *m, int delta_idx)
{
    m->s_idx += delta_idx;

    if (m->s_idx < 0)
        m->s_idx = 0;
    else if (m->s_idx >= m->items_cnt)
        m->s_idx = m->items_cnt - 1;

    if (m->s_idx < m->f_idx)
        m->f_idx = m->s_idx;
    else if (m->s_idx >= m->f_idx + m->scr_cap)
        m->f_idx = m->s_idx - m->scr_cap + 1;
}

static void check_terminal_size(int row)
{
    if (row < 2 * vertical_padding + 1) {
        endwin();
        fprintf(stderr, "Terminal must fit at least 1 item\n");
        exit(-3);
    }
}

static void on_resize(int *row, int *col, struct menu *m)
{
    getmaxyx(stdscr, *row, *col);
    check_terminal_size(*row);

    m->scr_cap = calculate_screen_capacity(*row);
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
    struct menu m;

    if (argc < 3 || argc > 101) {
        fprintf(stderr, "Provide from 2 to 100 arguments\n");
        return -1;
    }

    init_curses(&row, &col);
    init_colors();

    m.items = argv + 1;
    m.items_cnt = argc - 1;
    m.scr_cap = calculate_screen_capacity(row);
    m.f_idx = 0;
    m.s_idx = 0;

    draw_menu(&m);

    while ((key = getch()) != key_escape) {
        switch (key) {
            case key_enter:
                endwin();
                return m.s_idx + 1;
            case KEY_UP:
                switch_selected_item(&m, -1);
                break;
            case KEY_DOWN:
                switch_selected_item(&m, 1);
                break;
            case KEY_RESIZE:
                on_resize(&row, &col, &m);
                break;
            default:
                continue;
        }

        erase();
        draw_menu(&m);
    }

    endwin();
    return 0;
}
