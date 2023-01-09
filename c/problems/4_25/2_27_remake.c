/* 4_25/2_27_remake.c */
#include <curses.h>
#include <ncurses.h>

enum { delay_duration = 100 };
enum { num_passes = 6 };

static void show_star(int x, int y)
{
    move(y, x);
    addch('*');
    refresh();
}

static void hide_star(int x, int y)
{
    move(y, x);
    addch(' ');
    refresh();
}

static void truncate_xy(int *x, int *y, int mx, int my)
{
    if (*x < 0)
        *x = 0;
    else if (*x > mx)
        *x = mx;
    if (*y < 0)
        *y = 0;
    else if (*y > my)
        *y = my;
}

static void move_star(int *x, int *y, int dx, int dy, int max_x, int max_y)
{
    hide_star(*x, *y);
    *x += dx;
    *y += dy;
    truncate_xy(x, y, max_x, max_y);
    show_star(*x, *y);
}

static void on_resize(int *x, int *y, int *col, int *row)
{
    getmaxyx(stdscr, *row, *col);
    if (*x > *col)
        *x = *col;
    if (*y > *col)
        *y = *col;
}

static void handle_star_movement(int *x, int *y, int *dx, 
        int *pass_idx, int max_x, int max_y)
{
    move_star(x, y, *dx, 0, max_x, max_y);

    if (*x == 0 || *x == max_x) {
        (*pass_idx)++;
        *dx = *x == 0 ? 1 : -1;
    }
}

static void init_ncurses(int *col, int *row)
{
    initscr();
    cbreak();
    timeout(delay_duration);
    noecho();
    curs_set(0);
    getmaxyx(stdscr, *row, *col);
}

int main()
{
    int x, y, row, col, dx, pass_idx;

    init_ncurses(&col, &row);
    x = 0;
    y = row/2;
    dx = 1;
    pass_idx = 0;

    for (;;) {
        int key = getch();

        if (key == KEY_RESIZE)
            on_resize(&x, &y, &col, &row);
        else if (key != ERR)
            break;

        handle_star_movement(&x, &y, &dx, &pass_idx, col-1, row-1);
        if (pass_idx >= num_passes)
            break;
    }

    endwin();
    return 0;
}
