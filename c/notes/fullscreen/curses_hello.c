/* curses_hello.c */
#include <curses.h>
#include <unistd.h>

const char message[] = "Hello, world!";
enum { delay_duration = 5 };

int main()
{
    int row, col;

    initscr();
    getmaxyx(stdscr, row, col); /* this is a macro, not a func */
    move(row/2, (col-(sizeof(message)-1))/2);
    addstr(message);
    curs_set(0); /* hide cursor */
    refresh(); /* apply changes to screen */

    sleep(delay_duration); /* not a curses function, from unistd */
    endwin();
    
    return 0;
}
