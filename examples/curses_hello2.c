#include <curses.h>
#include <unistd.h>


const char message[] = "Hello, world!";
enum { delay_duration = 5 };

void say_hello()
{
    int row, col;
    getmaxyx(stdscr, row, col);
    move(row/2, (col-(sizeof(message)-1))/2);
    addstr(message);
    move(0, 0);
    refresh();
}

int main()
{
    int key;
    initscr();
    raw();
    noecho();
    say_hello();
    while((key = getch()) != 27) {
        if(key == KEY_RESIZE) {
            clear();
            say_hello();
        }
    }
    endwin();
    return 0;
}
