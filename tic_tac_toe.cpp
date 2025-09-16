#include <curses.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <thread>
using namespace std;

bool lose; // you lose
bool win; // you win
bool quit;
const int WIDTH = 7, HEIGHT = 7;
const int SIZE = 3;
bool drop;  // true if the block can drop; false if block is onhold during sleep time
int x, y;

const int nspeed = 20;
int speed_counter = 0;

// grid
int grid[SIZE][SIZE];


// return true if there exists one rows forming a line
bool row_check(int id) {
    for (int i = 0; i < SIZE; ++i) {
        bool form_row = true;
        for (int j = 0; j < SIZE; ++j) {
            if (grid[i][j] != id) {
                form_row = false;
            }
        }
        if (form_row == true) {
            return true;
        }
    }
    return false;
}

// return true if all the columns don't form a line
bool col_check(int id) {
    for (int j = 0; j < SIZE; ++j) {
        bool form_col = true;
        for (int i = 0; i < SIZE; ++i) {
            if (grid[i][j] != id) {
                form_col = false;
            }
        }
        if (form_col == true) {
            return true;
        }
    }
    return false;

}
bool diag_check(int id) {
    bool diag = true;
    for (int i = 0; i < SIZE; ++i) {
        if (grid[i][i] != id) {
            diag = false;
        }
    }
    bool off_diag = true;
    for (int i = 0; i < SIZE; ++i) {
        if (grid[i][SIZE-i] != id) {
            off_diag = false;
        }
    }
    return diag || off_diag;
}

bool judge_win(int id) {
    return row_check(id) || col_check(id) || diag_check(id);
}

// settle the position and engage with opponent back and forth in 1 round
void engage() {
    /*
    For any pos: (x, y), [(x-1)/2, (y-1)/2] correspond to the pos in grid
    */

    // settle usr's coord

    // settle bot's coord by searching best move

}




void init() {
    lose = false;
    win = false;
    quit = false;
    // printw("Select mode: One player/Two players(o/t)");

    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            grid[i][j] = 0;
        }
    }

}

void draw() {
    clear();
    if (lose) {
        printw("GAME OVER! You lose!");
        printw("\nPress 'Q' to quit; Press 'R' to restart");
    } 
    else if (win) {
        printw("CONGRATULATIONS! You win!");
        printw("\nPress 'Q' to quit; Press 'R' to restart");
    }
    else {
        for (int i = 0; i < HEIGHT; ++i) {
            for (int j = 0; j < WIDTH; ++j) {
                if (i % 2 == 0 && j % 2 == 0) {
                    mvaddch(i, j, '+');
                }
                else if (i % 2 == 0 && j % 2 != 0) {
                    mvaddch(i, j, '-');
                }
                else if (i % 2 != 0 && j % 2 == 0) {
                    mvaddch(i, j, '|');
                }
                else {

                    // render the positions of usr and oppo
                    mvaddch(i, j, ' ');
                }
            }
        }
    }
    refresh();
}

void input() {
    int key = getch();
    switch (key) {
        case KEY_UP:
            // moving the potential position up
            y -= 2;
            break;
        case KEY_DOWN:
            // moving the potential position down
            y += 2;
            break;
        case KEY_LEFT:
            // moving the potential position left
            x -= 2;
            break;
        case KEY_RIGHT:
            // moving the potential position right
            x += 2;
            break;
        case KEY_ENTER:
            // settle the position and engage with opponent back and forth in 1 round
            engage();
            break;
        case 'q':
            quit = true;
            break;
        case 'r':
            init();
            break;
    }

}

void logic() {
    if (judge_win(1) && !judge_win(2)) {
        win = true;
    }
    else if (!judge_win(1) && judge_win(2)) {
        lose = true;
    }
    else {
        // coord can't exceed boundary


    }
}

int main() {
    initscr();              // init ncurses
    cbreak();               // no line buffering
    noecho();               // don't echo keys
    keypad(stdscr, TRUE);   // enable KEY_UP, etc.
    nodelay(stdscr, TRUE);  // make getch() non-blocking (optional)
    curs_set(0);            // hide cursor (optional)

    init();
    while (!quit) {
        this_thread::sleep_for(50ms);
        speed_counter++;
        drop = (speed_counter == nspeed);
        draw();
        input();
        logic();
        // napms(40);
    }
    endwin();

    return 0;
}
