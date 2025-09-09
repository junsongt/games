#include <curses.h>
#include <unistd.h>

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <iostream>
#include <thread>
using namespace std;

bool gameover;
bool quit;
const int WIDTH = 11, HEIGHT = 11;
bool drop;  // true if the block can drop; false if block is onhold during sleep time

const int nspeed = 20;
int speed_counter = 0;

// grid
int grid[HEIGHT][WIDTH];

void init() {
    gameover = false;
    quit = false;


    for (int i = 0; i < HEIGHT; ++i) {
        for (int j = 0; j < WIDTH; ++j) {
            grid[i][j] = 0;
        }
    }

    return;
}

void draw() {
    // clear();
    // if (gameover) {
    //     printw("GAME OVER! You lose!");
    //     printw("\nPress 'Q' to quit; Press 'R' to restart");
    // } else {
    //     // row index ranging from [0, HEIGHT-1], which is with the fence constraint, which is consistent with grid's height: HEIGHT
    //     for (int i = 0; i < HEIGHT; ++i) {
    //         mvaddch(i, 0, '|');  // side fence
    //         // but column index is ranging from [0, WIDTH+1], including side fences on left & right, while index: [1, WIDTH] is within fence constraint, and consistent with grid's width: WIDTH.
    //         // so whenver accessing the graphic column index: j, (j-1) corresponds to the grid's cloumn index
    //         for (int j = 1; j < WIDTH + 1; ++j) {
    //             char ch = ' ';
    //             if (grid[i][j - 1] == 1) {
    //                 ch = '@';
    //             }
    //             for (int bi = 0; bi < 4; ++bi) {
    //                 for (int bj = 0; bj < 4; ++bj) {
    //                     int x = bx + bj;
    //                     int y = by + bi;
    //                     if (x == j - 1 && y == i && block[bi][bj]) {
    //                         ch = '@';
    //                     }
    //                 }
    //             }
    //             mvaddch(i, j, ch);
    //         }
    //         mvaddch(i, WIDTH + 1, '|');  // side fence
    //     }
    //     // botton fence
    //     for (int i = 0; i < WIDTH + 2; ++i) {
    //         mvaddch(HEIGHT, i, '=');
    //     }
    // }
    // refresh();
    // return;
}

void input() {
    return;
}

void logic() {
    return;
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
