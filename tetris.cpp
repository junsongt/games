#include <curses.h>

#include <cstdlib>
#include <iostream>
#include <vector>
using namespace std;

bool gameover;
bool quit;
const int width = 20, height = 20;
int score;
int block[4];  // coords of a dropping block

enum directions { STOP = 0,
                  LEFT,
                  RIGHT,
                  DOWN,
                  FLIP };
directions dir;
enum shapes { O,
              T,
              S,
              Z,
              J,
              L,
              I };
shapes sh;

struct block {
    shapes shape;
    int coords[4];
    block(shapes sh) { this->shape = sh; };
};

void init() {
    gameover = false;
    quit = false;
    dir = STOP;

    score = 0;
}

void draw() {
    clear();
    if (gameover) {
        printw("GAME OVER! Your score: %d", score);
        printw("\nPress 'Q' to quit; Press 'R' to restart");
    } else {
        // TODO
    }

    refresh();
    return;
}

void input() {
    switch (getch()) {
        case KEY_UP:
            dir = FLIP;
            break;
        case KEY_DOWN:
            dir = DOWN;
            break;
        case KEY_LEFT:
            dir = LEFT;
            break;
        case KEY_RIGHT:
            dir = RIGHT;
            break;
        case 'q':
            quit = true;
            break;
        case 'r':
            init();
            break;
    }
    return;
}

void logic() {
    switch (dir) {
        case LEFT:

            break;
        case RIGHT:

            break;
        case FLIP:

            break;
        case DOWN:

            break;
        default:
            break;
    }

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
        draw();
        input();
        logic();
        napms(20);
    }
    endwin();
    return 0;
}