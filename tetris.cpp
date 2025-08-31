#include <curses.h>

#include <cstdlib>
#include <ctime>
#include <iostream>
using namespace std;

bool gameover;
bool quit;
const int WIDTH = 20, HEIGHT = 20;

// grid
int grid[HEIGHT][WIDTH] = {0};
// shapes
const int SHAPES[7][4][4] = {
    // I
    {
        {0, 0, 0, 0},
        {1, 1, 1, 1},
        {0, 0, 0, 0},
        {0, 0, 0, 0}},
    // O
    {
        {0, 0, 0, 0},
        {0, 1, 1, 0},
        {0, 1, 1, 0},
        {0, 0, 0, 0}},
    // T
    {
        {0, 0, 0, 0},
        {1, 1, 1, 0},
        {0, 1, 0, 0},
        {0, 0, 0, 0}},
    // S
    {
        {0, 0, 0, 0},
        {0, 1, 1, 0},
        {1, 1, 0, 0},
        {0, 0, 0, 0}},
    // Z
    {
        {0, 0, 0, 0},
        {0, 1, 1, 0},
        {0, 0, 1, 1},
        {0, 0, 0, 0}},
    // J
    {
        {0, 0, 0, 0},
        {1, 1, 1, 0},
        {0, 0, 1, 0},
        {0, 0, 0, 0}},
    // L
    {
        {0, 0, 0, 0},
        {1, 1, 1, 0},
        {1, 0, 0, 0},
        {0, 0, 0, 0}}};

int score;
int block[4][4] = {0};
int bx, by;

// helpers
void setShape(const int shape[4][4]) {
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            block[i][j] = shape[i][j];
        }
    }
    return;
}

// rotate block clockwise
void rotate() {
    int temp[4][4] = {0};
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            temp[j][3 - i] = block[i][j];
        }
    }

    // check or collision
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            if (temp[i][j]) {
                int x = bx + j;
                int y = by + i;
                if (x < 0 || x >= WIDTH || y < 0 || y >= HEIGHT || grid[y][x]) {
                    return;  // hits, cancel rotation
                }
            }
        }
    }

    // if no collision, apply rotation
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            block[i][j] = temp[i][j];
        }
    }
}

bool canMove(int dx, int dy) {
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            if (block[i][j]) {
                int x = bx + j + dx;
                int y = by + i + dy;
                if (x < 0 || x >= WIDTH || y >= HEIGHT || (y >= 0 && grid[y][x])) {
                    return false;
                }
            }
        }
    }
    return true;
}

void placeBlock() {
    for (int i = 0; i < 4; ++i) {
        for (int j = 0; j < 4; ++j) {
            if (block[i][j]) {
                int x = bx + j;
                int y = by + i;
                if (y >= 0) {
                    grid[y][x] = 1;
                }
            }
        }
    }
}

void clearlines() {
    for (int i = HEIGHT - 1; i >= 0; --i) {
        bool full = true;
        for (int j = 0; j < WIDTH; ++j) {
            if (grid[i][j] == 0) {
                full = false;
            }
        }

        if (full) {
            // drop the stack
            for (int k = i; k > 0; --k) {
                for (int j = 0; j < WIDTH; ++j) {
                    grid[k][j] = grid[k - 1][j];  // after clear current line, the line above drops one level
                }
            }
            // after top level drops, empty level comes in
            for (int j = 0; j < WIDTH; ++j) {
                grid[0][j] = 0;
            }
            i++;  // recheck line
            score += 10;
        }
    }
}

void draw() {
    clear();
    if (gameover) {
        printw("GAME OVER! Your score: %d", score);
        printw("\nPress 'Q' to quit; Press 'R' to restart");
    } else {
        for (int i = 0; i < HEIGHT - 1; ++i) {
            mvaddch(i, 0, '|');  // side fence
            for (int j = 1; j < WIDTH; ++j) {
                char ch = '.';
                if (grid[i][j] == 1) {
                    ch = '#';
                }
                for (int bi = 0; bi < 4; ++bi) {
                    for (int bj = 0; bj < 4; ++bj) {
                        int x = bx + bj;
                        int y = by + bi;
                        if (x == j && y == i && block[bi][bj]) {
                            ch = '#';
                        }
                    }
                }
                mvaddch(i, j, ch);
            }
            mvaddch(i, WIDTH-1, '|');  // side fence
        }
        // botton fence
        for (int i = 0; i < WIDTH; ++i) {
            mvaddch(HEIGHT - 1, i, '=');
        }
        printw("\nscore: %d", score);
    }

    refresh();
    return;
}

void newShape() {
    int shape_idx = rand() % 7;
    setShape(SHAPES[shape_idx]);
    bx = WIDTH / 2 - 2;
    by = 0;
    if (!canMove(0, 0)) {
        draw();
        gameover = true;
    }
}

void init() {
    gameover = false;
    quit = false;
    score = 0;
    bx = WIDTH / 2 - 2;
    by = 0;
}

void input() {
    int key = getch();
    if (key == ' ')
        rotate();
    else if (key == KEY_LEFT && canMove(-1, 0))
        bx--;
    else if (key == KEY_RIGHT && canMove(1, 0))
        bx++;
    else if (key == KEY_DOWN && canMove(0, 1))
        by++;
    else if (key == 'q')
        quit = true;
    else if (key == 'r')
        init();
    return;
}

void logic() {
    if (canMove(0, 1)) {
        by++;
    } else {
        placeBlock();
        clearlines();
        newShape();
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
        napms(100);
    }
    endwin();
    return 0;
}