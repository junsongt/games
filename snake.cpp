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
const int WIDTH = 20, HEIGHT = 20;
int x, y, ball_x, ball_y, score;  // (x, y) coord of the head of snake; (ball_x, ball_y) coord of ball
int tail_x[100], tail_y[100];
int N;  // length of tail, excluding head
enum directions { STOP = 0,
                  LEFT,
                  RIGHT,
                  UP,
                  DOWN };
directions dir;  // direction of head

const int nspeed = 20;
int speed_counter = 0;
bool auto_mov;
directions v; // the velocity of each segment in the tail

void crawl() {
    v = dir; // ensure the velocity is consistent with the head dir so that v can be passed like wave thru the tail end
    if (dir != STOP) {
        int prev_x = x, prev_y = y;
        for (int i = 0; i < N; i++) {
            int curr_x = tail_x[i];
            int curr_y = tail_y[i];

            if (curr_x > prev_x && curr_y == prev_y) v = LEFT;
            if (curr_x < prev_x && curr_y == prev_y) v = RIGHT;
            if (curr_x == prev_x && curr_y > prev_y) v = UP;
            if (curr_x == prev_x && curr_y < prev_y) v = DOWN;

            tail_x[i] = prev_x;
            tail_y[i] = prev_y;
            prev_x = curr_x;
            prev_y = curr_y;
        }
    }
}

void eat_n_grow() {
    // if snake eats the food, grow
    if (x == ball_x && y == ball_y) {
        score += 10;
        ball_x = rand() % (WIDTH - 2) + 1;
        ball_y = rand() % (HEIGHT - 2) + 1;

        int end_x = (N == 0) ? x : tail_x[N - 1];
        int end_y = (N == 0) ? y : tail_y[N - 1];
        N++;  // increase the tail of the snake by 1
        // increment the tail at the moment when the food eaten, pos depending on the velocity of the tail end
        switch (v) {
            case LEFT:
                tail_x[N - 1] = end_x + 1;
                tail_y[N - 1] = end_y;
                break;
            case RIGHT:
                tail_x[N - 1] = end_x - 1;
                tail_y[N - 1] = end_y;
                break;
            case UP:
                tail_x[N - 1] = end_x;
                tail_y[N - 1] = end_y + 1;
                break;
            case DOWN:
                tail_x[N - 1] = end_x;
                tail_y[N - 1] = end_y - 1;
                break;
            default:
                break;
        }
    }

}


void init() {
    gameover = false;
    quit = false;
    dir = STOP;
    v = dir;
    x = WIDTH / 2;
    y = HEIGHT / 2;
    ball_x = rand() % (WIDTH - 2) + 1;
    ball_y = rand() % (HEIGHT - 2) + 1;
    score = 0;
    N = 0;
    auto_mov = false;
}

void draw() {
    clear();
    if (gameover) {
        printw("GAME OVER! Your score: %d", score);
        printw("\nPress 'Q' to quit; Press 'R' to restart");
    } else {
        for (int i = 0; i < WIDTH; i++) {
            mvaddch(0, i, '=');
        }
        for (int i = 1; i < HEIGHT - 1; i++) {
            for (int j = 0; j < WIDTH; j++) {
                if (j == 0 || j == WIDTH - 1) {
                    mvaddch(i, j, '|');
                }
                // draw head
                else if (i == y && j == x)
                    mvaddch(i, j, '@');
                // draw ball
                else if (i == ball_y && j == ball_x) {
                    mvaddch(i, j, 'O');
                } else {
                    bool in_tail = false;
                    for (int k = 0; k < N; k++) {
                        if (tail_x[k] == j && tail_y[k] == i) {
                            mvaddch(i, j, '$');
                            in_tail = true;
                        }
                    }
                    if (!in_tail) {
                        mvaddch(i, j, ' ');
                    }
                }
            }
        }
        for (int i = 0; i < WIDTH; i++) {
            mvaddch(HEIGHT - 1, i, '=');
        }
        printw("\nscore: %d", score);
    }
    refresh();
}

void input() {
    int key = getch();
    switch (key) {
        case KEY_UP:
            dir = UP;
            crawl();
            --y;
            eat_n_grow();
            break;
        case KEY_DOWN:
            dir = DOWN;
            crawl();
            ++y;
            eat_n_grow();
            break;
        case KEY_LEFT:
            dir = LEFT;
            crawl();
            --x;
            eat_n_grow();
            break;
        case KEY_RIGHT:
            dir = RIGHT;
            crawl();
            ++x;
            eat_n_grow();
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
    if (auto_mov) {
        crawl();
        switch (dir) {
            case LEFT:
                --x;
                break;
            case RIGHT:
                ++x;
                break;
            case UP:
                --y;
                break;
            case DOWN:
                ++y;
                break;
        }
        eat_n_grow();

        // if snake hits the wall, gameover
        if (x >= WIDTH - 1 || x <= 0 || y <= 0 || y >= HEIGHT - 1) {
            gameover = true;
        }

        // // free of wall-death modifier
        // x = (x+WIDTH) % WIDTH;
        // y = (y+HEIGHT) % HEIGHT;

        // if the snake bites its tail, gameover
        for (int i = 0; i < N; i++) {
            if (x == tail_x[i] && y == tail_y[i]) {
                gameover = true;
            }
        }
        speed_counter = 0;
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
        auto_mov = (speed_counter == nspeed);
        draw();
        input();
        logic();
        // napms(20);
    }
    endwin();
    return 0;
}