#include "include/term.h"

int term_init(void) {
    return -1;
}

void term_cleanup(void) {
}

void term_set_raw(int raw) {
    (void)raw;
}

void term_get_size(int *rows, int *cols) {
    if (rows) *rows = 24;
    if (cols) *cols = 80;
}

void term_gotoxy(int row, int col) {
    (void)row;
    (void)col;
}

void term_clear(int mode) {
    (void)mode;
}

void term_set_attr(int attr) {
    (void)attr;
}

int term_getkey(void) {
    return -1;
}

int term_kbhit(void) {
    return 0;
}

void term_set_color(int fg, int bg) {
    (void)fg;
    (void)bg;
}
