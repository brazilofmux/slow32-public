#include <stdio.h>
#include "errno.h"
#include "include/term.h"
#include "include/termios.h"
#include "include/unistd.h"

/* No term service: no descriptor is a terminal. */
int tcgetattr(int fd, struct termios *t) {
    (void)fd;
    (void)t;
    errno = ENOTTY;
    return -1;
}

int tcsetattr(int fd, int optional_actions, const struct termios *t) {
    (void)fd;
    (void)optional_actions;
    (void)t;
    errno = ENOTTY;
    return -1;
}

int isatty(int fd) {
    (void)fd;
    errno = ENOTTY;
    return 0;
}

int term_init(void) {
    return -1;
}

void term_cleanup(void) {
}

int term_set_cursor(int style) {
    (void)style;
    return -1;
}

int term_set_raw(int raw) {
    (void)raw;
    return -1;
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

int term_getchar(void) {
    return -1;
}

int term_kbhit(void) {
    return 0;
}

int term_wait_key(int ms) {
    (void)ms;
    return 0;
}

void term_set_color(int fg, int bg) {
    (void)fg;
    (void)bg;
}

void term_putc(int ch) {
    putchar(ch);
}

void term_puts(const char *s) {
    fputs(s, stdout);
}

int term_save_screen(void) {
    return -1;
}

int term_restore_screen(void) {
    return -1;
}
