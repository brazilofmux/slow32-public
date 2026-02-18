#include "terminal.h"
#include <stdio.h>
#include <term.h>

static int term_state = 0; /* 0=uninitialized, 1=available, -1=unavailable */
static int cur_fg = 7;
static int cur_bg = 0;

static void ensure_init(void) {
    if (term_state != 0)
        return;
    term_state = (term_init() == 0) ? 1 : -1;
}

int sb_term_init(void) {
    ensure_init();
    return (term_state == 1) ? 0 : -1;
}

void sb_term_shutdown(void) {
    if (term_state == 1)
        term_cleanup();
    term_state = 0;
}

static int clamp_ansi_color(int c) {
    if (c < 0) return 0;
    if (c > 15) return 15;
    return c;
}

void sb_term_cls(void) {
    ensure_init();
    if (term_state == 1) {
        term_clear(0);
        term_gotoxy(1, 1);
        return;
    }
    fputs("\033[2J\033[H", stdout);
    fflush(stdout);
}

void sb_term_locate(int row, int col) {
    ensure_init();
    if (row < 1) row = 1;
    if (col < 1) col = 1;
    if (term_state == 1) {
        term_gotoxy(row, col);
        return;
    }
    printf("\033[%d;%dH", row, col);
    fflush(stdout);
}

void sb_term_color(int fg, int bg, int has_fg, int has_bg) {
    ensure_init();

    if (!has_fg && !has_bg) {
        cur_fg = 7;
        cur_bg = 0;
    } else {
        if (has_fg) cur_fg = clamp_ansi_color(fg);
        if (has_bg) cur_bg = clamp_ansi_color(bg);
    }

    if (term_state == 1) {
        int service_fg = cur_fg & 7;
        int service_bg = cur_bg & 7;
        term_set_attr((cur_fg >= 8) ? 1 : 0);
        term_set_color(service_fg, service_bg);
        return;
    }

    if (!has_fg && !has_bg) {
        fputs("\033[0m", stdout);
        fflush(stdout);
        return;
    }

    if (cur_fg < 8) {
        printf("\033[%d", 30 + cur_fg);
    } else {
        printf("\033[%d", 90 + (cur_fg - 8));
    }
    if (cur_bg < 8) {
        printf(";%dm", 40 + cur_bg);
    } else {
        printf(";%dm", 100 + (cur_bg - 8));
    }
    fflush(stdout);
}

int sb_term_inkey(char out[2]) {
    ensure_init();
    if (term_state != 1)
        return 0;

    term_set_raw(1);
    if (!term_kbhit()) {
        term_set_raw(0);
        return 0;
    }
    int ch = term_getkey();
    term_set_raw(0);
    if (ch < 0)
        return 0;
    out[0] = (char)(ch & 0xFF);
    out[1] = '\0';
    return 1;
}
