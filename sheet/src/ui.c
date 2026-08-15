#include "sheet.h"

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <term.h>

#define KEY_UP    1000
#define KEY_DOWN  1001
#define KEY_LEFT  1002
#define KEY_RIGHT 1003
#define KEY_HOME  1004
#define KEY_PGUP  1006
#define KEY_PGDN  1007
#define KEY_DEL   1008
#define CTRL(k)   ((k) & 0x1f)

#define COL_WIDTH 10
#define ROW_GUTTER 4
#define EDIT_MAX  128

typedef struct {
    sheet_t *sh;
    char *filename;
    int filename_cap;
    int cur_col;
    int cur_row;
    int top_col;
    int top_row;
    int screen_rows;
    int screen_cols;
    int vis_cols;
    int vis_rows;
    int editing;
    int prompt;          /* 0 none, 1 save, 2 load */
    char edit[EDIT_MAX];
    int edit_len;
    char message[80];
} ui_t;

static void put_n(const char *s, int width) {
    int i;
    for (i = 0; i < width; i++) {
        char ch = (s && s[i]) ? s[i] : ' ';
        term_putc(ch);
        if (s && !s[i]) {
            s = NULL;
        }
    }
}

static void put_line(int row, int col, const char *s, int width) {
    term_gotoxy(row, col);
    put_n(s, width);
}

static int read_key(void) {
    int ch = term_getkey();
    if (ch < 0) {
        return -1;
    }
    if (ch != 27) {
        return ch;
    }
    if (!term_kbhit()) {
        return 27;
    }
    if (term_getkey() != '[') {
        return 27;
    }
    switch (term_getkey()) {
    case 'A': return KEY_UP;
    case 'B': return KEY_DOWN;
    case 'C': return KEY_RIGHT;
    case 'D': return KEY_LEFT;
    case 'H': return KEY_HOME;
    case '5':
        if (term_kbhit()) {
            term_getkey();
        }
        return KEY_PGUP;
    case '6':
        if (term_kbhit()) {
            term_getkey();
        }
        return KEY_PGDN;
    case '3':
        if (term_kbhit()) {
            term_getkey();
        }
        return KEY_DEL;
    default:
        return 27;
    }
}

static void compute_window(ui_t *ui) {
    int grid_cols;
    int grid_rows;

    term_get_size(&ui->screen_rows, &ui->screen_cols);
    if (ui->screen_rows < 8) {
        ui->screen_rows = 8;
    }
    if (ui->screen_cols < 20) {
        ui->screen_cols = 20;
    }
    /* rows: 1 status, 2 formula, 3 headers, last help */
    grid_rows = ui->screen_rows - 4;
    grid_cols = ui->screen_cols - ROW_GUTTER - 1;
    ui->vis_rows = grid_rows > 1 ? grid_rows : 1;
    ui->vis_cols = grid_cols / (COL_WIDTH + 1);
    if (ui->vis_cols < 1) {
        ui->vis_cols = 1;
    }
    if (ui->vis_cols > SHEET_COLS) {
        ui->vis_cols = SHEET_COLS;
    }
    if (ui->vis_rows > SHEET_ROWS) {
        ui->vis_rows = SHEET_ROWS;
    }
}

static void keep_cursor_visible(ui_t *ui) {
    if (ui->cur_col < ui->top_col) {
        ui->top_col = ui->cur_col;
    }
    if (ui->cur_col >= ui->top_col + ui->vis_cols) {
        ui->top_col = ui->cur_col - ui->vis_cols + 1;
    }
    if (ui->cur_row < ui->top_row) {
        ui->top_row = ui->cur_row;
    }
    if (ui->cur_row >= ui->top_row + ui->vis_rows) {
        ui->top_row = ui->cur_row - ui->vis_rows + 1;
    }
    if (ui->top_col < 0) {
        ui->top_col = 0;
    }
    if (ui->top_row < 0) {
        ui->top_row = 0;
    }
}

static void start_edit(ui_t *ui, const char *seed) {
    ui->editing = 1;
    ui->prompt = 0;
    ui->edit[0] = '\0';
    ui->edit_len = 0;
    if (seed) {
        strncpy(ui->edit, seed, EDIT_MAX - 1);
        ui->edit[EDIT_MAX - 1] = '\0';
        ui->edit_len = (int)strlen(ui->edit);
    }
}

static void draw(ui_t *ui) {
    int r, c;
    int help_row;
    char name[8];
    char val[16];
    char status[128];
    cell_t *cell;
    const char *raw;

    compute_window(ui);
    keep_cursor_visible(ui);
    help_row = ui->screen_rows;

    term_begin_update();
    term_set_color(7, 4);
    term_set_attr(0);

    format_cell_name(ui->cur_col, ui->cur_row, name, (int)sizeof(name));
    cell = sheet_cell(ui->sh, ui->cur_col, ui->cur_row);
    raw = (cell && cell->raw) ? cell->raw : "";
    if (cell) {
        sheet_format_value(cell, val, (int)sizeof(val));
    } else {
        val[0] = '\0';
    }

    if (ui->message[0]) {
        snprintf(status, sizeof(status), " %s", ui->message);
    } else {
        snprintf(status, sizeof(status), " %s  %s    %s",
                 name, val, ui->filename[0] ? ui->filename : "(untitled)");
    }
    term_set_attr(7);
    put_line(1, 1, status, ui->screen_cols);
    term_set_attr(0);

    if (ui->prompt == 1) {
        snprintf(status, sizeof(status), " Save: %s", ui->edit);
    } else if (ui->prompt == 2) {
        snprintf(status, sizeof(status), " Load: %s", ui->edit);
    } else if (ui->editing) {
        snprintf(status, sizeof(status), " %s: %s", name, ui->edit);
    } else {
        snprintf(status, sizeof(status), " %s: %s", name, raw);
    }
    put_line(2, 1, status, ui->screen_cols);

    /* column letters */
    term_gotoxy(3, 1);
    put_n("", ROW_GUTTER);
    for (c = 0; c < ui->vis_cols; c++) {
        char hdr[COL_WIDTH + 1];
        int cc = ui->top_col + c;
        memset(hdr, ' ', COL_WIDTH);
        hdr[COL_WIDTH] = '\0';
        if (cc < SHEET_COLS) {
            hdr[COL_WIDTH / 2] = (char)('A' + cc);
        }
        if (cc == ui->cur_col) {
            term_set_attr(7);
        }
        term_putc(' ');
        put_n(hdr, COL_WIDTH);
        term_set_attr(0);
    }

    for (r = 0; r < ui->vis_rows; r++) {
        int rr = ui->top_row + r;
        char rn[8];
        int screen_r = 4 + r;
        snprintf(rn, sizeof(rn), "%4d", rr + 1);
        term_gotoxy(screen_r, 1);
        if (rr == ui->cur_row) {
            term_set_attr(7);
        }
        put_n(rn, ROW_GUTTER);
        term_set_attr(0);
        for (c = 0; c < ui->vis_cols; c++) {
            int cc = ui->top_col + c;
            char buf[COL_WIDTH + 1];
            cell_t *here = sheet_cell(ui->sh, cc, rr);
            buf[0] = '\0';
            if (here) {
                sheet_format_value(here, buf, COL_WIDTH + 1);
            }
            term_putc(' ');
            if (cc == ui->cur_col && rr == ui->cur_row) {
                term_set_attr(7);
            }
            put_n(buf, COL_WIDTH);
            term_set_attr(0);
        }
    }

    term_set_attr(7);
    put_line(help_row, 1,
             " arrows move   Enter/= edit   / File   ^Q quit",
             ui->screen_cols);
    term_set_attr(0);
    term_end_update();
}

static void commit_edit(ui_t *ui) {
    if (ui->prompt == 1) {
        if (ui->edit_len > 0) {
            strncpy(ui->filename, ui->edit, (size_t)ui->filename_cap - 1);
            ui->filename[ui->filename_cap - 1] = '\0';
            if (sheet_save(ui->sh, ui->filename) == 0) {
                snprintf(ui->message, sizeof(ui->message), "Saved %s",
                         ui->filename);
            } else {
                snprintf(ui->message, sizeof(ui->message), "Cannot save %s",
                         ui->filename);
            }
        }
        ui->prompt = 0;
        ui->editing = 0;
        return;
    }
    if (ui->prompt == 2) {
        if (ui->edit_len > 0) {
            if (sheet_load(ui->sh, ui->edit) == 0) {
                strncpy(ui->filename, ui->edit, (size_t)ui->filename_cap - 1);
                ui->filename[ui->filename_cap - 1] = '\0';
                snprintf(ui->message, sizeof(ui->message), "Loaded %s",
                         ui->filename);
            } else {
                snprintf(ui->message, sizeof(ui->message), "Cannot load %s",
                         ui->edit);
            }
        }
        ui->prompt = 0;
        ui->editing = 0;
        return;
    }
    sheet_set(ui->sh, ui->cur_col, ui->cur_row, ui->edit);
    sheet_recalc(ui->sh);
    ui->editing = 0;
    ui->message[0] = '\0';
}

static void move(ui_t *ui, int dc, int dr) {
    ui->cur_col += dc;
    ui->cur_row += dr;
    if (ui->cur_col < 0) ui->cur_col = 0;
    if (ui->cur_row < 0) ui->cur_row = 0;
    if (ui->cur_col >= SHEET_COLS) ui->cur_col = SHEET_COLS - 1;
    if (ui->cur_row >= SHEET_ROWS) ui->cur_row = SHEET_ROWS - 1;
    ui->message[0] = '\0';
}

static int handle_edit_key(ui_t *ui, int ch) {
    if (ch == 27) {
        ui->editing = 0;
        ui->prompt = 0;
        return 1;
    }
    if (ch == '\r' || ch == '\n') {
        commit_edit(ui);
        return 1;
    }
    if (ch == 127 || ch == 8 || ch == KEY_DEL) {
        if (ui->edit_len > 0) {
            ui->edit[--ui->edit_len] = '\0';
        }
        return 1;
    }
    if (ch >= 32 && ch < 127 && ui->edit_len < EDIT_MAX - 1) {
        ui->edit[ui->edit_len++] = (char)ch;
        ui->edit[ui->edit_len] = '\0';
        return 1;
    }
    return 1;
}

void sheet_ui(sheet_t *sh, char *filename, int filename_cap) {
    ui_t ui;
    int running = 1;

    memset(&ui, 0, sizeof(ui));
    ui.sh = sh;
    ui.filename = filename;
    ui.filename_cap = filename_cap;

    term_set_raw(1);
    sheet_recalc(sh);

    while (running) {
        int ch;
        draw(&ui);
        ch = read_key();
        if (ch < 0) {
            break;
        }
        if (ui.editing || ui.prompt) {
            handle_edit_key(&ui, ch);
            continue;
        }
        if (ch == CTRL('q') || ch == CTRL('c')) {
            break;
        }
        if (ch == KEY_LEFT) {
            move(&ui, -1, 0);
        } else if (ch == KEY_RIGHT) {
            move(&ui, 1, 0);
        } else if (ch == KEY_UP) {
            move(&ui, 0, -1);
        } else if (ch == KEY_DOWN) {
            move(&ui, 0, 1);
        } else if (ch == KEY_PGUP) {
            move(&ui, 0, -ui.vis_rows);
        } else if (ch == KEY_PGDN) {
            move(&ui, 0, ui.vis_rows);
        } else if (ch == KEY_HOME) {
            ui.cur_col = 0;
            ui.cur_row = 0;
        } else if (ch == '/' ) {
            ui.message[0] = '\0';
            draw(&ui);
            term_gotoxy(1, 1);
            term_set_attr(7);
            put_n(" S Save   L Load   Q Quit", ui.screen_cols);
            term_set_attr(0);
            ch = read_key();
            if (ch == 's' || ch == 'S') {
                ui.prompt = 1;
                start_edit(&ui, ui.filename);
                ui.prompt = 1;
            } else if (ch == 'l' || ch == 'L') {
                ui.prompt = 2;
                start_edit(&ui, ui.filename);
                ui.prompt = 2;
            } else if (ch == 'q' || ch == 'Q') {
                running = 0;
            }
        } else if (ch == '\r' || ch == '\n' || ch == '=') {
            cell_t *cell = sheet_cell(sh, ui.cur_col, ui.cur_row);
            const char *seed = "";
            if (ch == '=') {
                seed = "=";
            } else if (cell && cell->raw) {
                seed = cell->raw;
            }
            start_edit(&ui, seed);
        } else if (ch >= 32 && ch < 127) {
            char seed[2];
            seed[0] = (char)ch;
            seed[1] = '\0';
            start_edit(&ui, seed);
        }
    }

    term_set_raw(0);
    term_set_color(7, 0);
    term_set_attr(0);
    term_clear(0);
    term_gotoxy(1, 1);
}
