/*
 * nano.c — A nano-like text editor for SLOW-32
 *
 * Uses the terminal service (term.h) for full-screen editing.
 * Single-file implementation with array-of-lines buffer.
 *
 * File model: the buffer holds file bytes verbatim. Loading never rewrites
 * what it read, and saving writes back exactly what was loaded unless it was
 * edited. Two visible consequences follow, and both are deliberate:
 *
 *   - A trailing newline is represented as a final empty line rather than a
 *     flag, so "a\n" loads as two lines and a normal newline-terminated file
 *     reports one more line than `wc -l`. This is what keeps an empty file,
 *     "text", and "text\n" distinguishable on a round trip.
 *   - CR is not stripped, so a CRLF file shows a '?' (the substitute glyph
 *     for control bytes) at the end of every line. Stripping it on load would
 *     silently rewrite the file on the next save.
 *
 * Undo is transactional: undo_begin()/undo_end() group the entries a single
 * command produces, so one ^Z reverses one command. Entry cost scales with
 * lines touched, not characters — see editor_replace_line_all().
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <term.h>
#include <unistd.h>

/* ---- Constants ---- */

#define TAB_STOP       4
#define LINE_INIT_CAP  64
#define LINES_INIT_CAP 128
#define MAX_FILENAME   256
/* Room for MAX_FILENAME plus the ".nano.<0-99>.tmp" save suffix. */
#define MAX_TEMP_NAME  (MAX_FILENAME + 16)
#define MAX_SEARCH     256
#define MAX_MESSAGE    256
#define MAX_DIRTY      256  /* max screen rows for dirty tracking */
#define QUIT_CONFIRM   2    /* presses of Ctrl-Q to force-quit when modified */
#define UNDO_MAX       512  /* max undo stack depth */

/* Internal key codes (above ASCII range) */
#define KEY_NONE    0
#define KEY_UP      1000
#define KEY_DOWN    1001
#define KEY_LEFT    1002
#define KEY_RIGHT   1003
#define KEY_HOME    1004
#define KEY_END     1005
#define KEY_PGUP    1006
#define KEY_PGDN    1007
#define KEY_DEL     1008
#define KEY_CTRL_LEFT  1009
#define KEY_CTRL_RIGHT 1010
#define KEY_ALT_D      1011
#define KEY_ALT_BS     1012  /* Alt-Backspace */

/* Ctrl key helper */
#define CTRL(k) ((k) & 0x1f)

/* ---- Data Structures ---- */

typedef struct {
    char *text;
    int len;
    int cap;
} line_t;

/* Undo operation types */
enum {
    UNDO_INSERT_CHAR,   /* inserted char at (cy, cx-1) */
    UNDO_DELETE_CHAR,   /* deleted char at (cy, cx) */
    UNDO_INSERT_TEXT,   /* inserted text starting at (cy, cx) */
    UNDO_DELETE_TEXT,   /* deleted text starting at (cy, cx) */
    UNDO_INSERT_LINE,   /* split line: new line created at cy */
    UNDO_DELETE_LINE,   /* joined lines: line removed at cy+1 */
    UNDO_FULL_LINE,     /* full line deleted (cut) — stores text */
    UNDO_ADD_LINE,      /* full line added (paste/dup) */
};

typedef struct {
    int type;
    int cy, cx;         /* cursor position before the operation */
    char ch;            /* for INSERT_CHAR / DELETE_CHAR */
    char *text;         /* malloc'd text for span and line operations */
    int text_len;
    int group;
    int state_before;
    int state_after;
} undo_entry_t;

typedef struct {
    line_t *lines;
    int num_lines;
    int lines_cap;
    int cx, cy;                 /* cursor position (0-based, in file coords) */
    int row_offset, col_offset; /* viewport scroll offsets */
    int screen_rows, screen_cols;
    int edit_rows;              /* rows available for editing (screen - 2) */
    int gutter_width;           /* line number gutter width */
    char filename[MAX_FILENAME];
    int modified;
    int running;
    int quit_count;             /* consecutive Ctrl-Q presses */
    char search_buf[MAX_SEARCH];
    line_t *cut_lines;          /* cut buffer (multi-line) */
    int cut_count;              /* number of lines in cut buffer */
    int cut_cap;                /* capacity of cut buffer */
    int last_was_cut;           /* was last action a cut? (for multi-cut) */
    undo_entry_t undo_stack[UNDO_MAX];
    int undo_top;               /* next applied entry */
    int undo_count;             /* applied entries plus redo entries */
    int undo_group_depth;
    int undo_group;
    int undo_group_before;
    int undo_group_after;
    int undo_group_has_entries;
    int undo_group_recording;
    int next_undo_group;
    int current_state;
    int saved_state;
    int next_state;
    char message[MAX_MESSAGE];
    char dirty[MAX_DIRTY];      /* 1 = screen row needs redraw */
    int dirty_status;           /* 1 = status bar needs redraw */
    int dirty_bottom;           /* 1 = message/help bar needs redraw */
    int prev_row_offset;        /* detect scroll changes */
    int prev_col_offset;
    int prev_gutter_width;
    int prev_num_lines;         /* detect line count changes (gutter width) */
    char prev_message[MAX_MESSAGE];
} editor_t;

static editor_t E;

/* Forward declarations */
static void editor_set_message(const char *msg);
static void editor_prompt(const char *prompt, char *buf, int bufsize);
static void editor_clamp_cx(void);
static void editor_sync_modified(void);

/* ---- Line Memory Management ---- */

static void line_init(line_t *l) {
    l->cap = LINE_INIT_CAP;
    l->text = malloc(l->cap);
    l->text[0] = '\0';
    l->len = 0;
}

static void line_free(line_t *l) {
    free(l->text);
    l->text = NULL;
    l->len = 0;
    l->cap = 0;
}

static void line_ensure_cap(line_t *l, int needed) {
    if (needed <= l->cap) return;
    while (l->cap < needed) l->cap *= 2;
    l->text = realloc(l->text, l->cap);
}

static void line_set(line_t *l, const char *s, int slen) {
    line_ensure_cap(l, slen + 1);
    memcpy(l->text, s, slen);
    l->text[slen] = '\0';
    l->len = slen;
}

static void line_insert_char(line_t *l, int pos, char ch) {
    if (pos < 0) pos = 0;
    if (pos > l->len) pos = l->len;
    line_ensure_cap(l, l->len + 2);
    memmove(l->text + pos + 1, l->text + pos, l->len - pos + 1);
    l->text[pos] = ch;
    l->len++;
}

static void line_insert_text(line_t *l, int pos, const char *text, int text_len) {
    if (text_len <= 0) return;
    if (pos < 0) pos = 0;
    if (pos > l->len) pos = l->len;
    line_ensure_cap(l, l->len + text_len + 1);
    memmove(l->text + pos + text_len, l->text + pos, l->len - pos + 1);
    memcpy(l->text + pos, text, text_len);
    l->len += text_len;
}

static void line_delete_char(line_t *l, int pos) {
    if (pos < 0 || pos >= l->len) return;
    memmove(l->text + pos, l->text + pos + 1, l->len - pos);
    l->len--;
}

static void line_delete_text(line_t *l, int pos, int text_len) {
    if (pos < 0 || pos >= l->len || text_len <= 0) return;
    if (text_len > l->len - pos) text_len = l->len - pos;
    memmove(l->text + pos, l->text + pos + text_len,
            l->len - pos - text_len + 1);
    l->len -= text_len;
}

static void line_append(line_t *l, const char *s, int slen) {
    line_ensure_cap(l, l->len + slen + 1);
    memcpy(l->text + l->len, s, slen);
    l->len += slen;
    l->text[l->len] = '\0';
}

/* ---- Editor Line Array Operations ---- */

static void editor_ensure_lines_cap(int needed) {
    if (needed <= E.lines_cap) return;
    while (E.lines_cap < needed) E.lines_cap *= 2;
    E.lines = realloc(E.lines, E.lines_cap * sizeof(line_t));
}

static void editor_insert_line(int at, const char *s, int slen) {
    if (at < 0) at = 0;
    if (at > E.num_lines) at = E.num_lines;
    editor_ensure_lines_cap(E.num_lines + 1);
    if (at < E.num_lines) {
        memmove(&E.lines[at + 1], &E.lines[at],
                (E.num_lines - at) * sizeof(line_t));
    }
    line_init(&E.lines[at]);
    line_set(&E.lines[at], s, slen);
    E.num_lines++;
}

static void editor_delete_line(int at) {
    if (at < 0 || at >= E.num_lines) return;
    line_free(&E.lines[at]);
    if (at < E.num_lines - 1) {
        memmove(&E.lines[at], &E.lines[at + 1],
                (E.num_lines - at - 1) * sizeof(line_t));
    }
    E.num_lines--;
}

/* Split line at position: line[at] becomes two lines */
static void editor_split_line(int at, int pos) {
    line_t *l = &E.lines[at];
    int tail_len = l->len - pos;
    char *tail = l->text + pos;
    editor_insert_line(at + 1, tail, tail_len);
    /* Truncate original line */
    l->text[pos] = '\0';
    l->len = pos;
}

/* Join line[at] with line[at+1] */
static void editor_join_lines(int at) {
    if (at < 0 || at >= E.num_lines - 1) return;
    line_append(&E.lines[at], E.lines[at + 1].text, E.lines[at + 1].len);
    editor_delete_line(at + 1);
}

/* ---- Gutter Width ---- */

static int compute_gutter_width(void) {
    int n = E.num_lines;
    int digits = 1;
    while (n >= 10) { digits++; n /= 10; }
    return digits + 1; /* digits + space */
}

static int line_visual_col(const line_t *l, int byte_col) {
    int i;
    int visual_col = 0;

    if (byte_col > l->len) byte_col = l->len;
    for (i = 0; i < byte_col; i++) {
        if (l->text[i] == '\t')
            visual_col += TAB_STOP - (visual_col % TAB_STOP);
        else
            visual_col++;
    }
    return visual_col;
}

/* ---- Dirty Tracking ---- */

/* Mark all screen rows dirty (e.g. after scroll) */
static void dirty_all(void) {
    int i;
    for (i = 0; i < E.edit_rows && i < MAX_DIRTY; i++)
        E.dirty[i] = 1;
    E.dirty_status = 1;
    E.dirty_bottom = 1;
}

/* Mark a single file row dirty (translates to screen row) */
static void dirty_file_row(int file_row) {
    int screen_row = file_row - E.row_offset;
    if (screen_row >= 0 && screen_row < E.edit_rows && screen_row < MAX_DIRTY)
        E.dirty[screen_row] = 1;
}

/* Mark file row and all rows below it dirty (for insert/delete line) */
static void dirty_from_file_row(int file_row) {
    int screen_row = file_row - E.row_offset;
    int i;
    if (screen_row < 0) screen_row = 0;
    for (i = screen_row; i < E.edit_rows && i < MAX_DIRTY; i++)
        E.dirty[i] = 1;
}

/* ---- Undo/Redo ---- */

static void undo_free_entry(undo_entry_t *e) {
    if (e->text) { free(e->text); e->text = NULL; }
}

static void undo_clear(void) {
    int i;
    for (i = 0; i < E.undo_count; i++)
        undo_free_entry(&E.undo_stack[i]);
    E.undo_top = 0;
    E.undo_count = 0;
    /* Reset the whole group state, not just the depth: leaving
     * undo_group_recording clear would silently drop the next command's
     * entries if undo_clear() were ever called with a group open. */
    E.undo_group_depth = 0;
    E.undo_group_recording = 0;
    E.undo_group_has_entries = 0;
}

static void editor_sync_modified(void) {
    E.modified = (E.current_state != E.saved_state);
    E.dirty_status = 1;
}

static void undo_begin(void) {
    if (E.undo_group_depth++ > 0) return;
    E.undo_group = E.next_undo_group++;
    E.undo_group_before = E.current_state;
    E.undo_group_after = E.next_state++;
    E.undo_group_has_entries = 0;
    E.undo_group_recording = 1;
}

/* Record that the open group has changed the buffer. The group's state is
 * applied here rather than only in undo_end() so that a redraw in the middle
 * of a command — the ^R replace prompts, say — shows [Modified] correctly.
 * Applying it twice is harmless: undo_group_after is a fixed value. */
static void undo_mark_group_dirty(void) {
    E.undo_group_has_entries = 1;
    E.current_state = E.undo_group_after;
    editor_sync_modified();
}

static void undo_end(void) {
    if (E.undo_group_depth <= 0) return;
    if (--E.undo_group_depth > 0) return;
    if (E.undo_group_has_entries) {
        E.current_state = E.undo_group_after;
        editor_sync_modified();
    }
}

static void undo_drop_oldest_group(void) {
    int count = 0;
    int group;

    if (E.undo_count == 0) return;
    group = E.undo_stack[0].group;
    while (count < E.undo_count && E.undo_stack[count].group == group) {
        undo_free_entry(&E.undo_stack[count]);
        count++;
    }
    if (count < E.undo_count) {
        memmove(&E.undo_stack[0], &E.undo_stack[count],
                (E.undo_count - count) * sizeof(undo_entry_t));
    }
    E.undo_count -= count;
    E.undo_top -= count;
    if (E.undo_top < 0) E.undo_top = 0;
}

static void undo_push(int type, int cy, int cx, char ch, const char *text, int text_len) {
    int implicit_group = (E.undo_group_depth == 0);
    int i;

    if (implicit_group) undo_begin();
    if (!E.undo_group_recording) {
        if (implicit_group) undo_end();
        return;
    }

    /* A new edit after undoing branches history and discards the redo tail. */
    for (i = E.undo_top; i < E.undo_count; i++)
        undo_free_entry(&E.undo_stack[i]);
    E.undo_count = E.undo_top;

    if (E.undo_count >= UNDO_MAX) {
        if (E.undo_stack[0].group == E.undo_group) {
            /* Never retain a partial transaction: make an oversized edit
             * non-undoable instead of allowing a corrupt partial undo. */
            while (E.undo_count > 0)
                undo_drop_oldest_group();
            undo_mark_group_dirty();
            E.undo_group_recording = 0;
            if (implicit_group) undo_end();
            return;
        }
        undo_drop_oldest_group();
    }

    {
        undo_entry_t *e = &E.undo_stack[E.undo_count++];
        e->type = type;
        e->cy = cy;
        e->cx = cx;
        e->ch = ch;
        e->text = NULL;
        e->text_len = 0;
        if (text && text_len > 0) {
            e->text = malloc(text_len + 1);
            memcpy(e->text, text, text_len);
            e->text[text_len] = '\0';
            e->text_len = text_len;
        }
        e->group = E.undo_group;
        e->state_before = E.undo_group_before;
        e->state_after = E.undo_group_after;
    }
    E.undo_top = E.undo_count;
    undo_mark_group_dirty();
    if (implicit_group) undo_end();
}

static void editor_undo(void) {
    int group;
    int state_before;
    if (E.undo_top == 0) {
        editor_set_message("Nothing to undo");
        return;
    }
    group = E.undo_stack[E.undo_top - 1].group;
    state_before = E.undo_stack[E.undo_top - 1].state_before;

    while (E.undo_top > 0 && E.undo_stack[E.undo_top - 1].group == group) {
        undo_entry_t *e = &E.undo_stack[--E.undo_top];
        switch (e->type) {
        case UNDO_INSERT_CHAR:
            E.cy = e->cy; E.cx = e->cx;
            line_delete_char(&E.lines[E.cy], E.cx);
            dirty_file_row(E.cy);
            break;
        case UNDO_DELETE_CHAR:
            E.cy = e->cy; E.cx = e->cx;
            line_insert_char(&E.lines[E.cy], E.cx, e->ch);
            dirty_file_row(E.cy);
            break;
        case UNDO_INSERT_TEXT:
            E.cy = e->cy; E.cx = e->cx;
            line_delete_text(&E.lines[E.cy], E.cx, e->text_len);
            dirty_file_row(E.cy);
            break;
        case UNDO_DELETE_TEXT:
            E.cy = e->cy; E.cx = e->cx;
            line_insert_text(&E.lines[E.cy], E.cx, e->text, e->text_len);
            dirty_file_row(E.cy);
            break;
        case UNDO_INSERT_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_join_lines(E.cy);
            dirty_from_file_row(E.cy);
            break;
        case UNDO_DELETE_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_split_line(E.cy, E.cx);
            dirty_from_file_row(E.cy);
            break;
        case UNDO_FULL_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_insert_line(E.cy, e->text, e->text_len);
            dirty_from_file_row(E.cy);
            break;
        case UNDO_ADD_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_delete_line(e->cy);
            if (E.num_lines == 0) editor_insert_line(0, "", 0);
            if (E.cy >= E.num_lines) E.cy = E.num_lines - 1;
            editor_clamp_cx();
            dirty_from_file_row(E.cy);
            break;
        }
    }
    E.current_state = state_before;
    /* Undo ends any run of cuts: a following ^K starts a fresh cut buffer
     * instead of appending to the run this undo just took apart. */
    E.last_was_cut = 0;
    editor_sync_modified();
    editor_set_message("Undo");
}

static void editor_redo(void) {
    int group;
    int state_after;
    if (E.undo_top >= E.undo_count) {
        editor_set_message("Nothing to redo");
        return;
    }
    group = E.undo_stack[E.undo_top].group;
    state_after = E.undo_stack[E.undo_top].state_after;

    while (E.undo_top < E.undo_count && E.undo_stack[E.undo_top].group == group) {
        undo_entry_t *e = &E.undo_stack[E.undo_top++];
        switch (e->type) {
        case UNDO_INSERT_CHAR:
            E.cy = e->cy; E.cx = e->cx;
            line_insert_char(&E.lines[E.cy], E.cx, e->ch);
            E.cx++;
            dirty_file_row(E.cy);
            break;
        case UNDO_DELETE_CHAR:
            E.cy = e->cy; E.cx = e->cx;
            line_delete_char(&E.lines[E.cy], E.cx);
            dirty_file_row(E.cy);
            break;
        case UNDO_INSERT_TEXT:
            E.cy = e->cy; E.cx = e->cx;
            line_insert_text(&E.lines[E.cy], E.cx, e->text, e->text_len);
            E.cx += e->text_len;
            dirty_file_row(E.cy);
            break;
        case UNDO_DELETE_TEXT:
            E.cy = e->cy; E.cx = e->cx;
            line_delete_text(&E.lines[E.cy], E.cx, e->text_len);
            dirty_file_row(E.cy);
            break;
        case UNDO_INSERT_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_split_line(E.cy, E.cx);
            E.cy++;
            E.cx = 0;
            dirty_from_file_row(E.cy - 1);
            break;
        case UNDO_DELETE_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_join_lines(E.cy);
            dirty_from_file_row(E.cy);
            break;
        case UNDO_FULL_LINE:
            E.cy = e->cy; E.cx = e->cx;
            editor_delete_line(E.cy);
            if (E.num_lines == 0) editor_insert_line(0, "", 0);
            if (E.cy >= E.num_lines) E.cy = E.num_lines - 1;
            editor_clamp_cx();
            dirty_from_file_row(E.cy);
            break;
        case UNDO_ADD_LINE:
            editor_insert_line(e->cy, e->text, e->text_len);
            E.cy = e->cy;
            E.cx = e->cx;
            dirty_from_file_row(E.cy);
            break;
        }
    }
    E.current_state = state_after;
    E.last_was_cut = 0;
    editor_sync_modified();
    editor_set_message("Redo");
}

/* ---- File I/O ---- */

static void editor_load(const char *filename) {
    FILE *f;
    line_t current;
    int ch;

    strncpy(E.filename, filename, MAX_FILENAME - 1);
    E.filename[MAX_FILENAME - 1] = '\0';

    f = fopen(filename, "r");
    if (!f) {
        editor_insert_line(0, "", 0);
        if (access(filename, F_OK) == 0)
            snprintf(E.message, MAX_MESSAGE, "Can't open: %s", filename);
        else
            snprintf(E.message, MAX_MESSAGE, "New file: %s", filename);
        return;
    }

    line_init(&current);
    while ((ch = fgetc(f)) != EOF) {
        if (ch == '\n') {
            editor_insert_line(E.num_lines, current.text, current.len);
            line_set(&current, "", 0);
            continue;
        }
        line_insert_char(&current, current.len, (char)ch);
    }
    /* Always retain the final logical line. This preserves the distinction
     * between an empty file, "text", and "text\n" without separate flags. */
    editor_insert_line(E.num_lines, current.text, current.len);
    line_free(&current);

    if (ferror(f))
        snprintf(E.message, MAX_MESSAGE, "Read error: %s (partial buffer)", filename);
    else
        snprintf(E.message, MAX_MESSAGE, "Loaded %s (%d lines)", filename, E.num_lines);
    fclose(f);
}

/* Build a sibling temp path for the atomic save. The caller's buffer must be
 * MAX_TEMP_NAME bytes: E.filename can fill MAX_FILENAME, and the suffix needs
 * room on top of it, so sizing this at MAX_FILENAME would make the longest
 * legal filenames unsaveable. */
static int editor_make_temp_name(char *temp_name, int temp_size) {
    int attempt;

    for (attempt = 0; attempt < 100; attempt++) {
        int len = snprintf(temp_name, temp_size, "%s.nano.%d.tmp",
                           E.filename, attempt);
        if (len < 0 || len >= temp_size)
            return -1;
        if (access(temp_name, F_OK) != 0)
            return 0;
    }
    return -1;
}

static int editor_write_file(const char *filename) {
    FILE *f;
    int i;
    int failed = 0;

    f = fopen(filename, "w");
    if (!f) return -1;

    for (i = 0; i < E.num_lines && !failed; i++) {
        if (E.lines[i].len > 0 &&
            fwrite(E.lines[i].text, 1, E.lines[i].len, f) !=
                (size_t)E.lines[i].len) {
            failed = 1;
        }
        if (!failed && i < E.num_lines - 1 && fputc('\n', f) == EOF)
            failed = 1;
    }
    if (!failed && fflush(f) == EOF)
        failed = 1;
    if (ferror(f))
        failed = 1;
    if (fclose(f) == EOF)
        failed = 1;

    return failed ? -1 : 0;
}

static int editor_save(void) {
    char temp_name[MAX_TEMP_NAME];

    if (E.filename[0] == '\0') {
        char namebuf[MAX_FILENAME] = "";
        editor_prompt("Filename: ", namebuf, MAX_FILENAME);
        if (namebuf[0] == '\0') {
            editor_set_message("Save cancelled");
            return -1;
        }
        strncpy(E.filename, namebuf, MAX_FILENAME - 1);
        E.filename[MAX_FILENAME - 1] = '\0';
    }

    if (editor_make_temp_name(temp_name, sizeof(temp_name)) != 0) {
        snprintf(E.message, MAX_MESSAGE, "Can't create safe save name: %s", E.filename);
        return -1;
    }

    if (editor_write_file(temp_name) != 0) {
        remove(temp_name);
        snprintf(E.message, MAX_MESSAGE, "Write failed; original unchanged: %s", E.filename);
        return -1;
    }
    /* rename() replaces the target inode, so the saved file carries the temp
     * file's mode rather than the original's, and a symlinked path is replaced
     * by a regular file. Preserving mode would need chmod(), which the SLOW-32
     * runtime does not provide; the atomicity is worth the trade here. */
    if (rename(temp_name, E.filename) != 0) {
        snprintf(E.message, MAX_MESSAGE, "Rename failed; recovery file: %s", temp_name);
        return -1;
    }

    E.saved_state = E.current_state;
    editor_sync_modified();
    snprintf(E.message, MAX_MESSAGE, "Saved %s (%d lines)", E.filename, E.num_lines);
    return 0;
}

/* ---- Keyboard Input ---- */

static int read_key(void) {
    int ch = term_getkey();
    if (ch < 0) return -1;

    if (ch == 27) {
        /* Escape sequence */
        if (!term_kbhit()) return 27;
        int ch2 = term_getkey();
        if (ch2 == '[') {
            int ch3 = term_getkey();
            switch (ch3) {
            case 'A': return KEY_UP;
            case 'B': return KEY_DOWN;
            case 'C': return KEY_RIGHT;
            case 'D': return KEY_LEFT;
            case 'H': return KEY_HOME;
            case 'F': return KEY_END;
            case '1': {
                /* ESC[1~ = Home, ESC[1;5D = Ctrl-Left, ESC[1;5C = Ctrl-Right */
                int ch4 = term_getkey();
                if (ch4 == '~') return KEY_HOME;
                if (ch4 == ';') {
                    int ch5 = term_getkey();
                    int ch6 = term_getkey();
                    if (ch5 == '5' && ch6 == 'D') return KEY_CTRL_LEFT;
                    if (ch5 == '5' && ch6 == 'C') return KEY_CTRL_RIGHT;
                    if (ch5 == '3' && ch6 == 'D') return KEY_CTRL_LEFT;  /* Alt-Left */
                    if (ch5 == '3' && ch6 == 'C') return KEY_CTRL_RIGHT; /* Alt-Right */
                }
                return KEY_HOME;
            }
            case '4':
                term_getkey(); /* consume ~ */
                return KEY_END;
            case '5':
                term_getkey();
                return KEY_PGUP;
            case '6':
                term_getkey();
                return KEY_PGDN;
            case '3':
                term_getkey();
                return KEY_DEL;
            default:
                return ch3;
            }
        }
        /* ESC ESC [ X = Alt-arrow (some terminals) */
        if (ch2 == 27) {
            if (term_kbhit()) {
                int ch3a = term_getkey();
                if (ch3a == '[') {
                    int ch3b = term_getkey();
                    if (ch3b == 'D') return KEY_CTRL_LEFT;   /* Alt-Left */
                    if (ch3b == 'C') return KEY_CTRL_RIGHT;  /* Alt-Right */
                }
            }
            return 27;
        }
        /* Alt-key combos: ESC followed by a letter */
        if (ch2 == 'b' || ch2 == 'B') return KEY_CTRL_LEFT;   /* Alt-B */
        if (ch2 == 'f' || ch2 == 'F') return KEY_CTRL_RIGHT;  /* Alt-F */
        if (ch2 == 'd' || ch2 == 'D') return KEY_ALT_D;       /* Alt-D */
        if (ch2 == 127 || ch2 == 8) return KEY_ALT_BS;        /* Alt-Backspace */
        return ch2;
    }

    /* Normalize backspace */
    if (ch == 127) return 8;

    return ch;
}

/* ---- Screen Rendering ---- */

static void editor_set_message(const char *msg) {
    strncpy(E.message, msg, MAX_MESSAGE - 1);
    E.message[MAX_MESSAGE - 1] = '\0';
}

/* Draw a single row of the editor (text content area) */
static void editor_draw_row(int screen_row, int file_row) {
    /* Position cursor at start of row (1-based) */
    term_gotoxy(screen_row, 1);

    if (file_row >= E.num_lines) {
        /* Beyond end of file: draw tilde + clear */
        term_putc('~');
        term_clear(1);
        return;
    }

    /* Draw line number gutter */
    {
        char gutter[16];
        int g;
        snprintf(gutter, sizeof(gutter), "%*d ", E.gutter_width - 1, file_row + 1);
        for (g = 0; gutter[g]; g++)
            term_putc(gutter[g]);
    }

    /* Draw text content. Tabs stay intact in the buffer and expand only for
     * display, so loading and saving a file does not rewrite it. */
    {
        line_t *l = &E.lines[file_row];
        int text_cols = E.screen_cols - E.gutter_width;
        int visual_col = 0;
        int drawn = 0;
        int j;
        for (j = 0; j < l->len && drawn < text_cols; j++) {
            unsigned char ch = (unsigned char)l->text[j];
            int width = (ch == '\t') ?
                TAB_STOP - (visual_col % TAB_STOP) : 1;
            int cell;
            for (cell = 0; cell < width && drawn < text_cols; cell++) {
                if (visual_col + cell >= E.col_offset) {
                    if (ch == '\t' || cell > 0)
                        term_putc(' ');
                    else if (ch < 32 || ch == 127)
                        term_putc('?');
                    else
                        term_putc(ch);
                    drawn++;
                }
            }
            visual_col += width;
        }
    }
    term_clear(1); /* clear rest of line */
}

static void editor_draw_status_bar(void) {
    char left[128], right[64];
    int left_len, right_len;
    int i, right_start;
    int visual_cx = line_visual_col(&E.lines[E.cy], E.cx);

    snprintf(left, sizeof(left), " %.40s%s",
             E.filename[0] ? E.filename : "[No Name]",
             E.modified ? " [Modified]" : "");
    snprintf(right, sizeof(right), "Ln %d, Col %d  %d lines",
             E.cy + 1, visual_cx + 1, E.num_lines);

    left_len = strlen(left);
    right_len = strlen(right);

    term_gotoxy(E.screen_rows - 1, 1);
    term_set_attr(7); /* reverse video */

    right_start = E.screen_cols - right_len;
    if (right_start <= left_len) right_start = E.screen_cols;
    for (i = 0; i < E.screen_cols; i++) {
        if (i < left_len)
            term_putc(left[i]);
        else if (i >= right_start)
            term_putc(right[i - right_start]);
        else
            term_putc(' ');
    }

    term_set_attr(0); /* normal */
}

static void editor_draw_help_bar(void) {
    const char *help = " ^S Save  ^Q Quit  ^F Find  ^R Replace  ^K Cut  ^U Paste  ^Z Undo  ^Y Redo";
    int len, i;

    term_gotoxy(E.screen_rows, 1);
    term_set_attr(7);

    len = strlen(help);
    for (i = 0; i < E.screen_cols; i++)
        term_putc(i < len ? help[i] : ' ');

    term_set_attr(0);
}

static void editor_scroll(void) {
    /* Vertical scroll */
    if (E.cy < E.row_offset)
        E.row_offset = E.cy;
    if (E.cy >= E.row_offset + E.edit_rows)
        E.row_offset = E.cy - E.edit_rows + 1;

    /* Horizontal scroll */
    {
        int text_cols = E.screen_cols - E.gutter_width;
        int visual_cx = line_visual_col(&E.lines[E.cy], E.cx);
        if (visual_cx < E.col_offset)
            E.col_offset = visual_cx;
        if (visual_cx >= E.col_offset + text_cols)
            E.col_offset = visual_cx - text_cols + 1;
    }
}

static void editor_refresh_screen(void) {
    int y;
    int new_gutter = compute_gutter_width();

    editor_scroll();

    /* Detect changes that require full redraw */
    if (E.row_offset != E.prev_row_offset ||
        E.col_offset != E.prev_col_offset ||
        new_gutter != E.prev_gutter_width) {
        dirty_all();
    }
    E.prev_row_offset = E.row_offset;
    E.prev_col_offset = E.col_offset;
    E.prev_gutter_width = new_gutter;
    E.gutter_width = new_gutter;

    /* Detect status bar changes */
    if (E.num_lines != E.prev_num_lines) {
        E.dirty_status = 1;
        E.prev_num_lines = E.num_lines;
    }

    /* Detect message bar changes */
    if (strcmp(E.message, E.prev_message) != 0) {
        E.dirty_bottom = 1;
        strncpy(E.prev_message, E.message, MAX_MESSAGE - 1);
        E.prev_message[MAX_MESSAGE - 1] = '\0';
    }

    term_begin_update();

    /* Only redraw dirty rows */
    for (y = 0; y < E.edit_rows; y++) {
        if (y < MAX_DIRTY && E.dirty[y]) {
            int file_row = E.row_offset + y;
            editor_draw_row(y + 1, file_row);
            E.dirty[y] = 0;
        }
    }

    if (E.dirty_status) {
        editor_draw_status_bar();
        E.dirty_status = 0;
    }

    if (E.dirty_bottom) {
        /* Message bar or help bar */
        if (E.message[0]) {
            term_gotoxy(E.screen_rows, 1);
            term_set_attr(7);
            {
                int len = strlen(E.message);
                int i;
                for (i = 0; i < E.screen_cols; i++)
                    term_putc(i < len ? E.message[i] : ' ');
            }
            term_set_attr(0);
        } else {
            editor_draw_help_bar();
        }
        E.dirty_bottom = 0;
    }

    /* Position cursor */
    {
        int screen_y = E.cy - E.row_offset + 1;
        int visual_cx = line_visual_col(&E.lines[E.cy], E.cx);
        int screen_x = E.gutter_width + (visual_cx - E.col_offset) + 1;
        term_gotoxy(screen_y, screen_x);
    }

    term_end_update();
}

/* ---- Cursor Movement ---- */

static void editor_clamp_cx(void) {
    if (E.cy < 0) E.cy = 0;
    if (E.cy >= E.num_lines) E.cy = E.num_lines - 1;
    {
        int line_len = E.lines[E.cy].len;
        if (E.cx > line_len) E.cx = line_len;
    }
}

static int is_word_char(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9') || c == '_';
}

static void editor_move_word_left(void) {
    /* Skip non-word chars, then skip word chars */
    if (E.cx == 0 && E.cy > 0) {
        E.cy--;
        E.cx = E.lines[E.cy].len;
    }
    if (E.cy < E.num_lines) {
        line_t *l = &E.lines[E.cy];
        while (E.cx > 0 && !is_word_char(l->text[E.cx - 1]))
            E.cx--;
        while (E.cx > 0 && is_word_char(l->text[E.cx - 1]))
            E.cx--;
    }
}

static void editor_move_word_right(void) {
    if (E.cy < E.num_lines) {
        line_t *l = &E.lines[E.cy];
        /* Skip word chars, then skip non-word chars */
        while (E.cx < l->len && is_word_char(l->text[E.cx]))
            E.cx++;
        while (E.cx < l->len && !is_word_char(l->text[E.cx]))
            E.cx++;
        if (E.cx >= l->len && E.cy < E.num_lines - 1) {
            E.cy++;
            E.cx = 0;
        }
    }
}

static void editor_move_cursor(int key) {
    switch (key) {
    case KEY_UP:
        if (E.cy > 0) E.cy--;
        editor_clamp_cx();
        break;
    case KEY_DOWN:
        if (E.cy < E.num_lines - 1) E.cy++;
        editor_clamp_cx();
        break;
    case KEY_LEFT:
        if (E.cx > 0) {
            E.cx--;
        } else if (E.cy > 0) {
            E.cy--;
            E.cx = E.lines[E.cy].len;
        }
        break;
    case KEY_RIGHT:
        if (E.cy < E.num_lines) {
            int line_len = E.lines[E.cy].len;
            if (E.cx < line_len) {
                E.cx++;
            } else if (E.cy < E.num_lines - 1) {
                E.cy++;
                E.cx = 0;
            }
        }
        break;
    case KEY_HOME:
    case CTRL('a'):
        E.cx = 0;
        break;
    case KEY_END:
    case CTRL('e'):
        if (E.cy < E.num_lines)
            E.cx = E.lines[E.cy].len;
        break;
    case KEY_PGUP:
        if (E.cy > E.edit_rows)
            E.cy -= E.edit_rows;
        else
            E.cy = 0;
        editor_clamp_cx();
        break;
    case KEY_PGDN:
        E.cy += E.edit_rows;
        if (E.cy >= E.num_lines) E.cy = E.num_lines - 1;
        editor_clamp_cx();
        break;
    case KEY_CTRL_LEFT:
        editor_move_word_left();
        break;
    case KEY_CTRL_RIGHT:
        editor_move_word_right();
        break;
    }
}

/* ---- Editing Operations ---- */

static void editor_insert_char(int ch) {
    if (E.cy >= E.num_lines) return;
    undo_push(UNDO_INSERT_CHAR, E.cy, E.cx, (char)ch, NULL, 0);
    line_insert_char(&E.lines[E.cy], E.cx, (char)ch);
    E.cx++;
    E.quit_count = 0;
    E.last_was_cut = 0;
    dirty_file_row(E.cy);
}

static void editor_insert_newline(void) {
    if (E.cy >= E.num_lines) return;

    /* Capture indentation in bytes; tabs remain tabs. */
    int indent = 0;
    {
        line_t *l = &E.lines[E.cy];
        while (indent < l->len &&
               (l->text[indent] == ' ' || l->text[indent] == '\t'))
            indent++;
        if (indent > E.cx) indent = E.cx;
    }

    undo_begin();
    undo_push(UNDO_INSERT_LINE, E.cy, E.cx, 0, NULL, 0);

    editor_split_line(E.cy, E.cx);
    E.cy++;
    E.cx = 0;

    /* Auto-indent: prepend leading spaces from previous line */
    if (indent > 0) {
        line_t *newl = &E.lines[E.cy];
        undo_push(UNDO_INSERT_TEXT, E.cy, 0, 0,
                  E.lines[E.cy - 1].text, indent);
        line_insert_text(newl, 0, E.lines[E.cy - 1].text, indent);
        E.cx = indent;
    }
    undo_end();

    E.quit_count = 0;
    E.last_was_cut = 0;
    dirty_from_file_row(E.cy - 1);
    E.dirty_status = 1;
}

static void editor_insert_tab(void) {
    char indent[TAB_STOP];
    int visual_cx;
    int spaces;
    if (E.cy >= E.num_lines) return;
    visual_cx = line_visual_col(&E.lines[E.cy], E.cx);
    spaces = TAB_STOP - (visual_cx % TAB_STOP);
    memset(indent, ' ', sizeof(indent));
    undo_push(UNDO_INSERT_TEXT, E.cy, E.cx, 0, indent, spaces);
    line_insert_text(&E.lines[E.cy], E.cx, indent, spaces);
    E.cx += spaces;
    E.quit_count = 0;
    E.last_was_cut = 0;
    dirty_file_row(E.cy);
}

static void editor_backspace(void) {
    if (E.cy >= E.num_lines) return;
    if (E.cx > 0) {
        char deleted = E.lines[E.cy].text[E.cx - 1];
        undo_push(UNDO_DELETE_CHAR, E.cy, E.cx - 1, deleted, NULL, 0);
        line_delete_char(&E.lines[E.cy], E.cx - 1);
        E.cx--;
        dirty_file_row(E.cy);
    } else if (E.cy > 0) {
        /* Join with previous line */
        int join_cx = E.lines[E.cy - 1].len;
        undo_push(UNDO_DELETE_LINE, E.cy - 1, join_cx, 0, NULL, 0);
        E.cx = join_cx;
        editor_join_lines(E.cy - 1);
        E.cy--;
        dirty_from_file_row(E.cy);
        E.dirty_status = 1;
    }
    E.quit_count = 0;
    E.last_was_cut = 0;
}

static void editor_delete(void) {
    if (E.cy >= E.num_lines) return;
    if (E.cx < E.lines[E.cy].len) {
        char deleted = E.lines[E.cy].text[E.cx];
        undo_push(UNDO_DELETE_CHAR, E.cy, E.cx, deleted, NULL, 0);
        line_delete_char(&E.lines[E.cy], E.cx);
        dirty_file_row(E.cy);
    } else if (E.cy < E.num_lines - 1) {
        /* Join with next line */
        undo_push(UNDO_DELETE_LINE, E.cy, E.cx, 0, NULL, 0);
        editor_join_lines(E.cy);
        dirty_from_file_row(E.cy);
        E.dirty_status = 1;
    }
    E.quit_count = 0;
    E.last_was_cut = 0;
}

static void cut_buf_clear(void) {
    int i;
    for (i = 0; i < E.cut_count; i++)
        line_free(&E.cut_lines[i]);
    E.cut_count = 0;
}

static void cut_buf_append(const char *text, int len) {
    if (E.cut_count >= E.cut_cap) {
        E.cut_cap = E.cut_cap ? E.cut_cap * 2 : 8;
        E.cut_lines = realloc(E.cut_lines, E.cut_cap * sizeof(line_t));
    }
    line_init(&E.cut_lines[E.cut_count]);
    line_set(&E.cut_lines[E.cut_count], text, len);
    E.cut_count++;
}

static void editor_cut_line(void) {
    if (E.cy >= E.num_lines) return;

    /* The sole empty line has nothing to remove. Return before touching the
     * cut buffer so a held ^K cannot grow it without bound, and leave
     * last_was_cut alone so an in-progress run of cuts stays intact. */
    if (E.num_lines == 1 && E.lines[0].len == 0) {
        E.quit_count = 0;
        editor_set_message("Nothing to cut");
        return;
    }

    /* If last action wasn't cut, clear the buffer */
    if (!E.last_was_cut)
        cut_buf_clear();

    /* Append to cut buffer */
    cut_buf_append(E.lines[E.cy].text, E.lines[E.cy].len);

    if (E.num_lines == 1) {
        /* Clear the sole line in place: deleting it would leave zero lines. */
        undo_push(UNDO_DELETE_TEXT, 0, 0, 0, E.lines[0].text, E.lines[0].len);
        line_set(&E.lines[0], "", 0);
        E.cx = 0;
    } else {
        undo_push(UNDO_FULL_LINE, E.cy, E.cx, 0,
                  E.lines[E.cy].text, E.lines[E.cy].len);
        editor_delete_line(E.cy);
        if (E.cy >= E.num_lines)
            E.cy = E.num_lines - 1;
        editor_clamp_cx();
    }
    E.quit_count = 0;
    E.last_was_cut = 1;
    {
        char msg[MAX_MESSAGE];
        snprintf(msg, MAX_MESSAGE, "%d line%s cut", E.cut_count, E.cut_count == 1 ? "" : "s");
        editor_set_message(msg);
    }
    dirty_from_file_row(E.cy);
    E.dirty_status = 1;
}

static void editor_duplicate_line(void) {
    if (E.cy >= E.num_lines) return;
    line_t *l = &E.lines[E.cy];
    editor_insert_line(E.cy + 1, l->text, l->len);
    E.cy++;
    undo_push(UNDO_ADD_LINE, E.cy, 0, 0, E.lines[E.cy].text, E.lines[E.cy].len);
    E.quit_count = 0;
    E.last_was_cut = 0;
    dirty_from_file_row(E.cy - 1);
    E.dirty_status = 1;
}

static void editor_paste_line(void) {
    int i;
    if (E.cut_count == 0) {
        editor_set_message("Nothing to paste");
        return;
    }
    undo_begin();
    for (i = 0; i < E.cut_count; i++) {
        editor_insert_line(E.cy + 1, E.cut_lines[i].text, E.cut_lines[i].len);
        E.cy++;
        undo_push(UNDO_ADD_LINE, E.cy, 0, 0, E.cut_lines[i].text, E.cut_lines[i].len);
    }
    undo_end();
    E.cx = 0;
    E.quit_count = 0;
    E.last_was_cut = 0;
    {
        char msg[MAX_MESSAGE];
        snprintf(msg, MAX_MESSAGE, "%d line%s pasted", E.cut_count, E.cut_count == 1 ? "" : "s");
        editor_set_message(msg);
    }
    dirty_from_file_row(E.cy - E.cut_count + 1);
    E.dirty_status = 1;
}

/* ---- Search ---- */

static void editor_prompt(const char *prompt, char *buf, int bufsize) {
    int pos = strlen(buf);
    int key;

    for (;;) {
        /* Draw prompt on bottom line */
        term_gotoxy(E.screen_rows, 1);
        term_set_attr(7);
        {
            int len, i;
            term_puts(prompt);
            len = strlen(prompt);
            /* Show current input */
            for (i = 0; i < pos && len + i < E.screen_cols - 1; i++)
                term_putc(buf[i]);
            len += i;
            for (; len < E.screen_cols; len++)
                term_putc(' ');
        }
        term_set_attr(0);
        /* Position cursor in prompt */
        term_gotoxy(E.screen_rows, (int)strlen(prompt) + pos + 1);

        key = read_key();
        if (key == '\r' || key == '\n') {
            break;
        }
        if (key == 27 || key == -1) {
            buf[0] = '\0';
            break;
        }
        if (key == 8) { /* backspace */
            if (pos > 0) {
                pos--;
                buf[pos] = '\0';
            }
            continue;
        }
        if (key >= 32 && key < 127 && pos < bufsize - 1) {
            buf[pos++] = (char)key;
            buf[pos] = '\0';
        }
    }
    E.message[0] = '\0'; /* clear message area */
}

static void editor_find(void) {
    int orig_cy = E.cy;
    int orig_cx = E.cx;

    editor_prompt("Search: ", E.search_buf, MAX_SEARCH);
    if (E.search_buf[0] == '\0') {
        editor_set_message("Search cancelled");
        return;
    }

    /* Search forward from current position */
    {
        int slen = strlen(E.search_buf);
        int y, start_x;

        /* Start from current line, position after cursor */
        for (y = E.cy; y < E.num_lines; y++) {
            line_t *l = &E.lines[y];
            start_x = (y == E.cy) ? E.cx + 1 : 0;
            {
                int x;
                for (x = start_x; x <= l->len - slen; x++) {
                    if (memcmp(l->text + x, E.search_buf, slen) == 0) {
                        E.cy = y;
                        E.cx = x;
                        editor_set_message("Found");
                        return;
                    }
                }
            }
        }
        /* Wrap to beginning */
        for (y = 0; y <= orig_cy; y++) {
            line_t *l = &E.lines[y];
            int limit = (y == orig_cy) ? orig_cx : l->len - slen;
            {
                int x;
                for (x = 0; x <= limit; x++) {
                    if (x <= l->len - slen &&
                        memcmp(l->text + x, E.search_buf, slen) == 0) {
                        E.cy = y;
                        E.cx = x;
                        editor_set_message("Found (wrapped)");
                        return;
                    }
                }
            }
        }
    }
    editor_set_message("Not found");
}

static void editor_delete_word_forward(void) {
    if (E.cy >= E.num_lines) return;
    line_t *l = &E.lines[E.cy];
    int start = E.cx;
    /* Skip word chars, then non-word chars */
    while (E.cx < l->len && is_word_char(l->text[E.cx])) E.cx++;
    while (E.cx < l->len && !is_word_char(l->text[E.cx])) E.cx++;
    if (E.cx > start) {
        int text_len = E.cx - start;
        undo_push(UNDO_DELETE_TEXT, E.cy, start, 0, l->text + start, text_len);
        line_delete_text(l, start, text_len);
        E.cx = start;
        dirty_file_row(E.cy);
    }
    E.quit_count = 0;
    E.last_was_cut = 0;
}

static void editor_delete_word_backward(void) {
    if (E.cy >= E.num_lines) return;
    line_t *l = &E.lines[E.cy];
    int end = E.cx;
    /* Skip non-word chars, then word chars */
    while (E.cx > 0 && !is_word_char(l->text[E.cx - 1])) E.cx--;
    while (E.cx > 0 && is_word_char(l->text[E.cx - 1])) E.cx--;
    if (end > E.cx) {
        int text_len = end - E.cx;
        undo_push(UNDO_DELETE_TEXT, E.cy, E.cx, 0, l->text + E.cx, text_len);
        line_delete_text(l, E.cx, text_len);
        dirty_file_row(E.cy);
    }
    E.quit_count = 0;
    E.last_was_cut = 0;
}

static void editor_indent_line(void) {
    char indent[TAB_STOP];
    line_t *l;
    if (E.cy >= E.num_lines) return;
    l = &E.lines[E.cy];
    memset(indent, ' ', sizeof(indent));
    undo_push(UNDO_INSERT_TEXT, E.cy, 0, 0, indent, TAB_STOP);
    line_insert_text(l, 0, indent, TAB_STOP);
    E.cx += TAB_STOP;
    E.quit_count = 0;
    E.last_was_cut = 0;
    dirty_file_row(E.cy);
}

/* Remove one indent level of leading whitespace. Measured in visual columns,
 * not bytes, so this reverses a tab-indented line as well as a space-indented
 * one — loading no longer rewrites tabs, so both occur in practice. */
static void editor_unindent_line(void) {
    line_t *l;
    int removed = 0;    /* leading bytes to drop */
    int cols = 0;       /* visual columns those bytes cover */

    if (E.cy >= E.num_lines) return;
    l = &E.lines[E.cy];
    while (removed < l->len && cols < TAB_STOP) {
        char ch = l->text[removed];
        if (ch == ' ')
            cols++;
        else if (ch == '\t')
            cols += TAB_STOP - (cols % TAB_STOP);
        else
            break;
        removed++;
    }
    if (removed > 0) {
        undo_push(UNDO_DELETE_TEXT, E.cy, 0, 0, l->text, removed);
        line_delete_text(l, 0, removed);
        if (E.cx >= removed)
            E.cx -= removed;
        else
            E.cx = 0;
        dirty_file_row(E.cy);
    }
    E.quit_count = 0;
    E.last_was_cut = 0;
}

static void editor_replace_at(int y, int x, const char *search, int slen,
                              const char *replacement, int rlen) {
    line_t *l = &E.lines[y];

    undo_begin();
    undo_push(UNDO_DELETE_TEXT, y, x, 0, search, slen);
    undo_push(UNDO_INSERT_TEXT, y, x, 0, replacement, rlen);
    line_delete_text(l, x, slen);
    line_insert_text(l, x, replacement, rlen);
    undo_end();
    dirty_file_row(y);
}

/* Replace every occurrence at or after from_x in line y, recording ONE undo
 * entry pair for the whole line rather than a pair per occurrence. Undo cost
 * then scales with lines touched instead of matches, which keeps a large
 * replace-all inside UNDO_MAX — overflowing it discards the entire history,
 * since a partial transaction can never be safely retained. Returns the
 * number of replacements made. */
static int editor_replace_line_all(int y, int from_x,
                                   const char *search, int slen,
                                   const char *replacement, int rlen) {
    line_t *l = &E.lines[y];
    line_t built;
    int count = 0;
    int x;

    if (slen <= 0) return 0;
    if (from_x < 0) from_x = 0;
    if (from_x > l->len) from_x = l->len;

    line_init(&built);
    line_append(&built, l->text, from_x);
    for (x = from_x; x <= l->len - slen; ) {
        if (memcmp(l->text + x, search, slen) == 0) {
            line_append(&built, replacement, rlen);
            x += slen;
            count++;
        } else {
            line_append(&built, l->text + x, 1);
            x++;
        }
    }
    line_append(&built, l->text + x, l->len - x);

    if (count > 0) {
        undo_begin();
        undo_push(UNDO_DELETE_TEXT, y, 0, 0, l->text, l->len);
        undo_push(UNDO_INSERT_TEXT, y, 0, 0, built.text, built.len);
        undo_end();
        line_set(l, built.text, built.len);
        dirty_file_row(y);
    }
    line_free(&built);
    return count;
}

static void editor_find_replace(void) {
    char replace_buf[MAX_SEARCH] = "";
    int slen, rlen;
    int replaced = 0;

    editor_prompt("Search: ", E.search_buf, MAX_SEARCH);
    if (E.search_buf[0] == '\0') {
        editor_set_message("Replace cancelled");
        return;
    }
    editor_prompt("Replace with: ", replace_buf, MAX_SEARCH);
    /* Empty replacement is valid (delete occurrences) */

    slen = strlen(E.search_buf);
    rlen = strlen(replace_buf);
    undo_begin();

    /* Search forward from current position, prompt for each */
    {
        int y, x, start_x;
        for (y = E.cy; y < E.num_lines; y++) {
            line_t *l = &E.lines[y];
            start_x = (y == E.cy) ? E.cx : 0;
            for (x = start_x; x <= l->len - slen; x++) {
                if (memcmp(l->text + x, E.search_buf, slen) == 0) {
                    char confirm[8] = "";
                    /* Move cursor to match */
                    E.cy = y;
                    E.cx = x;
                    dirty_file_row(y);
                    editor_refresh_screen();

                    editor_prompt("Replace? (y/n/a/q): ", confirm, sizeof(confirm));
                    if (confirm[0] == 'q' || confirm[0] == '\0')
                        goto done;

                    if (confirm[0] == 'y' || confirm[0] == 'a') {
                        /* Delete old, insert new */
                        editor_replace_at(y, x, E.search_buf, slen,
                                          replace_buf, rlen);
                        l = &E.lines[y];
                        E.cx = x + rlen;
                        replaced++;
                        x += rlen - 1; /* advance past replacement */

                        if (confirm[0] == 'a') {
                            /* Replace all remaining without prompting, one
                             * undo entry pair per line touched. */
                            replaced += editor_replace_line_all(
                                y, E.cx, E.search_buf, slen, replace_buf, rlen);
                            for (y = y + 1; y < E.num_lines; y++)
                                replaced += editor_replace_line_all(
                                    y, 0, E.search_buf, slen, replace_buf, rlen);
                            goto done;
                        }
                    }
                }
            }
        }
    }
done:
    undo_end();
    E.quit_count = 0;
    {
        char msg[MAX_MESSAGE];
        snprintf(msg, MAX_MESSAGE, "Replaced %d occurrence%s", replaced, replaced == 1 ? "" : "s");
        editor_set_message(msg);
    }
}

/* ---- Key Dispatch ---- */

static void editor_process_key(int key) {
    /* Clear message on any key (unless it's a message-producing action) */
    if (key != CTRL('q'))
        E.message[0] = '\0';

    switch (key) {
    case KEY_UP:
    case KEY_DOWN:
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_HOME:
    case KEY_END:
    case KEY_PGUP:
    case KEY_PGDN:
    case CTRL('a'):
    case CTRL('e'):
    case KEY_CTRL_LEFT:
    case KEY_CTRL_RIGHT:
        editor_move_cursor(key);
        E.quit_count = 0;
        E.dirty_status = 1;  /* cursor position changed */
        break;

    case 8: /* backspace */
        editor_backspace();
        break;

    case KEY_DEL:
        editor_delete();
        break;

    case '\r':
    case '\n':
        editor_insert_newline();
        break;

    case '\t':
        editor_insert_tab();
        break;

    case CTRL('s'): /* save */
        editor_save();
        E.dirty_status = 1;
        break;

    case CTRL('q'): /* quit */
        if (E.modified) {
            E.quit_count++;
            if (E.quit_count < QUIT_CONFIRM) {
                snprintf(E.message, MAX_MESSAGE,
                         "Unsaved changes! Press Ctrl-Q again to quit without saving.");
                break;
            }
        }
        E.running = 0;
        break;

    case CTRL('x'): /* save and quit */
        if (editor_save() == 0)
            E.running = 0;
        E.dirty_status = 1;
        E.dirty_bottom = 1;
        break;

    case CTRL('f'): /* find */
        editor_find();
        E.dirty_status = 1;
        E.dirty_bottom = 1;
        break;

    case CTRL('k'): /* cut line */
        editor_cut_line();
        break;

    case CTRL('u'): /* paste line */
        editor_paste_line();
        break;

    case CTRL('d'): /* duplicate line */
        editor_duplicate_line();
        break;

    case CTRL('r'): /* find & replace */
        editor_find_replace();
        E.dirty_status = 1;
        E.dirty_bottom = 1;
        break;

    case CTRL('z'): /* undo */
        editor_undo();
        break;

    case CTRL('y'): /* redo */
        editor_redo();
        break;

    case KEY_ALT_D: /* delete word forward */
        editor_delete_word_forward();
        break;

    case KEY_ALT_BS: /* delete word backward */
        editor_delete_word_backward();
        break;

    case CTRL(']'): /* indent line */
        editor_indent_line();
        break;

    case CTRL('t'): /* unindent line */
        editor_unindent_line();
        break;

    case CTRL('g'): /* go to line */
        {
            char linebuf[16] = "";
            int lineno;
            editor_prompt("Go to line: ", linebuf, sizeof(linebuf));
            if (linebuf[0] == '\0') break;
            lineno = atoi(linebuf);
            if (lineno < 1) lineno = 1;
            if (lineno > E.num_lines) lineno = E.num_lines;
            E.cy = lineno - 1;
            E.cx = 0;
            editor_clamp_cx();
            E.dirty_status = 1;
            E.dirty_bottom = 1;
        }
        break;

    case 27: /* escape - clear message */
        E.message[0] = '\0';
        E.quit_count = 0;
        break;

    default:
        /* Printable characters */
        if (key >= 32 && key < 127) {
            editor_insert_char(key);
        }
        break;
    }
}

/* ---- Buffer Unit Tests (--test flag) ---- */

static int test_count, test_pass;

static void test_assert(int cond, const char *msg) {
    test_count++;
    if (cond) {
        test_pass++;
    } else {
        printf("FAIL: %s\n", msg);
    }
}

static void editor_reset(void) {
    int i;
    undo_clear();
    cut_buf_clear();
    for (i = 0; i < E.num_lines; i++)
        line_free(&E.lines[i]);
    E.num_lines = 0;
    E.cx = E.cy = 0;
    E.row_offset = E.col_offset = 0;
    E.modified = 0;
    E.last_was_cut = 0;
    E.filename[0] = '\0';
    E.search_buf[0] = '\0';
    E.message[0] = '\0';
    E.current_state = 0;
    E.saved_state = 0;
    E.next_state = 1;
    E.next_undo_group = 1;
}

static int run_tests(void) {
    test_count = 0;
    test_pass = 0;

    /* Initialize minimal editor state for tests */
    E.lines_cap = LINES_INIT_CAP;
    E.lines = malloc(E.lines_cap * sizeof(line_t));
    E.num_lines = 0;
    E.screen_rows = 24;
    E.screen_cols = 80;
    E.edit_rows = 22;
    E.cut_lines = NULL;
    E.cut_count = 0;
    E.cut_cap = 0;
    E.undo_top = 0;
    E.undo_count = 0;
    E.next_state = 1;
    E.next_undo_group = 1;

    printf("Running buffer unit tests...\n");

    /* Test 1: line_init and line_set */
    {
        line_t l;
        line_init(&l);
        test_assert(l.len == 0, "line_init: len == 0");
        test_assert(l.text[0] == '\0', "line_init: empty string");
        line_set(&l, "Hello", 5);
        test_assert(l.len == 5, "line_set: len == 5");
        test_assert(strcmp(l.text, "Hello") == 0, "line_set: text == Hello");
        line_free(&l);
    }

    /* Test 2: line_insert_char */
    {
        line_t l;
        line_init(&l);
        line_set(&l, "Hllo", 4);
        line_insert_char(&l, 1, 'e');
        test_assert(l.len == 5, "line_insert_char: len == 5");
        test_assert(strcmp(l.text, "Hello") == 0, "line_insert_char: text == Hello");
        line_free(&l);
    }

    /* Test 3: line_delete_char */
    {
        line_t l;
        line_init(&l);
        line_set(&l, "Heello", 6);
        line_delete_char(&l, 2);
        test_assert(l.len == 5, "line_delete_char: len == 5");
        test_assert(strcmp(l.text, "Hello") == 0, "line_delete_char: text == Hello");
        line_free(&l);
    }

    /* Test 4: editor_insert_line */
    {
        editor_reset();
        editor_insert_line(0, "First", 5);
        editor_insert_line(1, "Third", 5);
        editor_insert_line(1, "Second", 6);
        test_assert(E.num_lines == 3, "insert_line: 3 lines");
        test_assert(strcmp(E.lines[0].text, "First") == 0, "insert_line: line 0 == First");
        test_assert(strcmp(E.lines[1].text, "Second") == 0, "insert_line: line 1 == Second");
        test_assert(strcmp(E.lines[2].text, "Third") == 0, "insert_line: line 2 == Third");
    }

    /* Test 5: editor_delete_line */
    {
        editor_reset();
        editor_insert_line(0, "A", 1);
        editor_insert_line(1, "B", 1);
        editor_insert_line(2, "C", 1);
        editor_delete_line(1);
        test_assert(E.num_lines == 2, "delete_line: 2 lines");
        test_assert(strcmp(E.lines[0].text, "A") == 0, "delete_line: line 0 == A");
        test_assert(strcmp(E.lines[1].text, "C") == 0, "delete_line: line 1 == C");
    }

    /* Test 6: editor_split_line */
    {
        editor_reset();
        editor_insert_line(0, "HelloWorld", 10);
        editor_split_line(0, 5);
        test_assert(E.num_lines == 2, "split_line: 2 lines");
        test_assert(strcmp(E.lines[0].text, "Hello") == 0, "split_line: line 0 == Hello");
        test_assert(strcmp(E.lines[1].text, "World") == 0, "split_line: line 1 == World");
    }

    /* Test 7: editor_join_lines */
    {
        editor_reset();
        editor_insert_line(0, "Hello", 5);
        editor_insert_line(1, "World", 5);
        editor_join_lines(0);
        test_assert(E.num_lines == 1, "join_lines: 1 line");
        test_assert(strcmp(E.lines[0].text, "HelloWorld") == 0, "join_lines: text == HelloWorld");
    }

    /* Test 8: cut and paste */
    {
        editor_reset();
        editor_insert_line(0, "Line1", 5);
        editor_insert_line(1, "Line2", 5);
        editor_insert_line(2, "Line3", 5);
        E.cy = 1; E.cx = 0;
        editor_cut_line();
        test_assert(E.num_lines == 2, "cut: 2 lines remain");
        test_assert(E.cut_count == 1, "cut: buffer has 1 line");
        test_assert(strcmp(E.cut_lines[0].text, "Line2") == 0, "cut: buffer == Line2");
        editor_paste_line();
        test_assert(E.num_lines == 3, "paste: 3 lines");
        test_assert(strcmp(E.lines[2].text, "Line2") == 0, "paste: line 2 == Line2");
        editor_undo();
        test_assert(E.num_lines == 2 && strcmp(E.lines[1].text, "Line3") == 0,
                    "paste: one undo removes entire paste");
        editor_redo();
        test_assert(E.num_lines == 3 && strcmp(E.lines[2].text, "Line2") == 0,
                    "paste: one redo restores entire paste");
    }

    /* Test 9: insert char at cursor */
    {
        editor_reset();
        editor_insert_line(0, "AC", 2);
        E.cy = 0; E.cx = 1;
        editor_insert_char('B');
        test_assert(strcmp(E.lines[0].text, "ABC") == 0, "insert_char: text == ABC");
        test_assert(E.cx == 2, "insert_char: cx == 2");
    }

    /* Test 10: backspace joining lines */
    {
        editor_reset();
        editor_insert_line(0, "Hello", 5);
        editor_insert_line(1, "World", 5);
        E.cy = 1; E.cx = 0;
        editor_backspace();
        test_assert(E.num_lines == 1, "backspace-join: 1 line");
        test_assert(strcmp(E.lines[0].text, "HelloWorld") == 0, "backspace-join: text == HelloWorld");
        test_assert(E.cx == 5, "backspace-join: cx == 5");
    }

    /* Test 11: gutter width calculation */
    {
        editor_reset();
        int i;
        for (i = 0; i < 99; i++)
            editor_insert_line(i, "", 0);
        test_assert(compute_gutter_width() == 3, "gutter_width: 99 lines -> 3");
        editor_insert_line(99, "", 0);
        test_assert(compute_gutter_width() == 4, "gutter_width: 100 lines -> 4");
    }

    /* Test 12: line_append */
    {
        line_t l;
        line_init(&l);
        line_set(&l, "Hello", 5);
        line_append(&l, " World", 6);
        test_assert(l.len == 11, "line_append: len == 11");
        test_assert(strcmp(l.text, "Hello World") == 0, "line_append: text == Hello World");
        line_free(&l);
    }

    /* Test 13: auto-indent (newline preserves leading spaces) */
    {
        editor_reset();
        editor_insert_line(0, "    indented text", 17);
        E.cy = 0; E.cx = 17; /* end of line */
        editor_insert_newline();
        test_assert(E.num_lines == 2, "auto-indent: 2 lines");
        test_assert(E.cx == 4, "auto-indent: cx == 4");
        test_assert(memcmp(E.lines[1].text, "    ", 4) == 0, "auto-indent: 4 leading spaces");
        editor_undo();
        test_assert(E.num_lines == 1, "auto-indent undo: one command");
        test_assert(strcmp(E.lines[0].text, "    indented text") == 0,
                    "auto-indent undo: original line");
        editor_redo();
        test_assert(E.num_lines == 2 && E.lines[1].len == 4,
                    "auto-indent redo: entire command");

        editor_reset();
        editor_insert_line(0, "    x", 5);
        E.cx = 2;
        editor_insert_newline();
        test_assert(strcmp(E.lines[1].text, "    x") == 0,
                    "auto-indent: split inside indentation has no embedded NUL");
    }

    /* Test 14: duplicate line */
    {
        editor_reset();
        editor_insert_line(0, "Hello", 5);
        editor_insert_line(1, "World", 5);
        E.cy = 0; E.cx = 0;
        editor_duplicate_line();
        test_assert(E.num_lines == 3, "dup_line: 3 lines");
        test_assert(strcmp(E.lines[0].text, "Hello") == 0, "dup_line: line 0 == Hello");
        test_assert(strcmp(E.lines[1].text, "Hello") == 0, "dup_line: line 1 == Hello (dup)");
        test_assert(strcmp(E.lines[2].text, "World") == 0, "dup_line: line 2 == World");
    }

    /* Test 15: word movement */
    {
        editor_reset();
        editor_insert_line(0, "hello world foo_bar", 19);
        E.cy = 0; E.cx = 0;
        editor_move_word_right();
        test_assert(E.cx == 6, "word_right: cx == 6 (start of world)");
        editor_move_word_left();
        test_assert(E.cx == 0, "word_left: cx == 0 (start of hello)");
    }

    /* Test 16: grouped replacement and undo */
    {
        editor_reset();
        editor_insert_line(0, "foo bar foo baz", 15);
        undo_begin();
        editor_replace_at(0, 0, "foo", 3, "quux", 4);
        editor_replace_at(0, 9, "foo", 3, "quux", 4);
        undo_end();
        test_assert(strcmp(E.lines[0].text, "quux bar quux baz") == 0,
                    "replace: both occurrences");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "foo bar foo baz") == 0,
                    "replace undo: entire command");
        editor_redo();
        test_assert(strcmp(E.lines[0].text, "quux bar quux baz") == 0,
                    "replace redo: entire command");
    }

    /* Test 17: undo insert char */
    {
        editor_reset();
        editor_insert_line(0, "AB", 2);
        E.cy = 0; E.cx = 1;
        editor_insert_char('X');
        test_assert(strcmp(E.lines[0].text, "AXB") == 0, "undo: inserted AXB");
        /* Undo it */
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "AB") == 0, "undo: back to AB");
        test_assert(E.cx == 1, "undo: cx restored to 1");
        /* Redo it */
        editor_redo();
        test_assert(strcmp(E.lines[0].text, "AXB") == 0, "redo: back to AXB");
    }

    /* Test 18: undo delete char */
    {
        editor_reset();
        editor_insert_line(0, "ABC", 3);
        E.cy = 0; E.cx = 1;
        editor_delete();
        test_assert(strcmp(E.lines[0].text, "AC") == 0, "undo-del: deleted to AC");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "ABC") == 0, "undo-del: back to ABC");
    }

    /* Test 19: multi-line cut */
    {
        editor_reset();
        E.last_was_cut = 0;
        cut_buf_clear();
        editor_insert_line(0, "Line1", 5);
        editor_insert_line(1, "Line2", 5);
        editor_insert_line(2, "Line3", 5);
        E.cy = 0; E.cx = 0;
        editor_cut_line();
        editor_cut_line();
        test_assert(E.cut_count == 2, "multi-cut: 2 lines in buffer");
        test_assert(strcmp(E.cut_lines[0].text, "Line1") == 0, "multi-cut: line 0");
        test_assert(strcmp(E.cut_lines[1].text, "Line2") == 0, "multi-cut: line 1");
        editor_undo();
        editor_undo();
        test_assert(E.num_lines == 3 && strcmp(E.lines[0].text, "Line1") == 0,
                    "multi-cut: undo restores both lines");
    }

    /* Test 20: indent/unindent */
    {
        editor_reset();
        editor_insert_line(0, "hello", 5);
        E.cy = 0; E.cx = 0;
        editor_indent_line();
        test_assert(strcmp(E.lines[0].text, "    hello") == 0, "indent: 4 spaces added");
        test_assert(E.cx == 4, "indent: cx == 4");
        editor_unindent_line();
        test_assert(strcmp(E.lines[0].text, "hello") == 0, "unindent: spaces removed");
        test_assert(E.cx == 0, "unindent: cx == 0");
    }

    /* Test 21: delete word forward */
    {
        editor_reset();
        editor_insert_line(0, "hello world", 11);
        E.cy = 0; E.cx = 0;
        editor_delete_word_forward();
        test_assert(strcmp(E.lines[0].text, "world") == 0, "del-word-fwd: hello removed");
        test_assert(E.cx == 0, "del-word-fwd: cx == 0");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "hello world") == 0,
                    "del-word-fwd: one undo restores text in order");
    }

    /* Test 22: multiple undo and redo operations retain the whole tail */
    {
        editor_reset();
        editor_insert_line(0, "", 0);
        editor_insert_char('A');
        editor_insert_char('B');
        editor_insert_char('C');
        editor_undo();
        editor_undo();
        editor_undo();
        test_assert(E.lines[0].len == 0, "multi-undo: all three edits undone");
        editor_redo();
        editor_redo();
        editor_redo();
        test_assert(strcmp(E.lines[0].text, "ABC") == 0,
                    "multi-redo: all three edits restored");
        editor_undo();
        editor_insert_char('X');
        editor_redo();
        test_assert(strcmp(E.lines[0].text, "ABX") == 0,
                    "history branch: new edit discards redo tail");
    }

    /* Test 23: save point follows undo and redo state */
    {
        const char *path = "/tmp/slow32-nano-savepoint.txt";
        editor_reset();
        remove(path);
        editor_insert_line(0, "", 0);
        editor_insert_char('A');
        strncpy(E.filename, path, MAX_FILENAME - 1);
        E.filename[MAX_FILENAME - 1] = '\0';
        test_assert(editor_save() == 0, "save-point: save succeeds");
        test_assert(!E.modified, "save-point: clean after save");
        editor_insert_char('B');
        test_assert(E.modified, "save-point: modified after edit");
        editor_undo();
        test_assert(!E.modified && strcmp(E.lines[0].text, "A") == 0,
                    "save-point: undo back to clean state");
        editor_redo();
        test_assert(E.modified && strcmp(E.lines[0].text, "AB") == 0,
                    "save-point: redo leaves clean state");
        remove(path);
    }

    /* Test 24: cutting the sole line does not create a phantom line */
    {
        editor_reset();
        editor_insert_line(0, "only", 4);
        editor_cut_line();
        test_assert(E.num_lines == 1 && E.lines[0].len == 0,
                    "sole-cut: leaves one empty line");
        editor_undo();
        test_assert(E.num_lines == 1 && strcmp(E.lines[0].text, "only") == 0,
                    "sole-cut: undo restores exactly one line");
        editor_redo();
        test_assert(E.num_lines == 1 && E.lines[0].len == 0,
                    "sole-cut: redo leaves exactly one line");
    }

    /* Test 25: tab display columns do not alter buffer bytes */
    {
        editor_reset();
        editor_insert_line(0, "a\tb", 3);
        test_assert(line_visual_col(&E.lines[0], 2) == 4,
                    "tabs: byte column maps to tab stop");
        test_assert(E.lines[0].text[1] == '\t', "tabs: tab byte retained");
    }

    /* Test 26: long lines, tabs, CRLF, and final newlines round-trip */
    {
        const char *path = "/tmp/slow32-nano-roundtrip.txt";
        FILE *f;
        int i;
        int ch;
        int count = 0;
        int tab_at = -1;
        int cr_at = -1;

        editor_reset();
        remove(path);
        f = fopen(path, "w");
        test_assert(f != NULL, "round-trip: create fixture");
        if (f) {
            for (i = 0; i < 5000; i++) fputc('x', f);
            fputc('\t', f);
            fputc('Z', f);
            fputc('\r', f);
            fputc('\n', f);
            test_assert(fflush(f) == 0 && !ferror(f),
                        "round-trip: fixture write succeeds");
            fclose(f);
        }

        editor_load(path);
        test_assert(E.num_lines == 2, "round-trip: final empty line retained");
        test_assert(E.lines[0].len == 5003, "round-trip: long line not truncated");
        test_assert(E.lines[0].text[5000] == '\t', "round-trip: tab retained");
        test_assert(E.lines[0].text[5002] == '\r', "round-trip: CR retained");
        test_assert(editor_save() == 0, "round-trip: atomic save succeeds");

        f = fopen(path, "r");
        test_assert(f != NULL, "round-trip: reopen saved file");
        if (f) {
            while ((ch = fgetc(f)) != EOF) {
                if (ch == '\t') tab_at = count;
                if (ch == '\r') cr_at = count;
                count++;
            }
            fclose(f);
        }
        test_assert(count == 5004, "round-trip: exact byte count preserved");
        test_assert(tab_at == 5000 && cr_at == 5002,
                    "round-trip: control bytes preserved in place");
        remove(path);
    }

    /* Test 27: undo ends a run of cuts */
    {
        editor_reset();
        editor_insert_line(0, "Line1", 5);
        editor_insert_line(1, "Line2", 5);
        E.cy = 0; E.cx = 0;
        editor_cut_line();
        test_assert(E.cut_count == 1, "cut-run: first cut buffers one line");
        editor_undo();
        test_assert(!E.last_was_cut, "cut-run: undo ends the run");
        editor_cut_line();
        test_assert(E.cut_count == 1,
                    "cut-run: cut after undo starts a fresh buffer");
    }

    /* Test 28: cutting the sole empty line is a no-op */
    {
        editor_reset();
        editor_insert_line(0, "only", 4);
        editor_cut_line();
        test_assert(E.cut_count == 1, "empty-cut: real line was cut");
        editor_cut_line();
        editor_cut_line();
        test_assert(E.cut_count == 1,
                    "empty-cut: sole empty line does not grow the cut buffer");
        test_assert(E.num_lines == 1 && E.lines[0].len == 0,
                    "empty-cut: buffer still holds one empty line");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "only") == 0,
                    "empty-cut: no-op cuts pushed no undo entries");
    }

    /* Test 29: a long filename still saves atomically. The name is long
     * enough that "<name>.nano.0.tmp" does not fit in MAX_FILENAME (which
     * used to make the file unsaveable) but short enough that the temp file's
     * final path component stays under the host's NAME_MAX. */
    {
        char longname[MAX_FILENAME];
        int n;

        editor_reset();
        n = snprintf(longname, sizeof(longname), "/tmp/slow32-nano-");
        while (n < MAX_FILENAME - 11)
            longname[n++] = 'n';
        longname[n] = '\0';
        test_assert(n + 11 >= MAX_FILENAME,
                    "long-name: fixture overflows a MAX_FILENAME temp buffer");

        remove(longname);
        editor_insert_line(0, "long", 4);
        strncpy(E.filename, longname, MAX_FILENAME - 1);
        E.filename[MAX_FILENAME - 1] = '\0';
        test_assert(editor_save() == 0,
                    "long-name: temp name has room for the save suffix");
        remove(longname);
    }

    /* Test 30: unindent reverses tab indentation, not just spaces */
    {
        editor_reset();
        editor_insert_line(0, "\thello", 6);
        E.cy = 0; E.cx = 6;
        editor_unindent_line();
        test_assert(strcmp(E.lines[0].text, "hello") == 0,
                    "unindent-tab: leading tab removed");
        test_assert(E.cx == 5, "unindent-tab: cx follows the removal");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "\thello") == 0,
                    "unindent-tab: undo restores the tab");

        /* Mixed whitespace: stop after one indent level of visual columns. */
        editor_reset();
        editor_insert_line(0, "  \tx", 4);
        E.cy = 0; E.cx = 4;
        editor_unindent_line();
        test_assert(strcmp(E.lines[0].text, "x") == 0,
                    "unindent-tab: two spaces plus a tab is one level");

        /* A tab past the first stop only covers the rest of that stop. */
        editor_reset();
        editor_insert_line(0, "\t\ty", 3);
        E.cy = 0; E.cx = 3;
        editor_unindent_line();
        test_assert(strcmp(E.lines[0].text, "\ty") == 0,
                    "unindent-tab: only one tab removed per press");
    }

    /* Test 31: replace-all costs one undo entry pair per line, not per match */
    {
        int before, after;
        editor_reset();
        editor_insert_line(0, "foo foo foo foo", 15);
        before = E.undo_count;
        test_assert(editor_replace_line_all(0, 0, "foo", 3, "bar", 3) == 4,
                    "replace-line: all four occurrences replaced");
        after = E.undo_count;
        test_assert(strcmp(E.lines[0].text, "bar bar bar bar") == 0,
                    "replace-line: text fully rewritten");
        test_assert(after - before == 2,
                    "replace-line: one entry pair for the whole line");
        editor_undo();
        test_assert(strcmp(E.lines[0].text, "foo foo foo foo") == 0,
                    "replace-line: one undo reverses the whole line");
        editor_redo();
        test_assert(strcmp(E.lines[0].text, "bar bar bar bar") == 0,
                    "replace-line: one redo reapplies the whole line");
    }

    /* Test 32: replace-line honours from_x and length-changing replacements */
    {
        editor_reset();
        editor_insert_line(0, "aa aa aa", 8);
        test_assert(editor_replace_line_all(0, 3, "aa", 2, "b", 1) == 2,
                    "replace-line: matches before from_x are skipped");
        test_assert(strcmp(E.lines[0].text, "aa b b") == 0,
                    "replace-line: prefix preserved, tail shortened");

        editor_reset();
        editor_insert_line(0, "xx", 2);
        test_assert(editor_replace_line_all(0, 0, "x", 1, "", 0) == 2,
                    "replace-line: empty replacement deletes matches");
        test_assert(E.lines[0].len == 0,
                    "replace-line: line emptied without looping");

        editor_reset();
        editor_insert_line(0, "ab", 2);
        test_assert(editor_replace_line_all(0, 0, "zz", 2, "y", 1) == 0,
                    "replace-line: no match reports zero");
        test_assert(strcmp(E.lines[0].text, "ab") == 0,
                    "replace-line: no match leaves the line alone");
    }

    /* Test 33: every editing command marks the buffer modified. Guards the
     * removal of the direct E.modified writes — the flag is now derived
     * solely from the undo state. */
    {
        int i;
        for (i = 0; i < 12; i++) {
            editor_reset();
            editor_insert_line(0, "    alpha beta", 14);
            editor_insert_line(1, "gamma", 5);
            E.cy = 0; E.cx = 4;
            test_assert(!E.modified, "modified: clean before the edit");
            switch (i) {
            case 0:  editor_insert_char('z'); break;
            case 1:  editor_insert_newline(); break;
            case 2:  editor_insert_tab(); break;
            case 3:  editor_backspace(); break;
            case 4:  editor_delete(); break;
            case 5:  editor_cut_line(); break;
            case 6:  editor_cut_line(); editor_paste_line(); break;
            case 7:  editor_duplicate_line(); break;
            case 8:  editor_delete_word_forward(); break;
            case 9:  E.cx = 9; editor_delete_word_backward(); break;
            case 10: editor_indent_line(); break;
            case 11: editor_unindent_line(); break;
            }
            test_assert(E.modified, "modified: set after the edit");
        }
    }

    /* Test 34: a no-op command leaves the modified flag alone */
    {
        editor_reset();
        editor_insert_line(0, "clean", 5);
        E.cy = 0; E.cx = 0;
        editor_unindent_line();          /* nothing to unindent */
        test_assert(!E.modified, "modified: no-op unindent stays clean");
        editor_cut_line();               /* clears the sole line */
        editor_cut_line();               /* no-op: already empty */
        editor_undo();
        test_assert(!E.modified,
                    "modified: undo back to the load state is clean again");
    }

    /* Cleanup */
    editor_reset();
    cut_buf_clear();
    free(E.cut_lines);
    undo_clear();
    free(E.lines);

    printf("%d/%d tests passed\n", test_pass, test_count);
    return (test_pass == test_count) ? 0 : 1;
}

/* ---- Main ---- */

int main(int argc, char *argv[]) {
    int i;

    /* Check for --test flag */
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--test") == 0)
            return run_tests();
    }

    /* Initialize terminal */
    if (term_init() != 0) {
        printf("Error: terminal service not available\n");
        return 1;
    }

    /* Initialize editor state */
    memset(&E, 0, sizeof(E));
    E.next_state = 1;
    E.next_undo_group = 1;
    E.lines_cap = LINES_INIT_CAP;
    E.lines = malloc(E.lines_cap * sizeof(line_t));
    E.num_lines = 0;
    E.running = 1;

    term_get_size(&E.screen_rows, &E.screen_cols);
    if (E.screen_rows < 5) E.screen_rows = 24;
    if (E.screen_cols < 20) E.screen_cols = 80;
    E.edit_rows = E.screen_rows - 2; /* status bar + help/message bar */

    /* Load file or start empty */
    if (argc >= 2 && argv[1][0] != '-') {
        editor_load(argv[1]);
    } else {
        editor_insert_line(0, "", 0);
        editor_set_message("nano for SLOW-32  ^S=Save ^Q=Quit ^X=Save+Quit ^F=Find");
    }

    E.gutter_width = compute_gutter_width();
    E.prev_gutter_width = E.gutter_width;
    E.prev_num_lines = E.num_lines;
    E.prev_row_offset = -1;  /* force initial dirty_all */
    E.prev_col_offset = -1;

    /* Enter raw mode and clear screen */
    term_set_raw(1);
    term_clear(0);

    /* Main loop */
    while (E.running) {
        int key;
        editor_refresh_screen();
        key = read_key();
        if (key == -1) break; /* EOF */
        editor_process_key(key);
    }

    /* Cleanup */
    term_set_attr(0);
    term_set_raw(0);
    term_clear(0);
    term_gotoxy(1, 1);
    term_cleanup();

    /* Free memory */
    for (i = 0; i < E.num_lines; i++)
        line_free(&E.lines[i]);
    free(E.lines);
    cut_buf_clear();
    free(E.cut_lines);
    undo_clear();

    return 0;
}
