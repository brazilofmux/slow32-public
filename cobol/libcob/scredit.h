/* scredit.h -- the screen field editor's core (docs/plans/screen-input.md).
 *
 * A state machine with no terminal in it:
 *
 *     field description + editor state + key function -> new state, verdict
 *
 * compiled into libcob (libcob.c's screen ACCEPT reads keys, calls
 * se_key, paints se_image and puts the cursor at se_cursor) and, the same
 * source, into a host test program (tests/scredit_test.c), where its
 * behaviour is pinned by tables.
 *
 * The behaviour is the standard's where the standard speaks (2023 9.2,
 * 14.9.1) and Micro Focus's ADIS, in its default configuration, where it
 * does not: as its reference describes it and as Microsoft COBOL 5 shows
 * it key by key (docs/adis-observed.md).  Where this differs from ADIS
 * the comment says so.
 *
 * This file: text fields (alphanumeric, alphabetic, alphanumeric-edited).
 * Numeric fields are step 3 of the plan.
 *
 * Rules, as kern.h's: no globals, no calls outside this file but memcpy,
 * memmove and memset, nothing of libcob's. */
#ifndef SCREDIT_H
#define SCREDIT_H

#include <string.h>

#define SE_MAXW 512                     /* the widest field (libcob's own limit) */

/* What a key means.  One static table in libcob maps keys to these; the
 * names and the default keys are ADIS's (its "standard key functions"). */
enum {
    SE_CHAR = 1,        /* a data character (the code point is se_key's `ch`) */
    SE_LEFT, SE_RIGHT,  /* one position; at the field's edge the verdict says to leave */
    SE_END,             /* the end of the data; from there, the last field */
    SE_BACKSPACE,       /* undo the last character (replace mode), or delete to the left (insert mode) */
    SE_DELETE,          /* delete under the cursor, closing up */
    SE_INSERT_TOGGLE,   /* insert mode on or off */
    SE_CLEAR_FIELD,     /* Ctrl-X */
    SE_CLEAR_EOF,       /* Ctrl-Z: from the cursor to the end of the field */
    SE_UNDO,            /* Ctrl-A: the field as it was when the cursor entered it */
    SE_INSERT_SPACE,    /* Ctrl-O */
    SE_RESTORE_CHAR,    /* Ctrl-R: the last deleted character back, inserted */
    SE_CHANGE_CASE      /* Ctrl-F: the character under the cursor, and on */
};

/* What se_key answers. */
enum {
    SE_OK = 0,          /* done (the image may have changed: repaint) */
    SE_REFUSED,         /* the key means nothing here, or the picture refuses it: nothing changed */
    SE_GO_PREV,         /* Left at the first position: the previous field, at the end of its data */
    SE_GO_NEXT,         /* Right at the end of the data: the next field */
    SE_GO_LAST,         /* End at the end of the data: the last field */
    SE_FILLED           /* a character went into the last position: AUTO leaves the field on this */
};

enum { SE_F_SECURE = 1, SE_F_NOPROMPT = 2 };

/* A text field: its width, and for each column what the picture has
 * there -- 'X' any character, 'A' a letter or a space, '9' a digit -- or
 * 0 with the insertion character in `lit` (B a space, 0, /).  The data
 * positions are the columns that are not insertion characters; editing
 * is of the string of data positions, and the insertion characters stay
 * where the picture has them.
 *
 * (ADIS treats an alphanumeric-edited picture as X(n) and lets its
 * insertion characters be typed over.  The standard has the data
 * entered consistent with the PICTURE, 14.9.1.4 rule 20: here they are
 * protected and the cursor skips them.  A deliberate difference.) */
typedef struct {
    int width;
    unsigned flags;
    unsigned char prompt;               /* shown for the positions after the data ('_' is ADIS's) */
    char cls[SE_MAXW];                  /* 'X' 'A' '9', or 0: an insertion character */
    char lit[SE_MAXW];                  /* ... which one */
    int nd;                             /* data positions */
    short col[SE_MAXW];                 /* the column of each */
} se_field;

typedef struct {
    char d[SE_MAXW];                    /* the data positions' characters (spaces: empty) */
    char entry[SE_MAXW];                /* ... as they were when the cursor entered the field (Ctrl-A) */
    char restore[SE_MAXW]; int nrestore;/* overtyped and deleted characters, last first out */
    char overflow[SE_MAXW]; int noverflow; /* characters pushed off the end by an insert */
    int pos;                            /* the cursor: a data position, 0 .. nd-1 */
    int off_end;                        /* ... and off the end of the field on the last one (see se_key) */
    int end;                            /* the end of the data: positions keyed or filled, a keyed trailing space among them */
    int insert;                         /* insert mode */
    int touched;                        /* the operator changed something */
} se_state;

/* ---- building a field from a PICTURE ----------------------------------- */

/* The picture string as one symbol a column: X(3)A9 -> XXXA9.  Returns
 * the number of columns, or -1 when it does not fit or is not a text
 * picture's alphabet (X A 9 B 0 /). */
static int se_expand_picture(const char *pic, char *out, int cap)
{
    int n = 0;
    for (const char *p = pic; *p; p++) {
        char c = *p;
        if (c >= 'a' && c <= 'z') c = (char)(c - 'a' + 'A');
        if (c != 'X' && c != 'A' && c != '9' && c != 'B' && c != '0' && c != '/') return -1;
        int rep = 1;
        if (p[1] == '(') {
            rep = 0; p += 2;
            while (*p >= '0' && *p <= '9') rep = rep * 10 + (*p++ - '0');
            if (*p != ')') return -1;
        }
        while (rep-- > 0) { if (n >= cap) return -1; out[n++] = c; }
    }
    return n;
}

/* A field from its expanded picture (`mask`, one symbol a column; NULL
 * or empty: X in every column). */
static void se_field_init(se_field *f, const char *mask, int width, unsigned flags, int prompt)
{
    memset(f, 0, sizeof *f);
    if (width > SE_MAXW) width = SE_MAXW;
    f->width = width; f->flags = flags; f->prompt = (unsigned char)(prompt ? prompt : '_');
    int edited = 0;
    for (int i = 0; i < width; i++) {
        char c = mask && mask[0] ? mask[i] : 'X';
        if (c == 'B' || c == '0' || c == '/') { f->cls[i] = 0; f->lit[i] = c == 'B' ? ' ' : c; edited = 1; }
        else { f->cls[i] = c == 'A' || c == '9' ? c : 'X'; f->col[f->nd++] = (short)i; }
    }
    /* a picture of 9s and insertion characters only is numeric-edited,
     * not this file's; with X or A beside them the 9 is a digit position
     * of an alphanumeric(-edited) picture.  0 is an insertion character
     * only in an edited picture, which it makes one. */
    (void)edited;
}

/* ---- state ------------------------------------------------------------- */

static int se_trimmed(const se_field *f, const se_state *s)  /* past the last character that is not a space */
{
    int n = f->nd;
    while (n > 0 && s->d[n - 1] == ' ') n--;
    return n;
}
/* the data positions in use: past the last non-space, or as far as the
 * operator has keyed (a space keyed at the end is data, not prompt) */
static int se_len(const se_field *f, const se_state *s)
{
    int n = se_trimmed(f, s);
    return s->end > n ? (s->end < f->nd ? s->end : f->nd) : n;
}

/* The cursor arrives in the field, which holds `image` (width columns:
 * the item moved to the picture, or spaces): the data is taken from the
 * data positions, the entry state remembered, the buffers emptied.
 * at_end_of_data: arriving by Left from the next field. */
static void se_enter(const se_field *f, se_state *s, const char *image, int at_end_of_data)
{
    int insert = s->insert;
    memset(s, 0, sizeof *s);
    s->insert = insert;                 /* insert mode belongs to the ACCEPT, not the field */
    for (int k = 0; k < f->nd; k++) s->d[k] = image ? image[f->col[k]] : ' ';
    memcpy(s->entry, s->d, (size_t)f->nd);
    if (at_end_of_data) { int n = se_len(f, s); s->pos = n < f->nd ? n : f->nd - 1; }
}

/* Back in a field already entered (Tab away and back): the data stays,
 * the cursor and the undo state start again. */
static void se_reenter(const se_field *f, se_state *s, int at_end_of_data)
{
    memcpy(s->entry, s->d, (size_t)f->nd);
    s->nrestore = s->noverflow = 0; s->off_end = 0;
    s->pos = 0;
    if (at_end_of_data) { int n = se_len(f, s); s->pos = n < f->nd ? n : f->nd - 1; }
}

/* Home on a screen whose first field is this one: the cursor to its
 * first position, nothing else changed. */
static void se_reenter_keep(const se_field *f, se_state *s) { (void)f; s->pos = 0; s->off_end = 0; }

/* The field as it shows: width columns.  The positions after the data
 * show the prompt character while the field is the current one (`cur`),
 * spaces otherwise; SECURE shows an asterisk for each character (ADIS's
 * default shows nothing; the asterisk is its option, and ours). */
static void se_image(const se_field *f, const se_state *s, char *out, int cur)
{
    int n = se_len(f, s);
    for (int i = 0; i < f->width; i++) out[i] = f->cls[i] ? ' ' : f->lit[i];
    for (int k = 0; k < f->nd; k++) {
        char c = s->d[k];
        if (k >= n) c = cur && !(f->flags & SE_F_NOPROMPT) ? (char)f->prompt : ' ';
        else if (f->flags & SE_F_SECURE) c = c == ' ' ? ' ' : '*';
        out[f->col[k]] = c;
    }
}

/* The field's text for the item: the picture's insertion characters in
 * place, spaces where nothing was keyed. */
static void se_text(const se_field *f, const se_state *s, char *out)
{
    for (int i = 0; i < f->width; i++) out[i] = f->cls[i] ? ' ' : f->lit[i];
    for (int k = 0; k < f->nd; k++) out[f->col[k]] = s->d[k];
}

static int se_cursor(const se_field *f, const se_state *s) { return f->nd ? f->col[s->pos] : 0; }

/* REQUIRED: at least one character that is not a space (2023 13.18.47.4
 * rule 3).  FULL: all spaces, or the first and the last position both
 * filled (13.18.26.4 rule 3). */
static int se_required_ok(const se_field *f, const se_state *s) { return se_len(f, s) > 0; }
static int se_full_ok(const se_field *f, const se_state *s)
{
    int n = se_len(f, s);
    return n == 0 || (f->nd > 0 && s->d[0] != ' ' && s->d[f->nd - 1] != ' ');
}

/* ---- keys -------------------------------------------------------------- */

static int se_class_ok(char cls, int ch)
{
    if (ch < 32 || ch > 126) return 0;                  /* a text field's characters are one byte each */
    if (cls == 'A') return ch == ' ' || (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
    if (cls == '9') return ch >= '0' && ch <= '9';
    return 1;
}

static void se_push(char *stk, int *n, char c) { if (*n < SE_MAXW) stk[(*n)++] = c; }

/* the data positions from `from` on move one to the right; what falls
 * off the end goes to the overflow buffer */
static void se_shift_right(const se_field *f, se_state *s, int from)
{
    /* when the data reaches the last position (a keyed space is data) */
    if (se_len(f, s) == f->nd) se_push(s->overflow, &s->noverflow, s->d[f->nd - 1]);
    memmove(s->d + from + 1, s->d + from, (size_t)(f->nd - 1 - from));
}

/* ... and one to the left over position `at`; the end is filled from the
 * overflow buffer (returns 1), or with a space */
static int se_shift_left(const se_field *f, se_state *s, int at)
{
    int had = s->noverflow > 0;
    memmove(s->d + at, s->d + at + 1, (size_t)(f->nd - 1 - at));
    s->d[f->nd - 1] = had ? s->overflow[--s->noverflow] : ' ';
    return had;
}

/* Is the whole string still what its positions' classes allow, after a
 * shift moved characters under other columns?  (X(4)9(2): inserting must
 * not push a letter into a digit position.) */
static int se_classes_hold(const se_field *f, const se_state *s)
{
    for (int k = 0; k < f->nd; k++)
        if (s->d[k] != ' ' && !se_class_ok(f->cls[f->col[k]], (unsigned char)s->d[k])) return 0;
    return 1;
}

/* The cursor is a data position, 0 .. nd-1.  When the last position is
 * typed into the cursor cannot advance and stays there, "off the end"
 * (ADIS's default end-of-field behaviour): a character keyed next
 * overtypes the last one, and Backspace takes it back without moving
 * first.  Right on the last position of a full field goes off the end
 * too; Left comes back from it. */
static int se_key1(const se_field *f, se_state *s, int fn, int ch);
static int se_key(const se_field *f, se_state *s, int fn, int ch)
{
    int v = se_key1(f, s, fn, ch);
    /* off the end only while the last position holds data */
    if (s->off_end && se_len(f, s) < f->nd) s->off_end = 0;
    return v;
}

static int se_key1(const se_field *f, se_state *s, int fn, int ch)
{
    if (!f->nd) return SE_REFUSED;
    int last = f->nd - 1, n = se_len(f, s);
    int off = s->off_end;
    s->off_end = 0;
    switch (fn) {
    case SE_CHAR: {
        if (!se_class_ok(f->cls[f->col[s->pos]], ch)) { s->off_end = off; return SE_REFUSED; }
        if (s->insert) {
            se_state t = *s;
            se_shift_right(f, s, s->pos);
            s->d[s->pos] = (char)ch;
            if (!se_classes_hold(f, s)) { *s = t; s->off_end = off; return SE_REFUSED; }
            s->end = n < f->nd ? n + 1 : f->nd;
        } else {
            se_push(s->restore, &s->nrestore, s->d[s->pos]);
            s->d[s->pos] = (char)ch;
        }
        if (s->end < s->pos + 1) s->end = s->pos + 1;
        s->touched = 1;
        if (s->pos < last) { s->pos++; return SE_OK; }
        s->off_end = 1;
        return SE_FILLED;
    }
    case SE_LEFT:
        if (s->pos == 0) return SE_GO_PREV;
        s->pos--;
        return SE_OK;
    case SE_RIGHT:
        /* the cursor does not go past the end of the data: the next field */
        if (off || s->pos >= n) { s->off_end = off; return SE_GO_NEXT; }
        if (s->pos == last) { s->off_end = 1; return SE_OK; }
        s->pos++;
        return SE_OK;
    case SE_END:
        if (off || s->pos >= n) { s->off_end = off; return SE_GO_LAST; }
        s->pos = n > last ? last : n;
        return SE_OK;
    case SE_BACKSPACE:
        if (!off) {
            if (s->pos == 0) return SE_REFUSED;
            s->pos--;
        }
        if (s->insert) { if (!se_shift_left(f, s, s->pos)) s->end = n > 0 ? n - 1 : 0; }
        else {
            s->d[s->pos] = s->nrestore ? s->restore[--s->nrestore] : ' ';
            if (s->pos + 1 >= n) s->end = s->pos;           /* the last character taken back */
        }
        if (!se_classes_hold(f, s)) s->d[s->pos] = ' ';
        s->touched = 1;
        return SE_OK;
    case SE_DELETE: {
        if (s->pos >= n) return SE_REFUSED;                 /* past the data: nothing to delete */
        se_state t = *s;
        se_push(s->restore, &s->nrestore, s->d[s->pos]);
        int refilled = se_shift_left(f, s, s->pos);
        if (!se_classes_hold(f, s)) { *s = t; return SE_REFUSED; }
        if (!refilled) s->end = n - 1;
        s->touched = 1;
        s->off_end = off;                                   /* the cursor has not moved */
        return SE_OK;
    }
    case SE_INSERT_TOGGLE:
        s->insert = !s->insert;                             /* and the cursor is no longer off the end */
        return SE_OK;
    case SE_CLEAR_FIELD:
        /* the data goes to the restore buffer, first character first */
        for (int k = 0; k < n; k++) se_push(s->restore, &s->nrestore, s->d[k]);
        memset(s->d, ' ', (size_t)f->nd);
        s->pos = 0; s->end = 0; s->noverflow = 0; s->touched = 1;
        return SE_OK;
    case SE_CLEAR_EOF:
        for (int k = s->pos; k < n; k++) se_push(s->restore, &s->nrestore, s->d[k]);
        memset(s->d + s->pos, ' ', (size_t)(f->nd - s->pos));
        s->end = s->pos; s->noverflow = 0; s->touched = 1;
        s->off_end = off;
        return SE_OK;
    case SE_UNDO:
        /* the field as it was on entry; the restore buffer is kept, the
         * overflow buffer emptied (as the clears empty it) */
        memcpy(s->d, s->entry, (size_t)f->nd);
        s->pos = 0; s->end = 0; s->noverflow = 0;
        return SE_OK;
    case SE_INSERT_SPACE: {
        /* a character pushed off the end goes to the overflow buffer
         * (ADIS signals an error and does it all the same) */
        se_state t = *s;
        se_shift_right(f, s, s->pos);
        s->d[s->pos] = ' ';
        if (!se_classes_hold(f, s)) { *s = t; return SE_REFUSED; }
        s->end = n < f->nd ? n + 1 : f->nd;
        if (s->end < s->pos + 1) s->end = s->pos + 1;
        s->touched = 1;
        s->off_end = off;                                   /* the cursor has not moved */
        return SE_OK;
    }
    case SE_RESTORE_CHAR: {
        if (!s->nrestore) return SE_REFUSED;
        se_state t = *s;
        char c = s->restore[s->nrestore - 1];
        se_shift_right(f, s, s->pos);
        s->d[s->pos] = c;
        if (!se_classes_hold(f, s)) { *s = t; return SE_REFUSED; }
        s->nrestore--;
        s->end = n < f->nd ? n + 1 : f->nd;
        if (s->end < s->pos + 1) s->end = s->pos + 1;
        s->touched = 1;
        s->off_end = off;                                   /* the cursor has not moved */
        return SE_OK;
    }
    case SE_CHANGE_CASE: {
        /* the character under the cursor keyed again, a letter in its
         * other case: as a keyed character is, in replace mode */
        if (s->pos >= n) return SE_REFUSED;
        char c = s->d[s->pos];
        if (c >= 'a' && c <= 'z') c = (char)(c - 'a' + 'A');
        else if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
        se_push(s->restore, &s->nrestore, s->d[s->pos]);
        s->d[s->pos] = c; s->touched = 1;
        if (s->pos < last) { s->pos++; return SE_OK; }
        s->off_end = 1;
        return SE_FILLED;
    }
    }
    return SE_REFUSED;
}

#endif
