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

#ifndef SCREDIT_EXPAND_ONLY             /* the compiler takes se_expand_picture alone */

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

/* ======================================================================
 * Numeric and numeric-edited fields.
 *
 * Such a field is edited as digits standing in the picture's digit
 * positions: the state is the integer digits, the fraction digits, a
 * sign, and where the shown digits begin; the image is those digits put
 * through the picture by the ordinary editing code after every key (the
 * caller does that: this file calls nothing); the cursor is on a digit
 * position or on the point.
 *
 * Two styles of entry.  The fixed-position one is the adding machine's,
 * and is what a 1993 Micro Focus runtime was observed to do, key by key,
 * from the outside (docs/adis-observed.md; tests/scredit-differential.sh
 * -N compares): the cursor starts on the first position that shows and
 * digits overtype from there, so 5 Enter in ZZZ99.99 is 50.00; only the
 * point key aligns; each side of an assumed point is a field of its own.
 * It taught what a picture does to a state machine, and it is kept, and
 * tested, as that.
 *
 * Natural entry (sn_natural) is what the runtime uses: a number is keyed
 * as it is written.  Digits enter at the point and push left, the point
 * key goes to the fraction, an assumed point is a point, and a full
 * integer part carries the cursor into the fraction.  The cursor keys
 * still reach every digit position, and a digit typed there overtypes.
 * ====================================================================== */

#define SN_MAXD 40
#define SN_MAXW 80

typedef struct {
    int width;                          /* columns */
    char pat[SN_MAXW + 1];              /* the flattened picture (picture.h) */
    int ni, nf;                         /* integer and fraction digit positions */
    short icol[SN_MAXD], fcol[SN_MAXD]; /* their columns */
    int pcol;                           /* the point's column, -1 when it has none (V, or no fraction) */
    int first9;                         /* the first integer position that is never suppressed (ni: none) */
    int has_sign;                       /* the picture can show a sign (an edited one) */
    int edited;
    int natural;                        /* keyed as a number is written: see sn_natural */
    char kind[SN_MAXW];                 /* each column: 'i' 'f' digit positions, '.' the point, ',' an insertion, 'F' the floating string's first, 's' a fixed sign or currency */
} sn_field;

typedef struct {
    char id[SN_MAXD], fd[SN_MAXD];      /* the digits, '0'..'9' */
    int neg;
    int lead;                           /* the first integer position that shows: digits from here on are data, zeros too */
    int frac;                           /* the cursor is in the fraction */
    int pos;                            /* integer: 0..ni (ni: on the point); fraction: 0..nf-1 */
    int off_end;                        /* on the last digit of its part, just typed: the next digit overtypes it */
    char e_id[SN_MAXD], e_fd[SN_MAXD]; int e_neg, e_lead;
    int touched;
} sn_state;

enum { SN_DIGIT = 1, SN_POINT, SN_MINUS, SN_PLUS, SN_LEFT, SN_RIGHT, SN_END, SN_BACKSPACE, SN_DELETE,
       SN_CLEAR_FIELD, SN_CLEAR_EOF, SN_UNDO };
enum { SN_OK = 0, SN_REFUSED, SN_GO_PREV, SN_GO_NEXT, SN_GO_LAST, SN_FILLED };

/* The field from its flattened picture: `floating` is the picture's
 * floating symbol or 0 (PicInfo.floating). */
static int sn_field_init(sn_field *f, const char *pat, int floating, int edited)
{
    memset(f, 0, sizeof *f);
    f->pcol = -1; f->edited = edited;
    int c = 0, after = 0, seen_float = 0, nfl = 0;
    for (const char *p = pat; *p; p++) if (floating && *p == floating) nfl++;
    if (nfl < 2) floating = 0;                          /* one + - $ is a fixed insertion */
    int first9 = -1;
    for (const char *p = pat; *p; p++) {
        char s = *p;
        int digit = 0, fixed9 = 0;
        if (c + 2 > SN_MAXW) return -1;
        if (s == '9') { digit = 1; fixed9 = 1; }
        else if (s == 'Z' || s == '*') digit = 1;
        else if (floating && s == floating) { if (seen_float) digit = 1; seen_float = 1; }
        if (s == 'V') { after = 1; continue; }
        if (s == 'S') continue;
        if (s == 'P') return -1;                        /* scaling positions: not edited here */
        if (s == '.') { f->pcol = c; after = 1; f->kind[c] = '.'; c++; continue; }
        if (digit) {
            if (!after) { if (f->ni >= SN_MAXD) return -1; if (fixed9 && first9 < 0) first9 = f->ni; f->icol[f->ni++] = (short)c; f->kind[c] = 'i'; }
            else { if (f->nf >= SN_MAXD) return -1; f->fcol[f->nf++] = (short)c; f->kind[c] = 'f'; }
            c++; continue;
        }
        if (s == '+' || s == '-') f->has_sign = 1;
        if (s == 'C' || s == 'D') { f->has_sign = 1; f->kind[c] = f->kind[c + 1] = 's'; c += 2; continue; }
        f->kind[c] = (floating && s == floating) ? 'F' : (s == ',' || s == 'B' || s == '0' || s == '/') ? ',' : 's';
        c++;
    }
    f->first9 = first9 < 0 ? f->ni : first9;
    f->width = c;
    strncpy(f->pat, pat, SN_MAXW);
    return 0;
}

static int sn_firstsig(const sn_field *f, const sn_state *s)
{
    int k = 0;
    while (k < f->ni && s->id[k] == '0') k++;
    return k;
}
/* A picture with a point to stand on, or with suppressed positions and no
 * point (ZZ9: the point is after the last digit), takes integer digits
 * at the point, pushing left.  One with neither (9(5), 9(3)V99: ADIS
 * takes the two sides of a V as two fields side by side) is typed over
 * left to right, the cursor staying on its last digit. */
static int sn_has_point(const sn_field *f) { return f->pcol >= 0 || f->first9 > 0; }

/* Natural entry, which is what the runtime uses: the number is keyed the
 * way it is written.  Every integer position takes digits at the point,
 * whatever the picture suppresses, so 5 is 5 and not 50 in ZZZ99.99 or
 * 50000 in 9(5); the point key goes to the fraction; an assumed point
 * (9(3)V99) is a point like any other, and the integer part filling
 * carries the cursor over it.  The fixed positions of the adding-machine
 * style above (the observed behaviour of the 1993 runtime) are still
 * there for whoever moves the cursor onto a digit: it is overtyped. */
static void sn_natural(sn_field *f) { f->natural = 1; f->first9 = f->ni; }
static int sn_point(const sn_field *f) { return f->pcol >= 0 || f->natural; }

static void sn_snapshot(sn_state *s)
{
    memcpy(s->e_id, s->id, SN_MAXD); memcpy(s->e_fd, s->fd, SN_MAXD); s->e_neg = s->neg; s->e_lead = s->lead;
}

static void sn_home(const sn_field *f, sn_state *s)
{
    s->frac = 0; s->off_end = 0;
    s->pos = s->lead;
    if (s->pos >= f->ni && !sn_has_point(f)) s->pos = f->ni ? f->ni - 1 : 0;
}

/* digits: the item's value as ni+nf digits (leading zeros), neg its sign */
static void sn_enter(const sn_field *f, sn_state *s, const char *digits, int neg)
{
    memset(s, 0, sizeof *s);
    memcpy(s->id, digits, (size_t)f->ni); memcpy(s->fd, digits + f->ni, (size_t)f->nf);
    s->neg = neg;
    s->lead = sn_firstsig(f, s);
    if (s->lead > f->first9) s->lead = f->first9;
    sn_snapshot(s);
    sn_home(f, s);
}

static void sn_digits(const sn_field *f, const sn_state *s, char *out)   /* ni+nf digits for the editing code */
{
    memcpy(out, s->id, (size_t)f->ni); memcpy(out + f->ni, s->fd, (size_t)f->nf);
}

static int sn_cursor(const sn_field *f, const sn_state *s)
{
    if (s->frac) return f->fcol[s->pos];
    if (s->pos < f->ni) return f->icol[s->pos];
    if (f->pcol >= 0) return f->pcol;
    return f->ni ? f->icol[f->ni - 1] : 0;
}

/* A run of n digit positions with no suppression and no point to stand
 * on -- 9(5), each side of 9(3)V99, 99/99/99 -- typed over left to
 * right like text: the cursor stays on the last digit once it is typed
 * (*off: off the end), the point key right-justifies what stands left of
 * the cursor, Delete closes up from the left.  Returns SN_GO_PREV /
 * SN_GO_NEXT when a move leaves the run, -1 for a key that is not its
 * own to answer (sign, undo). */
static int sn_plain(char *d, int n, int *pos, int *off, int fn, int ch)
{
    int was = *off;
    *off = 0;
    switch (fn) {
    case SN_DIGIT:
        d[*pos] = (char)ch;
        if (*pos < n - 1) { (*pos)++; return SN_OK; }
        *off = 1;
        return SN_FILLED;
    case SN_POINT: {
        char t[SN_MAXD]; int k = *pos + (was ? 1 : 0);
        memcpy(t, d, (size_t)k);
        memset(d, '0', (size_t)n);
        memcpy(d + n - k, t, (size_t)k);
        *pos = n - 1; *off = 1;
        return SN_OK;
    }
    case SN_LEFT:
        if (*pos > 0) { (*pos)--; return SN_OK; }
        return SN_GO_PREV;
    case SN_RIGHT:
        if (*pos < n - 1) { (*pos)++; return SN_OK; }
        if (!was) { *off = 1; return SN_OK; }
        *off = 1;
        return SN_GO_NEXT;
    case SN_END:
        if (*pos >= n - 1) { *off = was; return SN_GO_LAST; }
        *pos = n - 1;
        return SN_OK;
    case SN_BACKSPACE:
        if (was) { d[*pos] = '0'; return SN_OK; }
        if (*pos > 0) { (*pos)--; d[*pos] = '0'; return SN_OK; }
        return SN_GO_PREV;
    case SN_DELETE:
        memmove(d + 1, d, (size_t)*pos); d[0] = '0';
        if (*pos < n - 1) (*pos)++;
        return SN_OK;
    case SN_CLEAR_FIELD:
        memset(d, '0', (size_t)n); *pos = 0;
        return SN_OK;
    case SN_CLEAR_EOF:
        memset(d + *pos, '0', (size_t)(n - *pos));
        return SN_OK;
    }
    *off = was;
    return -1;
}

static int sn_key1(const sn_field *f, sn_state *s, int fn, int ch);
static int sn_key(const sn_field *f, sn_state *s, int fn, int ch)
{
    int v;
    /* natural entry: a full integer field takes no more digits at its end */
    if (f->natural && fn == SN_DIGIT && !s->frac && f->nf == 0 && f->ni > 0 && s->lead == 0 && s->off_end && s->pos == f->ni - 1) return SN_REFUSED;
    /* the same picture, the cursor past its digits or on the last of
     * them: "off the end" is set by the point key, and by Right and End
     * when the cursor is already past the digits; it stays through the
     * digits typed after it, and the next Backspace past the digits is
     * taken up undoing it */
    if (!s->frac && f->pcol < 0 && f->first9 > 0 && f->nf == 0 && s->pos >= f->ni - 1) {
        int off = s->off_end, at = s->pos;
        if (off && at == f->ni && fn == SN_BACKSPACE) { s->off_end = 0; return SN_OK; }
        v = sn_key1(f, s, fn, ch);
        if (s->pos == f->ni) {
            if (at == f->ni && (fn == SN_RIGHT || fn == SN_END)) s->off_end = 1;
            else if (off && (fn == SN_DIGIT || (at == f->ni && (fn == SN_DELETE || fn == SN_CLEAR_EOF)))) s->off_end = 1;
        }
    } else
        v = sn_key1(f, s, fn, ch);
    /* a picture with no point to stand on (ZZ9), full: the cursor is on
     * its last digit, off the end, not past it */
    if (!s->frac && f->pcol < 0 && f->first9 > 0 && s->pos == f->ni && s->lead == 0 && f->ni > 0) {
        s->pos = f->ni - 1; s->off_end = 1;
        if (fn == SN_DIGIT && v == SN_OK) v = SN_FILLED;      /* its last digit taken */
    }
    return v;
}

static int sn_key1(const sn_field *f, sn_state *s, int fn, int ch)
{
    int ni = f->ni, nf = f->nf;
    /* the plain runs: an integer part with no point and no suppression,
     * and each side of a V */
    if (fn != SN_MINUS && fn != SN_PLUS && fn != SN_UNDO) {
        if (s->frac && !sn_point(f)) {
            int v = sn_plain(s->fd, nf, &s->pos, &s->off_end, fn, ch);
            if (v == SN_GO_PREV && ni > 0 && fn == SN_LEFT) {   /* back into the integer side, a field of its own (Backspace stays) */
                sn_snapshot(s);
                s->frac = 0; s->pos = ni - 1; s->off_end = 0;
                return SN_OK;
            }
            if (v >= 0) { if (v == SN_OK || v == SN_FILLED) s->touched = 1; return fn == SN_BACKSPACE && v == SN_GO_PREV ? SN_REFUSED : v; }
        } else if (!s->frac && !sn_has_point(f) && ni > 0) {
            int was_pos = s->pos, was_off = s->off_end;
            int v = sn_plain(s->id, ni, &s->pos, &s->off_end, fn, ch);
            if (fn == SN_CLEAR_FIELD) { s->neg = 0; if (nf > 0 && f->pcol >= 0) memset(s->fd, '0', (size_t)nf); }
            (void)was_off;
            if (nf > 0 && (v == SN_GO_NEXT || fn == SN_POINT || (fn == SN_RIGHT && was_pos == ni - 1))) {
                /* on into the fraction side */
                sn_snapshot(s);
                s->frac = 1; s->pos = 0; s->off_end = 0;
                return SN_OK;
            }
            if (v >= 0) { if (v == SN_OK || v == SN_FILLED) s->touched = 1; return fn == SN_BACKSPACE && v == SN_GO_PREV ? SN_REFUSED : v; }
        }
    }
    int off = s->off_end;
    s->off_end = 0;
    switch (fn) {
    case SN_DIGIT:
        if (s->frac) {
            s->fd[s->pos] = (char)ch; s->touched = 1;
            if (s->pos < nf - 1) { s->pos++; return SN_OK; }
            s->off_end = 1;
            return SN_FILLED;
        }
        if (s->pos < ni) {
            s->id[s->pos] = (char)ch; s->touched = 1;
            if (s->pos < s->lead) s->lead = s->pos;
            if (!sn_has_point(f)) {
                if (s->pos < ni - 1) { s->pos++; return SN_OK; }
                s->off_end = 1;
                return SN_FILLED;
            }
            s->pos++;
            /* past the last integer position: on the point, or into the
             * fraction when no more integer digits can be taken */
            if (s->pos == ni && s->lead == 0 && sn_point(f) && nf > 0) { s->frac = 1; s->pos = 0; }
            return SN_OK;
        }
        /* on the point: the digit goes in before it, the others move left */
        if (s->lead > 0) {
            memmove(s->id, s->id + 1, (size_t)(ni - 1));
            s->id[ni - 1] = (char)ch; s->lead--; s->touched = 1;
            if (s->lead == 0 && sn_point(f) && nf > 0) { s->frac = 1; s->pos = 0; }
            return SN_OK;
        }
        /* the integer part is full: with a point to stand on the digit
         * is refused; with none (ZZ9) it overtypes the last digit */
        if (!sn_point(f) && ni > 0) { s->id[ni - 1] = (char)ch; s->touched = 1; return SN_FILLED; }
        if (nf == 0 && ni > 0) { s->id[ni - 1] = (char)ch; s->touched = 1; return SN_FILLED; }
        return SN_REFUSED;
    case SN_POINT:
        if (s->frac) { s->off_end = off; return SN_REFUSED; }
        if (s->pos < ni) {
            /* align: the digits left of the cursor, right-justified */
            char t[SN_MAXD]; int n = s->pos + (off ? 1 : 0) - s->lead;   /* off the end: the last digit too */
            if (n < 0) n = 0;
            memcpy(t, s->id + s->lead, (size_t)n);
            memset(s->id, '0', (size_t)ni);
            memcpy(s->id + ni - n, t, (size_t)n);
            s->lead = ni - n < f->first9 ? ni - n : f->first9;
            s->touched = 1;
        }
        if (nf > 0) { if (!sn_point(f)) sn_snapshot(s); s->frac = 1; s->pos = 0; }
        else { s->pos = ni - 1; s->off_end = 1; }           /* no fraction: onto the last digit, off the end */
        return SN_OK;
    case SN_MINUS: case SN_PLUS:
        if (!f->has_sign) { s->off_end = off; return SN_REFUSED; }
        s->neg = fn == SN_MINUS; s->touched = 1; s->off_end = off;
        return SN_OK;
    case SN_LEFT:
        if (s->frac) {
            if (s->pos > 0) { s->pos--; return SN_OK; }
            if (ni == 0) return SN_GO_PREV;
            if (!sn_point(f)) sn_snapshot(s);
            s->frac = 0; s->pos = sn_point(f) ? ni : ni - 1;
            return SN_OK;
        }
        if (s->pos > s->lead) { s->pos--; return SN_OK; }
        return SN_GO_PREV;
    case SN_RIGHT:
        if (s->frac) {
            if (s->pos < nf - 1) { s->pos++; return SN_OK; }
            s->off_end = 1;                                 /* off the end of the fraction first */
            return off ? SN_GO_NEXT : SN_OK;
        }
        if (s->pos < ni - 1 || (s->pos == ni - 1 && sn_has_point(f))) { s->pos++; return SN_OK; }
        if (nf > 0) { if (!sn_point(f)) sn_snapshot(s); s->frac = 1; s->pos = 0; return SN_OK; }
        s->off_end = off;
        return SN_GO_NEXT;
    case SN_END:
        if (s->frac || (nf > 0 && sn_point(f))) {
            if (s->frac && s->pos == nf - 1) return SN_GO_LAST;
            s->frac = 1; s->pos = nf - 1;
            return SN_OK;
        }
        if (s->pos >= ni - 1) { s->off_end = off; return SN_GO_LAST; }
        s->pos = ni - 1;
        return SN_OK;
    case SN_BACKSPACE:
        if (s->frac) {
            if (off) { s->fd[s->pos] = '0'; s->touched = 1; return SN_OK; }
            if (s->pos > 0) { s->pos--; s->fd[s->pos] = '0'; s->touched = 1; return SN_OK; }
            if (ni == 0) return SN_REFUSED;
            if (!sn_point(f)) sn_snapshot(s);
            s->frac = 0; s->pos = sn_point(f) ? ni : ni - 1;
            return SN_OK;
        }
        if (off && s->pos < ni) {
            /* off the end of a full integer part with no point: the last
             * digit goes, the others move right, the cursor is past them */
            memmove(s->id + 1, s->id, (size_t)(ni - 1)); s->id[0] = '0';
            if (s->lead < f->first9) s->lead++;
            s->pos = ni; s->touched = 1;
            return SN_OK;
        }
        if (s->pos == ni && ni > 0) {
            if (s->lead < f->first9) {                  /* digits have grown into the suppressed positions */
                memmove(s->id + 1, s->id, (size_t)(ni - 1)); s->id[0] = '0';
                s->lead++; s->touched = 1;
                return SN_OK;
            }
            if (ni - 1 < s->lead) return SN_REFUSED;
            s->pos = ni - 1; s->id[s->pos] = '0'; s->touched = 1;
            return SN_OK;
        }
        if (s->pos > s->lead) { s->pos--; s->id[s->pos] = '0'; s->touched = 1; return SN_OK; }
        return SN_REFUSED;
    case SN_DELETE:
        if (s->frac) {
            memmove(s->fd + s->pos, s->fd + s->pos + 1, (size_t)(nf - 1 - s->pos));
            s->fd[nf - 1] = '0'; s->touched = 1;
            return SN_OK;
        }
        if (s->pos >= ni) return SN_REFUSED;
        memmove(s->id + 1, s->id, (size_t)s->pos); s->id[0] = '0';
        if (s->lead < f->first9) s->lead++;
        s->touched = 1;
        s->pos++;
        if (f->pcol < 0 && !(f->natural && nf > 0) && s->pos >= ni) s->pos = ni - 1;   /* no point to move onto: the last digit */
        return SN_OK;
    case SN_CLEAR_FIELD:
        memset(s->id, '0', (size_t)ni); memset(s->fd, '0', (size_t)nf);
        s->neg = 0; s->lead = f->first9; s->touched = 1;
        sn_home(f, s);
        return SN_OK;
    case SN_CLEAR_EOF:
        if (s->frac) memset(s->fd + s->pos, '0', (size_t)(nf - s->pos));
        else {
            if (s->pos < ni) memset(s->id + s->pos, '0', (size_t)(ni - s->pos));
            if (sn_point(f)) memset(s->fd, '0', (size_t)nf);
        }
        s->touched = 1;
        return SN_OK;
    case SN_UNDO:
        memcpy(s->id, s->e_id, SN_MAXD); memcpy(s->fd, s->e_fd, SN_MAXD); s->neg = s->e_neg; s->lead = s->e_lead;
        if (s->frac && !sn_point(f)) { s->pos = 0; return SN_OK; }     /* the fraction of a V picture is a field of its own */
        sn_home(f, s);
        return SN_OK;
    }
    return SN_REFUSED;
}

#endif                                  /* SCREDIT_EXPAND_ONLY */

#endif
