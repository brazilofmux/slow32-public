/* f77_card.h -- fixed-form Fortran 77 card-image reader.
 *
 * Assembles statements; the token layer is Ragel (f77_lexer.rl).
 *
 * Fixed-form is not C-like, and three of its rules drive this design:
 *
 *   1. A *statement* -- not a line -- is the unit.  Columns 1-5 hold an
 *      optional label, column 6 a continuation marker, columns 7-72 the
 *      text, and 73-80 are ignored sequence numbers.  A statement is an
 *      initial line plus any continuation lines, concatenated.  So the
 *      reader assembles a whole statement before a single token is cut.
 *
 *   2. Blanks are insignificant outside character context.  `DO 10 I`,
 *      `DO10I` and `D O 1 0 I` are the same text.  Blanks are therefore
 *      squeezed during assembly, not skipped during tokenization --
 *      otherwise keywords with embedded blanks would never be
 *      recognised.
 *
 *   3. There are no reserved words.  `IF`, `DO` and `FORMAT` are all
 *      legal variable names, so the lexer must NOT classify keywords;
 *      it returns names and lets the parser decide from context.  This
 *      is why `DO 10 I = 1, 10` (a loop) and `DO10I = 1.10` (an
 *      assignment to the variable DO10I) can only be told apart by
 *      scanning ahead for the comma -- a parser job, not a lexer one.
 *
 * Two constructs survive blank-squeezing verbatim: character constants
 * ('...', with '' as an embedded quote) and Hollerith counts (nH
 * followed by exactly n characters, blanks included).  Both are
 * detected during assembly, when the character positions still mean
 * something.
 *
 * Extensions accepted, deliberately: tab-formatted source (a tab in
 * columns 1-6 jumps to the statement body; a digit right after it marks
 * a continuation), lower case, and `!` as an end-of-statement comment.
 * Real F77 decks in the wild use all three.
 */
#ifndef F77_CARD_H
#define F77_CARD_H

#define F77_MAX_STMT 8192
#define F77_MAX_NAME 64

/* --- source state --- */
static char *lx_src;
static int   lx_len;
static int   lx_pos;        /* byte offset of the next line to read */
static int   lx_line;       /* 1-based line number of that line */

/* --- assembled statement --- */
static char  lx_stmt[F77_MAX_STMT];
static int   lx_stmt_len;
static int   lx_stmt_label; /* -1 when the statement carries no label */
static int   lx_stmt_line;  /* line number of the initial line */
/* lx_brk[i] is 1 iff a blank was squeezed out immediately before
 * lx_stmt[i] -- i.e. i starts a space-delimited token in the original
 * card.  Leading blanks (column-7 indent) do not set this; only a
 * blank between two copied characters does.  The FUNCTION-header
 * classifier uses it so INTEGER FUNCTIONAL stays one name while
 * INTEGER FUNCTION F and the no-space INTEGERFUNCTIONF(X) both still
 * open a FUNCTION unit (GitHub issue 25). */
static unsigned char lx_brk[F77_MAX_STMT];
static int lx_after_blank;

static void f77_error(char *msg);    /* supplied by the driver */

/* --- character helpers ---------------------------------------------- */

static int lx_isdigit(int c) { return c >= '0' && c <= '9'; }
static int lx_isalpha(int c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z') ||
           c == '_' || c == '$';
}
static int lx_upper(int c) {
    if (c >= 'a' && c <= 'z') return c - 'a' + 'A';
    return c;
}

/* --- INCLUDE (MIL-STD-1753 2.3) -------------------------------------
 *
 * INCLUDE 'name', INCLUDE (name) or INCLUDE name, alone on its line,
 * copies the named text into the source in its place.  It is done when
 * the source is loaded (lx_expand): the INCLUDE line becomes a comment
 * and the text follows it, so everything after sees one source.  The
 * text is a file beside the source, then in the current directory, as
 * written or in lower case, with or without .f, .for or .inc.  An
 * included text may INCLUDE another, eight deep, but not one it is
 * being read from.  Messages name an included text's own lines
 * (lx_where): lx_seg says where in the source each piece begins. */
#define LX_INC_DEPTH 8
#define LX_MAX_SEG   512
static int   lx_seg_x[LX_MAX_SEG];     /* the source's line where a piece begins */
static int   lx_seg_o[LX_MAX_SEG];     /* its first line in its file */
static int   lx_seg_f[LX_MAX_SEG];     /* its file: an index into lx_seg_nm, -1 the source */
static char  lx_seg_nm[64][64];
static int   lx_seg_nnm;
static int   lx_nseg;
static char  lx_inc_nm[LX_INC_DEPTH][64];   /* the texts being read */
static int   lx_inc_n;
static int   lx_xline;                       /* lines of the expanded source so far */
static const char *lx_inname;

static int lx_seg_file(char *nm) {
    int j;
    for (j = 0; j < lx_seg_nnm; j++) if (strcmp(lx_seg_nm[j], nm) == 0) return j;
    if (lx_seg_nnm >= 64) return 63;
    strcpy(lx_seg_nm[j], nm);
    lx_seg_nnm = lx_seg_nnm + 1;
    return j;
}

static void lx_seg_note(int file, int oline) {
    if (lx_nseg >= LX_MAX_SEG) return;
    lx_seg_x[lx_nseg] = lx_xline + 1;
    lx_seg_o[lx_nseg] = oline;
    lx_seg_f[lx_nseg] = file;
    lx_nseg = lx_nseg + 1;
}

/* The file and line of the source's line x: the file's name, or 0 for
 * the source itself. */
static char *lx_where(int x, int *line) {
    int i;
    for (i = lx_nseg - 1; i >= 0; i--)
        if (lx_seg_x[i] <= x) {
            *line = lx_seg_o[i] + (x - lx_seg_x[i]);
            return lx_seg_f[i] < 0 ? 0 : lx_seg_nm[lx_seg_f[i]];
        }
    *line = x;
    return 0;
}

/* "line n", or "NAME line n" for a line an INCLUDE brought in. */
static char *lx_where_text(int x) {
    static char b[96];
    int line;
    char *f;
    f = lx_where(x, &line);
    if (f) sprintf(b, "%.60s line %d", f, line);
    else sprintf(b, "line %d", line);
    return b;
}

static void lx_put_where(int x) {
    int line;
    char *f;
    f = lx_where(x, &line);
    fdputs("f77: ", 2);
    if (f) { fdputs(f, 2); fdputc(' ', 2); }
    fdputs("line ", 2);
    fdputuint(2, (unsigned int)line);
    fdputs(": ", 2);
}

/* Is the line an INCLUDE?  Its name into nm, 1 when it is. */
static int lx_inc_is(const char *ln, int n, char *nm) {
    int i;
    int k;
    int col;
    const char *kw;
    if (n == 0 || ln[0] == 'C' || ln[0] == 'c' || ln[0] == '*' || ln[0] == '!') return 0;
    col = 1;
    i = 0;
    if (ln[0] == '\t') { i = 1; col = 7; }
    while (col <= 6 && i < n) {
        if (ln[i] == '\t') { i = i + 1; col = 7; break; }
        if (col == 6 ? (ln[i] != ' ' && ln[i] != '0') : ln[i] != ' ') return 0;
        i = i + 1;
        col = col + 1;
    }
    if (n > 72 && ln[0] != '\t') n = 72;              /* columns 73-80 are sequence numbers */
    while (i < n && ln[i] == ' ') i = i + 1;
    for (kw = "INCLUDE"; *kw; kw++, i++)
        if (i >= n || lx_upper(ln[i]) != *kw) return 0;
    while (i < n && ln[i] == ' ') i = i + 1;
    k = 0;
    if (i < n && (ln[i] == '\'' || ln[i] == '(')) {
        int close;
        close = ln[i] == '(' ? ')' : '\'';
        i = i + 1;
        while (i < n && ln[i] != close && k < 63) nm[k++] = ln[i++];
        if (i >= n) return 0;
        i = i + 1;
    } else {
        while (i < n && (lx_isalpha(ln[i]) || lx_isdigit(ln[i])) && k < 63)
            nm[k++] = ln[i++];
    }
    while (k > 0 && nm[k - 1] == ' ') k = k - 1;
    nm[k] = 0;
    while (i < n && (ln[i] == ' ' || ln[i] == '\r')) i = i + 1;
    if (i < n && ln[i] != '!') return 0;               /* more on the line: some other statement */
    return k > 0;
}

/* Read the text an INCLUDE names into a fresh buffer; 0 if none. */
static char *lx_inc_load(char *nm, int *len) {
    static const char *sfx[4] = { "", ".f", ".for", ".inc" };
    char path[1024];
    char low[64];
    char dir[960];
    int d, c, x, k, fd, n, cap;
    char *b;
    dir[0] = 0;
    if (lx_inname) {
        k = (int)strlen(lx_inname);
        while (k > 0 && lx_inname[k - 1] != '/') k = k - 1;
        if (k > 0 && k < (int)sizeof dir) { memcpy(dir, lx_inname, (size_t)k); dir[k] = 0; }
    }
    for (k = 0; nm[k] && k < 63; k++) low[k] = (char)(nm[k] >= 'A' && nm[k] <= 'Z' ? nm[k] - 'A' + 'a' : nm[k]);
    low[k] = 0;
    for (d = 0; d < 2; d++)
        for (c = 0; c < 2; c++)
            for (x = 0; x < 4; x++) {
                if (d == 0 && !dir[0]) continue;
                if (strlen(dir) + strlen(nm) + 5 >= sizeof path) continue;
                sprintf(path, "%s%s%s", d == 0 ? dir : "", c ? low : nm, sfx[x]);
                fd = open(path, 0);
                if (fd < 0) continue;
                cap = 1 << 16; n = 0;
                b = malloc((size_t)cap);
                for (;;) {
                    int r;
                    if (n == cap) { cap = cap * 2; b = realloc(b, (size_t)cap); }
                    r = (int)read(fd, b + n, (size_t)(cap - n));
                    if (r <= 0) break;
                    n = n + r;
                }
                close(fd);
                *len = n;
                return b;
            }
    return 0;
}

static void lx_inc_error(char *msg, char *nm) {
    lx_put_where(lx_xline);
    fdputs(msg, 2);
    fdputs(nm, 2);
    fdputc('\n', 2);
    f77_nerr = f77_nerr + 1;
}

/* Copy the text t[0..n) into out, INCLUDEs done; file is its index in
 * lx_seg_nm, -1 for the source.  Returns 0 when out is full. */
static int lx_expand(const char *t, int n, int file, char *out, int cap, int *olen) {
    int p;
    int q;
    int oline;
    int isinc;
    int j;
    int tl;
    char *tx;
    char nm[64];
    p = 0;
    oline = 0;
    while (p < n) {
        q = p;
        while (q < n && t[q] != '\n') q = q + 1;
        oline = oline + 1;
        if (*olen + (q - p) + 2 >= cap) return 0;
        isinc = lx_inc_is(t + p, q - p, nm);
        memcpy(out + *olen, t + p, (size_t)(q - p));
        if (isinc) out[*olen] = 'C';                   /* the INCLUDE line, now a comment */
        *olen = *olen + (q - p);
        out[*olen] = '\n';
        *olen = *olen + 1;
        lx_xline = lx_xline + 1;
        p = q < n ? q + 1 : q;
        if (!isinc) continue;
        for (j = 0; j < lx_inc_n; j++)
            if (strcmp(lx_inc_nm[j], nm) == 0) break;
        if (j < lx_inc_n) { lx_inc_error("INCLUDE of a text being read: ", nm); continue; }
        if (lx_inc_n >= LX_INC_DEPTH) { lx_inc_error("INCLUDEs nested too deep: ", nm); continue; }
        tx = lx_inc_load(nm, &tl);
        if (!tx) { lx_inc_error("INCLUDE: no file ", nm); continue; }
        strcpy(lx_inc_nm[lx_inc_n], nm);
        lx_inc_n = lx_inc_n + 1;
        lx_seg_note(lx_seg_file(nm), 1);
        if (!lx_expand(tx, tl, lx_seg_nnm > 0 ? lx_seg_file(nm) : -1, out, cap, olen)) { free(tx); return 0; }
        free(tx);
        lx_inc_n = lx_inc_n - 1;
        lx_seg_note(file, oline + 1);                  /* back in this text, after the INCLUDE */
    }
    return 1;
}

/* --- statement assembly --------------------------------------------- */

/* True when the line starting at `p` is a comment or is entirely blank.
 * F77: C or * in column 1.  D in column 1 is a debug line, which we
 * treat as a comment (the historic default when debug lines are off). */
static int lx_is_comment(int p) {
    int c;
    if (p >= lx_len) return 0;
    c = lx_upper(lx_src[p]);
    if (c == 'C' || c == '*' || c == '!' || c == 'D') return 1;
    return 0;
}

/* Advance past the newline that ends the line beginning at p. */
static int lx_next_line(int p) {
    while (p < lx_len && lx_src[p] != '\n') p = p + 1;
    if (p < lx_len) p = p + 1;
    lx_line = lx_line + 1;
    return p;
}

/* Is the line at p blank (only spaces/tabs in columns 1-72)?  Columns
 * 73-80 are sequence numbers and do not count: a card of 72 blanks with
 * its sequence number is a blank card.  FCVS decks have them, and before
 * this rule such a card assembled as an empty statement, which the
 * reader reported as end of file -- half the program vanished with no
 * diagnostic but a missing FORMAT (FM036, FM038). */
static int lx_is_blank_line(int p) {
    int col;
    col = 1;
    while (p < lx_len && lx_src[p] != '\n' && col <= 72) {
        if (lx_src[p] != ' ' && lx_src[p] != '\t' && lx_src[p] != '\r')
            return 0;
        p = p + 1;
        col = col + 1;
    }
    return 1;
}

/* Locate the statement body of the line at p.
 * Returns the offset of the first body character, and reports through
 * *cont whether the line is a continuation, and through *label the
 * value in columns 1-5 (-1 when blank). */
static int lx_line_body(int p, int *cont, int *label) {
    int col;
    int lab;
    int have_lab;
    int c;

    *cont = 0;
    *label = -1;
    lab = 0;
    have_lab = 0;
    col = 1;

    while (p < lx_len && lx_src[p] != '\n' && col <= 6) {
        c = lx_src[p];
        if (c == '\t') {
            /* Tab-format extension: the tab ends the label field.  A
             * digit 1-9 immediately after it marks a continuation;
             * anything else begins the statement body. */
            p = p + 1;
            if (p < lx_len && lx_src[p] >= '1' && lx_src[p] <= '9') {
                *cont = 1;
                p = p + 1;
            }
            if (have_lab) *label = lab;
            return p;
        }
        if (col == 6) {
            if (c != ' ' && c != '0') *cont = 1;
            p = p + 1;
            col = col + 1;
            break;
        }
        if (lx_isdigit(c)) { lab = lab * 10 + (c - '0'); have_lab = 1; }
        else if (c != ' ') {
            /* Junk in the label field: not fatal, F77 decks vary. */
        }
        p = p + 1;
        col = col + 1;
    }
    if (have_lab) *label = lab;
    return p;
}

/* Append one character to the statement buffer. */
static void lx_put(int c) {
    if (lx_stmt_len >= F77_MAX_STMT - 1) {
        f77_error("statement too long");
        return;
    }
    lx_brk[lx_stmt_len] = lx_after_blank ? 1 : 0;
    lx_stmt[lx_stmt_len] = (char)c;
    lx_stmt_len = lx_stmt_len + 1;
    lx_after_blank = 0;
}

/* Was the text just emitted an unsigned integer immediately preceded by
 * a non-alphanumeric?  That is what makes a following H a Hollerith
 * count rather than part of an identifier. */
static int lx_hcount_digits;   /* digits consumed by the count just read */

static int lx_pending_hollerith_count(void) {
    int i;
    int n;
    int v;
    i = lx_stmt_len - 1;
    n = 0;
    while (i >= 0 && lx_stmt[i] >= '0' && lx_stmt[i] <= '9') { i = i - 1; n = n + 1; }
    lx_hcount_digits = n;
    if (n == 0) return -1;
    if (i >= 0 && (lx_isalpha(lx_stmt[i]) || lx_stmt[i] == '.')) return -1;
    /* A type's length, REAL*8 H or CHARACTER*8 HEAD: the statement so
     * far is a type keyword and a star, and no Hollerith follows a
     * length.  (A DATA repeat count, 2*8H..., keeps its Hollerith.) */
    if (i > 0 && lx_stmt[i] == '*') {
        if (i == 7 && (strncmp(lx_stmt, "INTEGER", 7) == 0 || strncmp(lx_stmt, "LOGICAL", 7) == 0 ||
                       strncmp(lx_stmt, "COMPLEX", 7) == 0)) return -1;
        if (i == 4 && strncmp(lx_stmt, "REAL", 4) == 0) return -1;
        if (i == 9 && strncmp(lx_stmt, "CHARACTER", 9) == 0) return -1;
        if (i == 15 && strncmp(lx_stmt, "DOUBLEPRECISION", 15) == 0) return -1;
    }
    v = 0;
    i = i + 1;
    while (i < lx_stmt_len) { v = v * 10 + (lx_stmt[i] - '0'); i = i + 1; }
    return v;
}

/* Assemble the next statement.  Returns 1 on success, 0 at end of file. */
static int f77_next_stmt(void) {
    int p;
    int body;
    int cont;
    int label;
    int col;
    int c;
    int inq;
    int hcount;
    int first;

    lx_stmt_len = 0;
    lx_stmt_label = -1;
    lx_after_blank = 0;
    first = 1;
    inq = 0;

    for (;;) {
        /* Skip comment and blank lines. */
        while (lx_pos < lx_len &&
               (lx_is_comment(lx_pos) || lx_is_blank_line(lx_pos))) {
            lx_pos = lx_next_line(lx_pos);
        }
        if (lx_pos >= lx_len) break;

        p = lx_pos;
        body = lx_line_body(p, &cont, &label);

        if (!first && !cont) break;      /* a new statement begins here */
        if (first && cont) {
            /* Continuation with nothing to continue: tolerate it. */
            cont = 0;
        }
        if (first) {
            lx_stmt_label = label;
            lx_stmt_line = lx_line;
        }

        /* Copy columns 7-72, squeezing blanks outside character context. */
        col = 7;
        p = body;
        while (p < lx_len && lx_src[p] != '\n' && col <= 72) {
            c = lx_src[p];
            if (c == '\r') { p = p + 1; continue; }
            if (inq) {
                lx_put(c);
                if (c == '\'') {
                    /* '' inside a constant is an escaped quote. */
                    if (p + 1 < lx_len && lx_src[p + 1] == '\'') {
                        p = p + 1;
                        col = col + 1;
                        lx_put('\'');
                    } else {
                        inq = 0;
                    }
                }
                p = p + 1;
                col = col + 1;
                continue;
            }
            if (c == ' ' || c == '\t') {
                /* A blank between copied characters is a token
                 * boundary; leading indent is not. */
                if (lx_stmt_len > 0) lx_after_blank = 1;
                p = p + 1;
                col = col + 1;
                continue;
            }
            if (c == '!') break;                  /* trailing comment */
            if (c == '\'') { inq = 1; lx_put(c); p = p + 1; col = col + 1; continue; }
            if (lx_upper(c) == 'H') {
                hcount = lx_pending_hollerith_count();
                if (hcount > 0) {
                    /* Copy exactly hcount raw characters, blanks kept.
                     * Represent it as a quoted constant so the tokenizer
                     * has one string form to handle. */
                    int k;
                    /* Retract the count digits: they are part of the
                     * Hollerith constant, not a separate integer. */
                    lx_stmt_len = lx_stmt_len - lx_hcount_digits;
                    lx_put('\'');
                    p = p + 1;
                    col = col + 1;
                    k = 0;
                    while (k < hcount) {
                        if (col <= 72 && (p >= lx_len || lx_src[p] == '\n' || lx_src[p] == '\r')) {
                            /* A line ending short of column 72 is blank
                             * to 72, as its card was, and the blanks are
                             * part of the constant.  (Found by fort370.) */
                            lx_put(' ');
                            col = col + 1;
                            k = k + 1;
                            continue;
                        }
                        if (p >= lx_len || lx_src[p] == '\n' || col > 72) {
                            /* The constant continues on the next
                             * continuation line (comments between are
                             * allowed), from its column 7. */
                            int q;
                            int qcont;
                            int qlabel;
                            q = lx_next_line(lx_pos);
                            while (q < lx_len && (lx_is_comment(q) || lx_is_blank_line(q)))
                                q = lx_next_line(q);
                            if (q >= lx_len) {
                                f77_error("Hollerith constant runs off the end of the file");
                                break;
                            }
                            p = lx_line_body(q, &qcont, &qlabel);
                            if (!qcont) {
                                f77_error("Hollerith constant runs off the line");
                                break;
                            }
                            lx_pos = q;
                            col = 7;
                            continue;
                        }
                        if (lx_src[p] == '\'') lx_put('\'');
                        lx_put(lx_src[p]);
                        p = p + 1;
                        col = col + 1;
                        k = k + 1;
                    }
                    lx_put('\'');
                    continue;
                }
            }
            lx_put(lx_upper(c));
            p = p + 1;
            col = col + 1;
        }
        /* A character constant still open at the end of a line short of
         * column 72 keeps the blanks its card had out to 72, before the
         * continuation line's text: a file with trailing blanks trimmed
         * reads as the deck does.  (Found by fort370.) */
        if (inq) while (col <= 72) { lx_put(' '); col = col + 1; }

        lx_pos = lx_next_line(lx_pos);
        first = 0;

        /* Peek: is the next non-comment line a continuation? */
        {
            int q;
            int qcont;
            int qlabel;
            q = lx_pos;
            while (q < lx_len && (lx_is_comment(q) || lx_is_blank_line(q))) {
                q = lx_next_line(q);
                lx_pos = q;
            }
            if (q >= lx_len) break;
            lx_line_body(q, &qcont, &qlabel);
            if (!qcont) break;
        }
    }

    lx_stmt[lx_stmt_len] = 0;
    return lx_stmt_len > 0 || lx_stmt_label >= 0;
}

#endif
