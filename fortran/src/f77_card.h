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
static int   lx_free;       /* free-form source (lx_next_stmt_free) */
static int   lx_cpp;        /* preprocessed: the conditionals (lx_cpp_run) */
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
    if (n == 0 || ln[0] == '!') return 0;
    if (!lx_free && (ln[0] == 'C' || ln[0] == 'c' || ln[0] == '*')) return 0;
    col = 1;
    i = 0;
    if (lx_free) col = 7;                              /* free form: no columns */
    else if (ln[0] == '\t') { i = 1; col = 7; }
    while (col <= 6 && i < n) {
        if (ln[i] == '\t') { i = i + 1; col = 7; break; }
        if (col == 6 ? (ln[i] != ' ' && ln[i] != '0') : ln[i] != ' ') return 0;
        i = i + 1;
        col = col + 1;
    }
    if (!lx_free && n > 72 && ln[0] != '\t') n = 72;  /* columns 73-80 are sequence numbers */
    while (i < n && ln[i] == ' ') i = i + 1;
    for (kw = "INCLUDE"; *kw; kw++, i++)
        if (i >= n || lx_upper(ln[i]) != *kw) return 0;
    while (i < n && ln[i] == ' ') i = i + 1;
    k = 0;
    if (i < n && (ln[i] == '\'' || ln[i] == '"' || ln[i] == '(')) {
        int close;
        close = ln[i] == '(' ? ')' : ln[i];
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
        if (isinc) out[*olen] = '!';                   /* the INCLUDE line, now a comment (either form) */
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

/* Drop `n` characters of the assembled statement at `at`. */
static void lx_stmt_cut(int at, int n) {
    memmove(lx_stmt + at, lx_stmt + at + n, (size_t)(lx_stmt_len - at - n));
    memmove(lx_brk + at, lx_brk + at + n, (size_t)(lx_stmt_len - at - n));
    lx_stmt_len = lx_stmt_len - n;
}

/* RECURSIVE (Fortran 90) on a SUBROUTINE or FUNCTION header -- RECURSIVE
 * SUBROUTINE S, RECURSIVE [type] FUNCTION F, type RECURSIVE FUNCTION F -- is
 * dropped from the text, so the unit scan and the parser see the header
 * they know.  Nothing more is needed: a subprogram's locals are in its
 * frame already (SAVE and DATA make the static ones, as F90 has it).  Only
 * a statement with no '=' outside parentheses: an assignment to a
 * variable whose name begins RECURSIVE is left alone. */
static void lx_strip_recursive(void) {
    static const char *after[] = { "SUBROUTINE", "FUNCTION", "INTEGER", "REAL", "DOUBLE",
                                   "COMPLEX", "LOGICAL", "CHARACTER", 0 };
    static const char *types[] = { "INTEGER", "REAL", "DOUBLEPRECISION", "DOUBLECOMPLEX",
                                   "COMPLEX", "LOGICAL", 0 };
    int i;
    int depth;
    if (lx_stmt_len < 17) return;
    depth = 0;
    for (i = 0; i < lx_stmt_len; i++) {
        if (lx_stmt[i] == '\'') return;        /* no header has a string */
        if (lx_stmt[i] == '(') depth = depth + 1;
        else if (lx_stmt[i] == ')') depth = depth - 1;
        else if (lx_stmt[i] == '=' && depth == 0) return;
    }
    if (strncmp(lx_stmt, "RECURSIVE", 9) == 0) {
        for (i = 0; after[i]; i++)
            if (strncmp(lx_stmt + 9, after[i], strlen(after[i])) == 0) {
                lx_stmt_cut(0, 9);
                lx_brk[0] = 0;
                return;
            }
        return;
    }
    for (i = 0; types[i]; i++) {
        int n;
        n = (int)strlen(types[i]);
        if (strncmp(lx_stmt, types[i], n) == 0 && n + 17 <= lx_stmt_len &&
            strncmp(lx_stmt + n, "RECURSIVEFUNCTION", 17) == 0) {
            lx_stmt_cut(n, 9);
            return;
        }
    }
}

/* True when the line starting at `p` is a comment or is entirely blank.
 * F77: C or * in column 1.  D in column 1 is a debug line, which we
 * treat as a comment (the historic default when debug lines are off). */
static int lx_is_comment(int p) {
    int c;
    int col;
    if (p >= lx_len) return 0;
    c = lx_upper(lx_src[p]);
    if (c == 'C' || c == '*' || c == '!' || c == 'D') return 1;
    /* Fortran 90 fixed form: a line whose first non-blank character is
     * '!' is a comment line, whatever the column -- except 6, where any
     * character marks a continuation.  An indented "! ..." line had
     * been an empty statement. */
    col = 1;
    while (p < lx_len && (lx_src[p] == ' ' || lx_src[p] == '\t')) {
        col = lx_src[p] == '\t' ? 7 : col + 1;
        p = p + 1;
    }
    return p < lx_len && lx_src[p] == '!' && col != 6;
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

/* --- free-form source (Fortran 90) ------------------------------------
 *
 * A .f90 or .f95 file (lx_free).  Blanks are significant in free form,
 * but a valid free-form statement, squeezed, is text the fixed-form
 * parser already reads: fixed form never relied on blanks.  So only the
 * assembly differs, and the statement comes out as the fixed-form reader
 * makes it -- blanks squeezed (lx_brk keeps where they were), upper case
 * outside character constants, "..." as '...'.  The rules (F90 3.3.1):
 * - a line runs to its end; `!` outside a character constant begins a
 *   comment;
 * - `&` as the last non-blank character before any comment continues the
 *   statement on the next non-comment line, where a first non-blank `&`
 *   is dropped; a character constant continued so resumes right after
 *   that `&`, or at column 1 without one;
 * - `;` ends a statement, and another may follow on the line;
 * - a statement may begin with a label of 1 to 5 digits.
 * No Hollerith: F90 free form has no H edit descriptor's blanks to keep,
 * and an nH left in the text is refused where it is parsed. */
/* An error the reader finds in the text: once, though the unit scan, the
 * compilation and the inliner each read the statement -- the scan reads
 * the whole file first, in order, so a line not past the last reported
 * has been reported. */
static int lx_card_err_line;
static void lx_card_error(char *msg) {
    if (lx_stmt_line <= lx_card_err_line) return;
    lx_card_err_line = lx_stmt_line;
    f77_error(msg);
}

/* Is the rest of the line from p only blanks, or a comment? */
static int lx_free_rest_blank(int p) {
    while (p < lx_len && (lx_src[p] == ' ' || lx_src[p] == '\t' || lx_src[p] == '\r')) p = p + 1;
    return p >= lx_len || lx_src[p] == '\n' || lx_src[p] == '!';
}

static int lx_next_stmt_free(void) {
    int p;
    int c;
    int inq;
    int first;
    int cont;

    lx_stmt_len = 0;
    lx_stmt_label = -1;
    lx_after_blank = 0;
    first = 1;
    inq = 0;
    for (;;) {
        p = lx_pos;
        if (first) {
            /* blanks, empty statements (;) and comment lines before it */
            while (p < lx_len && (lx_src[p] == ' ' || lx_src[p] == '\t' || lx_src[p] == '\r' || lx_src[p] == ';'))
                p = p + 1;
            if (p >= lx_len) { lx_pos = p; return 0; }
            if (lx_src[p] == '\n' || lx_src[p] == '!') { lx_pos = lx_next_line(p); continue; }
            lx_stmt_line = lx_line;
            if (lx_isdigit(lx_src[p])) {
                int q;
                int v;
                q = p;
                v = 0;
                while (q < lx_len && lx_isdigit(lx_src[q])) { v = v * 10 + (lx_src[q] - '0'); q = q + 1; }
                if (q - p <= 5 && q < lx_len && (lx_src[q] == ' ' || lx_src[q] == '\t')) {
                    lx_stmt_label = v;
                    p = q;
                }
            }
        } else {
            /* a continuation line: comment lines between are allowed */
            int q;
            q = p;
            while (q < lx_len && (lx_src[q] == ' ' || lx_src[q] == '\t' || lx_src[q] == '\r')) q = q + 1;
            if (q >= lx_len) { lx_card_error("the file ends in a continued statement"); break; }
            if (lx_src[q] == '\n' || lx_src[q] == '!') { lx_pos = lx_next_line(q); continue; }
            if (lx_src[q] == '&') p = q + 1;
            else if (!inq) { p = q; if (lx_stmt_len > 0) lx_after_blank = 1; }
        }
        cont = 0;
        while (p < lx_len && lx_src[p] != '\n') {
            c = lx_src[p];
            if (c == '\r') { p = p + 1; continue; }
            if (inq) {
                if (c == '&' && lx_free_rest_blank(p + 1)) { cont = 1; break; }
                if (c == inq) {
                    if (p + 1 < lx_len && lx_src[p + 1] == inq) {
                        /* a doubled quote: one of it */
                        if (inq == '\'') { lx_put('\''); lx_put('\''); } else lx_put('"');
                        p = p + 2;
                        continue;
                    }
                    lx_put('\'');
                    inq = 0;
                    p = p + 1;
                    continue;
                }
                if (c == '\'' && inq == '"') { lx_put('\''); lx_put('\''); p = p + 1; continue; }
                lx_put(c);
                p = p + 1;
                continue;
            }
            if (c == ' ' || c == '\t') {
                if (lx_stmt_len > 0) lx_after_blank = 1;
                p = p + 1;
                continue;
            }
            if (c == '!') break;
            if (c == '&') {
                if (!lx_free_rest_blank(p + 1)) { lx_card_error("& in the middle of a line"); break; }
                cont = 1;
                break;
            }
            if (c == ';') {
                /* the next statement begins after it, on this line */
                lx_strip_recursive();
                lx_stmt[lx_stmt_len] = 0;
                lx_pos = p + 1;
                if (lx_stmt_len > 0 || lx_stmt_label >= 0) return 1;
                first = 1;
                lx_stmt_label = -1;
                lx_after_blank = 0;
                break;
            }
            if (c == '\'' || c == '"') { inq = c; lx_put('\''); p = p + 1; continue; }
            lx_put(lx_upper(c));
            p = p + 1;
        }
        if (first && lx_pos > p) continue;          /* an empty statement before a ';' */
        lx_pos = lx_next_line(p);
        if (!cont) {
            if (inq) lx_card_error("a character constant runs off the line");
            break;
        }
        first = 0;
    }
    lx_strip_recursive();
    lx_stmt[lx_stmt_len] = 0;
    return lx_stmt_len > 0 || lx_stmt_label >= 0;
}

/* --- the preprocessor's conditionals ------------------------------------
 *
 * A .F, .F90 or .F95 file is preprocessed, as gfortran and flang do by
 * the name.  What is taken is the conditionals -- #if, #ifdef, #ifndef,
 * #elif, #else, #endif, with defined(), integers, ! && || == != < <= > >=
 * + - and parentheses -- over no macros at all: every name is undefined
 * and 0, as it is to a compiler not asked for OpenMP or anything else.
 * LAPACK's #if defined(_OPENMP) and #ifdef USE_ISNAN are the use.  A
 * directive line, and every line a false condition skips, gets '!' in
 * column 1 -- a comment in either form, and the lines keep their numbers.
 * #define, #undef and #include are refused, not ignored: a macro left
 * unexpanded would change the program without a word. */
static const char *lx_ce_p;
static const char *lx_ce_e;
static int lx_ce_bad;

static void lx_ce_ws(void) { while (lx_ce_p < lx_ce_e && (*lx_ce_p == ' ' || *lx_ce_p == '\t' || *lx_ce_p == '\r')) lx_ce_p++; }
static int lx_ce_is(const char *s) {
    int n;
    lx_ce_ws();
    n = (int)strlen(s);
    if (lx_ce_e - lx_ce_p >= n && strncmp(lx_ce_p, s, (size_t)n) == 0) { lx_ce_p += n; return 1; }
    return 0;
}
static int lx_ce_or(void);
static int lx_ce_prim(void) {
    int v;
    lx_ce_ws();
    if (lx_ce_is("(")) { v = lx_ce_or(); if (!lx_ce_is(")")) lx_ce_bad = 1; return v; }
    if (lx_ce_is("!")) return !lx_ce_prim();
    if (lx_ce_is("-")) return -lx_ce_prim();
    if (lx_ce_is("+")) return lx_ce_prim();
    if (lx_ce_p < lx_ce_e && lx_isdigit(*lx_ce_p)) {
        v = 0;
        while (lx_ce_p < lx_ce_e && lx_isdigit(*lx_ce_p)) { v = v * 10 + (*lx_ce_p - '0'); lx_ce_p++; }
        while (lx_ce_p < lx_ce_e && (*lx_ce_p == 'L' || *lx_ce_p == 'l' || *lx_ce_p == 'U' || *lx_ce_p == 'u')) lx_ce_p++;
        return v;
    }
    if (lx_ce_p < lx_ce_e && (lx_isalpha(*lx_ce_p) || *lx_ce_p == '_')) {
        const char *b;
        b = lx_ce_p;
        while (lx_ce_p < lx_ce_e && (lx_isalpha(*lx_ce_p) || lx_isdigit(*lx_ce_p) || *lx_ce_p == '_')) lx_ce_p++;
        if (lx_ce_p - b == 7 && strncmp(b, "defined", 7) == 0) {
            int par;
            par = lx_ce_is("(");
            lx_ce_ws();
            if (!(lx_ce_p < lx_ce_e && (lx_isalpha(*lx_ce_p) || *lx_ce_p == '_'))) { lx_ce_bad = 1; return 0; }
            while (lx_ce_p < lx_ce_e && (lx_isalpha(*lx_ce_p) || lx_isdigit(*lx_ce_p) || *lx_ce_p == '_')) lx_ce_p++;
            if (par && !lx_ce_is(")")) lx_ce_bad = 1;
        }
        return 0;                                  /* no macros: every name is 0 */
    }
    lx_ce_bad = 1;
    return 0;
}
static int lx_ce_add(void) {
    int v;
    v = lx_ce_prim();
    for (;;) {
        if (lx_ce_is("+")) v = v + lx_ce_prim();
        else if (lx_ce_is("-")) v = v - lx_ce_prim();
        else return v;
    }
}
static int lx_ce_rel(void) {
    int v;
    v = lx_ce_add();
    for (;;) {
        if (lx_ce_is("<=")) v = v <= lx_ce_add();
        else if (lx_ce_is(">=")) v = v >= lx_ce_add();
        else if (lx_ce_is("<")) v = v < lx_ce_add();
        else if (lx_ce_is(">")) v = v > lx_ce_add();
        else return v;
    }
}
static int lx_ce_eq(void) {
    int v;
    v = lx_ce_rel();
    for (;;) {
        if (lx_ce_is("==")) v = v == lx_ce_rel();
        else if (lx_ce_is("!=")) v = v != lx_ce_rel();
        else return v;
    }
}
static int lx_ce_and(void) {
    int v;
    v = lx_ce_eq();
    while (lx_ce_is("&&")) { int w; w = lx_ce_eq(); v = v && w; }
    return v;
}
static int lx_ce_or(void) {
    int v;
    v = lx_ce_and();
    while (lx_ce_is("||")) { int w; w = lx_ce_and(); v = v || w; }
    return v;
}

/* One directive's expression, t[0..n): its value; *bad on what it cannot read. */
static int lx_ce_eval(const char *t, int n, int *bad) {
    int v;
    lx_ce_p = t;
    lx_ce_e = t + n;
    lx_ce_bad = 0;
    v = lx_ce_or();
    lx_ce_ws();
    if (lx_ce_p < lx_ce_e) lx_ce_bad = 1;
    *bad = lx_ce_bad;
    return v;
}

static void lx_cpp_error(char *msg, int line) {
    lx_stmt_line = line;
    lx_stmt_len = 0;
    lx_stmt[0] = 0;
    f77_error(msg);
}

/* The conditionals, over t[0..n) in place. */
static void lx_cpp_run(char *t, int n) {
    int p;
    int line;
    int depth;
    int live[64];       /* is this level's current branch taken (and its parents)? */
    int done[64];       /* has a branch of this level been taken yet? */
    p = 0;
    line = 0;
    depth = 0;
    live[0] = 1;
    while (p < n) {
        int q;
        int d;
        q = p;
        while (q < n && t[q] != '\n') q = q + 1;
        line = line + 1;
        d = p;
        while (d < q && (t[d] == ' ' || t[d] == '\t')) d = d + 1;
        if (d < q && t[d] == '#') {
            int k;
            int kn;
            int bad;
            k = d + 1;
            while (k < q && (t[k] == ' ' || t[k] == '\t')) k = k + 1;
            kn = k;
            while (kn < q && lx_isalpha(t[kn])) kn = kn + 1;
            bad = 0;
            if (kn - k == 2 && strncmp(t + k, "if", 2) == 0) {
                int v;
                v = lx_ce_eval(t + kn, q - kn, &bad);
                if (depth >= 63) { lx_cpp_error("#if nested too deep", line); return; }
                depth = depth + 1;
                live[depth] = live[depth - 1] && v;
                done[depth] = live[depth];
            } else if ((kn - k == 5 && strncmp(t + k, "ifdef", 5) == 0) || (kn - k == 6 && strncmp(t + k, "ifndef", 6) == 0)) {
                int v;
                v = kn - k == 6;                      /* no macros: #ifndef X is true */
                if (depth >= 63) { lx_cpp_error("#if nested too deep", line); return; }
                depth = depth + 1;
                live[depth] = live[depth - 1] && v;
                done[depth] = live[depth];
            } else if (kn - k == 4 && strncmp(t + k, "elif", 4) == 0) {
                int v;
                if (depth == 0) { lx_cpp_error("#elif without #if", line); return; }
                v = lx_ce_eval(t + kn, q - kn, &bad);
                live[depth] = live[depth - 1] && !done[depth] && v;
                if (live[depth]) done[depth] = 1;
            } else if (kn - k == 4 && strncmp(t + k, "else", 4) == 0) {
                if (depth == 0) { lx_cpp_error("#else without #if", line); return; }
                live[depth] = live[depth - 1] && !done[depth];
                done[depth] = 1;
            } else if (kn - k == 5 && strncmp(t + k, "endif", 5) == 0) {
                if (depth == 0) { lx_cpp_error("#endif without #if", line); return; }
                depth = depth - 1;
            } else if (live[depth]) {
                lx_cpp_error("only the preprocessor's conditionals are supported (#if, #ifdef, #ifndef, #elif, #else, #endif)", line);
            }
            if (bad && live[depth > 0 ? depth - 1 : 0]) lx_cpp_error("a #if expression this preprocessor cannot read", line);
            if (q > p) t[p] = '!';
            else if (q == p) { /* an empty line stays empty */ }
        } else if (!live[depth] && q > p) {
            t[p] = '!';
        }
        p = q < n ? q + 1 : q;
    }
    if (depth != 0) lx_cpp_error("#if without #endif", line);
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

    if (lx_free) return lx_next_stmt_free();
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
            if (inq == '"') {
                /* A "..." constant (Fortran 90) goes out in the one form
                 * the tokenizer knows, '...': "" inside it is one ", and
                 * an apostrophe inside it is doubled. */
                if (c == '"') {
                    if (p + 1 < lx_len && lx_src[p + 1] == '"') {
                        lx_put('"');
                        p = p + 1;
                        col = col + 1;
                    } else {
                        lx_put('\'');
                        inq = 0;
                    }
                } else if (c == '\'') {
                    lx_put('\'');
                    lx_put('\'');
                } else {
                    lx_put(c);
                }
                p = p + 1;
                col = col + 1;
                continue;
            }
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
            if (c == '\'') { inq = '\''; lx_put(c); p = p + 1; col = col + 1; continue; }
            if (c == '"') { inq = '"'; lx_put('\''); p = p + 1; col = col + 1; continue; }
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

    lx_strip_recursive();
    lx_stmt[lx_stmt_len] = 0;
    return lx_stmt_len > 0 || lx_stmt_label >= 0;
}

#endif
