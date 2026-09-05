/* s32sort -- a DFSORT-shaped sort/merge utility for SLOW-32.
 *
 *   s32sort SORTIN=in1 [SORTIN=in2 ...] SORTOUT=out [SYSIN=ctl] [MAINSIZE=nM]
 *
 * Control statements come from SYSIN (stdin when not given), one per
 * line, continuation by a trailing comma or an open parenthesis:
 *
 *   SORT  FIELDS=(p,l,f,o[,p,l,f,o]...)   MERGE FIELDS=(...)   SORT FIELDS=COPY
 *   RECORD TYPE={F|L},LENGTH=n            F: fixed n-byte records (default, n from
 *                                         the first SORTIN's size if absent);
 *                                         L: text lines, padded with blanks to n for
 *                                         the sort and written back at their own
 *                                         length (a reformatted record is trimmed);
 *                                         LENGTH omitted: the longest input line
 *   INCLUDE COND=(p,l,f,op,{p,l,f|C'..'|X'..'|[+-]n}[,{AND|OR},...])
 *   OMIT    COND=(...)                    op: EQ NE GT GE LT LE
 *   SUM     FIELDS=(p,l,f,...) | NONE     records equal on the sort fields collapse
 *                                         into one, these fields summed (NONE: kept)
 *   INREC   FIELDS=(item,...)             reformat before the sort
 *   OUTREC  FIELDS=(item,...)             reformat after; item = p,l | nX | C'..' | X'..'
 *   OPTION  ...                           EQUALS is always on; the rest is accepted
 *   OUTFIL  FNAMES=(dd,...)[,INCLUDE=(..)|OMIT=(..)|SAVE][,OUTREC=(items)]
 *           [,STARTREC=n][,ENDREC=n][,SPLIT|SPLITBY=n]
 *                                         any number; every sorted record goes to each
 *                                         OUTFIL whose selection passes; SAVE takes what
 *                                         no other OUTFIL took; SPLIT rotates the FNAMES;
 *                                         SORTOUT, when given, still receives everything
 *   JOINKEYS F1=dd,FIELDS=(p,l,o,...)[,SORTED][,LENGTH=n]     JOINKEYS F2=dd,FIELDS=(...)
 *   JOIN    UNPAIRED[,F1][,F2][,ONLY]     default: paired records only (inner join)
 *   REFORMAT FIELDS=(F1:p,l | F2:p,l | ? ...)[,FILL=C'.'|X'..']
 *                                         the joined record enters the main task (SORT,
 *                                         INCLUDE, SUM, OUTREC, OUTFIL apply to it); ? is
 *                                         the pairing indicator B, 1 or 2
 *
 * Data sets are named on the command line, DFSORT's DD names as NAME=path:
 * SORTIN (repeatable), SORTOUT, SYSIN, and whatever OUTFIL FNAMES and
 * JOINKEYS F1/F2 name.
 *
 * Formats: CH (bytes), ZD (zoned decimal, overpunched or trailing/leading
 * sign), PD (packed decimal), BI (unsigned big-endian binary), FI (signed
 * big-endian binary), FS (a signed decimal string: blanks, [+-], digits),
 * AC (bytes, as CH).  p counts from 1, as DFSORT does.  o is A or D.
 *
 * The sort runs on cobol/libcob/xsort.h: each record's fields render once
 * into a byte string whose unsigned order is the requested order (numbers
 * as sign-folded big-endian 64-bit values, D fields complemented, the
 * arrival number last), and the engine keeps as many records in memory as
 * MAINSIZE (default half the heap the program was linked with) allows,
 * spilling sorted runs beside SORTOUT and merging them k ways.  MERGE
 * treats each SORTIN as already in order.  Equal keys keep input order.
 *
 * Plain C: the same source builds for the host, and dfsort/tests runs
 * every deck on both and compares the bytes. */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

static void fatal(const char *m) { fprintf(stderr, "s32sort: %s\n", m); exit(16); }
#include "../cobol/libcob/xsort.h"

/* ---------------------------------------------------------------- */
/* fields                                                            */

enum { F_CH, F_ZD, F_PD, F_BI, F_FI, F_FS };
static const char *fmt_names[] = { "CH", "ZD", "PD", "BI", "FI", "FS", 0 };

typedef struct { unsigned pos, len; int fmt, desc; } field;       /* pos 0-based */

#define MAXF 64
static field sortf[MAXF]; static unsigned nsortf; static int copy_only, is_merge;
static field sumf[MAXF]; static unsigned nsumf; static int sum_none, have_sum;

/* INREC/OUTREC items */
typedef struct { int kind; unsigned pos, len; unsigned char *lit; } ritem;    /* kind 0 field, 1 blanks, 2 literal */
static ritem inrec[MAXF]; static unsigned ninrec;
static ritem outrec[MAXF]; static unsigned noutrec;

/* INCLUDE/OMIT: a flat list of (comparison, connector) */
typedef struct { field a; int op; int rhs_is_field; field b; unsigned char lit[64]; unsigned litlen; long long num; int is_num; int conn; } cond;   /* conn: 0 none, 1 AND, 2 OR */
static cond conds[MAXF]; static unsigned nconds; static int cond_is_omit;

static unsigned rec_len, in_len;           /* record length after INREC / raw input length */
static int rec_text;                       /* RECORD TYPE=L */
#define LTAG 2                              /* TYPE=L: the line's own length, after the padded record */
static unsigned slot_of(unsigned len) { return rec_text ? len + LTAG : len; }
static unsigned line_len(const unsigned char *rec, unsigned len) { return rec_text ? (unsigned)(rec[len] | (rec[len + 1] << 8)) : len; }
static void set_line_len(unsigned char *rec, unsigned len, unsigned n) { if (rec_text) { rec[len] = (unsigned char)n; rec[len + 1] = (unsigned char)(n >> 8); } }
static size_t mainsize;

/* data sets: NAME=path from the command line */
typedef struct { char name[32]; const char *path; } ddent;
static ddent dds[128]; static unsigned ndds;
static const char *dd_path(const char *name)
{
    for (unsigned i = 0; i < ndds; i++) if (!strcmp(dds[i].name, name)) return dds[i].path;
    return 0;
}

/* OUTFIL */
typedef struct {
    char names[16][32]; unsigned nnames; FILE *fp[16];
    cond conds[MAXF]; unsigned nconds; int omit; int save;
    ritem items[MAXF]; unsigned nitems; unsigned char *buf;
    unsigned long long startrec, endrec; unsigned split; unsigned next; unsigned long long seen, written;
} outfil;
static outfil ofs[32]; static unsigned nofs;

/* JOINKEYS */
typedef struct { const char *dd; field keys[MAXF]; unsigned nkeys; unsigned len; int sorted; } joinside;
static joinside jk[2]; static int have_join;
static int join_unpaired1, join_unpaired2, join_only;
typedef struct { int side; unsigned pos, len; } rfitem;    /* side 1/2, 0 = the ? indicator */
static rfitem reform[MAXF]; static unsigned nreform; static unsigned char join_fill = ' ';

/* ---------------------------------------------------------------- */
/* numeric decoding                                                  */

static long long dec_zd(const unsigned char *p, unsigned n)
{
    long long v = 0; int neg = 0;
    for (unsigned i = 0; i < n; i++) {
        unsigned char c = p[i];
        if (c >= '0' && c <= '9') v = v * 10 + (c - '0');
        else if (c >= 'p' && c <= 'y') { v = v * 10 + (c - 'p'); neg = 1; }      /* overpunched negative */
        else if (c >= 0xD0 && c <= 0xD9) { v = v * 10 + (c - 0xD0); neg = 1; }  /* EBCDIC zone D */
        else if (c >= 0xC0 && c <= 0xC9) v = v * 10 + (c - 0xC0);               /* EBCDIC zone C */
        else if (c == '{') v = v * 10;
        else if (c >= 'A' && c <= 'I') v = v * 10 + (c - 'A' + 1);
        else if (c == '}') { v = v * 10; neg = 1; }
        else if (c >= 'J' && c <= 'R') { v = v * 10 + (c - 'J' + 1); neg = 1; }
        else if (c == '-') neg = 1;
        /* blanks, '+', anything else: skipped */
    }
    return neg ? -v : v;
}
static long long dec_pd(const unsigned char *p, unsigned n)
{
    long long v = 0;
    for (unsigned i = 0; i < n; i++) { v = v * 10 + (p[i] >> 4); if (i + 1 < n) v = v * 10 + (p[i] & 15); }
    unsigned s = p[n - 1] & 15;
    return (s == 0xD || s == 0xB) ? -v : v;
}
static long long dec_bi(const unsigned char *p, unsigned n, int signed_)
{
    unsigned long long u = 0;
    for (unsigned i = 0; i < n; i++) u = (u << 8) | p[i];
    if (signed_ && n < 8 && (u >> (8 * n - 1)) & 1) u |= ~0ULL << (8 * n);
    return (long long)u;
}
static long long dec_fs(const unsigned char *p, unsigned n)
{
    long long v = 0; int neg = 0; unsigned i = 0;
    while (i < n && p[i] == ' ') i++;
    if (i < n && (p[i] == '-' || p[i] == '+')) { neg = p[i] == '-'; i++; }
    for (; i < n && p[i] >= '0' && p[i] <= '9'; i++) v = v * 10 + (p[i] - '0');
    return neg ? -v : v;
}
static long long dec_num(const unsigned char *rec, const field *f)
{
    const unsigned char *p = rec + f->pos;
    switch (f->fmt) {
    case F_ZD: return dec_zd(p, f->len);
    case F_PD: return dec_pd(p, f->len);
    case F_BI: return dec_bi(p, f->len, 0);
    case F_FI: return dec_bi(p, f->len, 1);
    case F_FS: return dec_fs(p, f->len);
    }
    return 0;
}

/* encode v back into the field (for SUM) */
static void enc_num(unsigned char *rec, const field *f, long long v)
{
    unsigned char *p = rec + f->pos; unsigned n = f->len;
    int neg = v < 0; unsigned long long a = neg ? 0 - (unsigned long long)v : (unsigned long long)v;
    switch (f->fmt) {
    case F_ZD:
        for (int i = (int)n - 1; i >= 0; i--) { p[i] = (unsigned char)('0' + a % 10); a /= 10; }
        if (neg) p[n - 1] = (unsigned char)('p' + (p[n - 1] - '0'));
        break;
    case F_PD: {
        p[n - 1] = (unsigned char)(((a % 10) << 4) | (neg ? 0xD : 0xC)); a /= 10;
        for (int i = (int)n - 2; i >= 0; i--) { p[i] = (unsigned char)(a % 10); a /= 10; p[i] |= (unsigned char)((a % 10) << 4); a /= 10; }
        break;
    }
    case F_BI: case F_FI: {
        unsigned long long u = (unsigned long long)v;
        for (int i = (int)n - 1; i >= 0; i--) { p[i] = (unsigned char)u; u >>= 8; }
        break;
    }
    case F_FS: {
        char buf[32]; int k = snprintf(buf, sizeof buf, "%s%llu", neg ? "-" : "", a);
        memset(p, ' ', n);
        if ((unsigned)k <= n) memcpy(p + n - k, buf, (size_t)k); else memcpy(p, buf + k - n, n);
        break;
    }
    }
}

/* ---------------------------------------------------------------- */
/* the normalized key                                                */

static unsigned key_len(void)
{
    unsigned k = 4;
    for (unsigned i = 0; i < nsortf; i++) k += sortf[i].fmt == F_CH ? sortf[i].len : 8;
    return k;
}
static void key_build(const unsigned char *rec, unsigned seq, unsigned char *out)
{
    for (unsigned i = 0; i < nsortf; i++) {
        const field *f = &sortf[i];
        unsigned char *o = out;
        if (f->fmt == F_CH) { memcpy(o, rec + f->pos, f->len); out += f->len; }
        else {
            unsigned long long u = (unsigned long long)dec_num(rec, f) ^ (1ULL << 63);
            for (int b = 7; b >= 0; b--) { o[b] = (unsigned char)u; u >>= 8; }
            out += 8;
        }
        if (f->desc) for (unsigned char *q = o; q < out; q++) *q = (unsigned char)~*q;
    }
    out[0] = (unsigned char)(seq >> 24); out[1] = (unsigned char)(seq >> 16); out[2] = (unsigned char)(seq >> 8); out[3] = (unsigned char)seq;
}

/* ---------------------------------------------------------------- */
/* INCLUDE / OMIT                                                    */

static int cmp_field_bytes(const unsigned char *a, unsigned na, const unsigned char *b, unsigned nb)
{
    unsigned n = na > nb ? na : nb;
    for (unsigned i = 0; i < n; i++) { int ca = i < na ? a[i] : ' ', cb = i < nb ? b[i] : ' '; if (ca != cb) return ca < cb ? -1 : 1; }
    return 0;
}
static int cond_eval(const unsigned char *rec, const cond *c)
{
    int r;
    if (c->a.fmt == F_CH) {
        if (c->rhs_is_field) r = cmp_field_bytes(rec + c->a.pos, c->a.len, rec + c->b.pos, c->b.len);
        else r = cmp_field_bytes(rec + c->a.pos, c->a.len, c->lit, c->litlen);
    } else {
        long long va = dec_num(rec, &c->a), vb = c->rhs_is_field ? dec_num(rec, &c->b) : c->num;
        r = va < vb ? -1 : va > vb ? 1 : 0;
    }
    switch (c->op) { case 0: return r == 0; case 1: return r != 0; case 2: return r > 0; case 3: return r >= 0; case 4: return r < 0; default: return r <= 0; }
}
static int selected(const unsigned char *rec)
{
    if (!nconds) return 1;
    /* AND binds tighter than OR: evaluate as a sum of products */
    int any = 0, cur = 1;
    for (unsigned i = 0; i < nconds; i++) {
        cur = cur && cond_eval(rec, &conds[i]);
        if (i + 1 == nconds || conds[i].conn == 2) { any = any || cur; cur = 1; }
    }
    return cond_is_omit ? !any : any;
}

/* ---------------------------------------------------------------- */
/* INREC / OUTREC                                                    */

static unsigned reformat_len(const ritem *it, unsigned n)
{
    unsigned l = 0;
    for (unsigned i = 0; i < n; i++) l += it[i].len;
    return l;
}
static void reformat(const unsigned char *in, unsigned inlen, unsigned char *out, const ritem *it, unsigned n)
{
    for (unsigned i = 0; i < n; i++) {
        if (it[i].kind == 0) { for (unsigned j = 0; j < it[i].len; j++) out[j] = it[i].pos + j < inlen ? in[it[i].pos + j] : ' '; }
        else if (it[i].kind == 1) memset(out, ' ', it[i].len);
        else memcpy(out, it[i].lit, it[i].len);
        out += it[i].len;
    }
}

/* ---------------------------------------------------------------- */
/* control statements                                                */

static char ctl[65536]; static unsigned ctl_len;
static const char *cp;

static void syntax(const char *what) { char m[200]; snprintf(m, sizeof m, "control statement: %s near \"%.40s\"", what, cp ? cp : ""); fatal(m); }
static void skipws(void) { while (*cp == ' ' || *cp == '\t' || *cp == '\n' || *cp == '\r') cp++; }
static int accept(char c) { skipws(); if (*cp == c) { cp++; return 1; } return 0; }
static void expect(char c) { if (!accept(c)) { char m[40]; snprintf(m, sizeof m, "expected '%c'", c); syntax(m); } }
static int word(char *out, unsigned cap)
{
    skipws(); unsigned n = 0;
    while (isalnum((unsigned char)*cp) || *cp == '_' || *cp == '-' || *cp == '+') { if (n + 1 < cap) out[n++] = (char)toupper((unsigned char)*cp); cp++; }
    out[n] = 0; return n > 0;
}
static unsigned number(void)
{
    skipws(); if (!isdigit((unsigned char)*cp)) syntax("expected a number");
    unsigned v = 0; while (isdigit((unsigned char)*cp)) v = v * 10 + (unsigned)(*cp++ - '0');
    return v;
}
static int format_of(const char *w)
{
    for (int i = 0; fmt_names[i]; i++) if (!strcmp(w, fmt_names[i])) return i;
    if (!strcmp(w, "AC") || !strcmp(w, "AQ")) return F_CH;
    if (!strcmp(w, "CSF")) return F_FS;
    syntax("unknown field format"); return 0;
}
static void parse_pl(field *f)      /* p,l */
{
    f->pos = number(); if (!f->pos) syntax("field position starts at 1"); f->pos--;
    expect(','); f->len = number(); if (!f->len || f->len > 4096) syntax("bad field length");
}
static void parse_plf(field *f)     /* p,l,f */
{
    char w[16]; parse_pl(f); expect(','); if (!word(w, sizeof w)) syntax("expected a format"); f->fmt = format_of(w);
    if (f->fmt != F_CH && f->fmt != F_FS && f->fmt != F_ZD && f->len > 8 && (f->fmt == F_BI || f->fmt == F_FI)) syntax("binary field longer than 8");
}
static unsigned parse_lit(unsigned char *out, unsigned cap, int hex)  /* after C or X: 'text' */
{
    expect('\''); unsigned n = 0;
    if (!hex) { for (;;) { if (*cp == '\'') { if (cp[1] == '\'') { if (n < cap) out[n++] = '\''; cp += 2; continue; } cp++; break; } if (!*cp) syntax("unterminated literal"); if (n < cap) out[n++] = (unsigned char)*cp; cp++; } }
    else { for (;;) { if (*cp == '\'') { cp++; break; } if (!isxdigit((unsigned char)cp[0]) || !isxdigit((unsigned char)cp[1])) syntax("bad hex literal"); unsigned v; sscanf(cp, "%2x", &v); if (n < cap) out[n++] = (unsigned char)v; cp += 2; } }
    return n;
}

static void parse_fields_list(field *fs, unsigned *n, int with_order)
{
    expect('(');
    do {
        if (*n >= MAXF) syntax("too many fields");
        field *f = &fs[*n];
        parse_plf(f);
        f->desc = 0;
        if (with_order) { expect(','); char w[8]; if (!word(w, sizeof w)) syntax("expected A or D"); if (!strcmp(w, "D")) f->desc = 1; else if (strcmp(w, "A")) syntax("expected A or D"); }
        (*n)++;
    } while (accept(','));
    expect(')');
}

static void parse_items(ritem *it, unsigned *n)
{
    expect('(');
    do {
        if (*n >= MAXF) syntax("too many items");
        ritem *r = &it[*n]; memset(r, 0, sizeof *r);
        skipws();
        if ((*cp == 'C' || *cp == 'c') && cp[1] == '\'') { cp++; unsigned char buf[256]; unsigned l = parse_lit(buf, sizeof buf, 0); r->kind = 2; r->len = l; r->lit = malloc(l ? l : 1); memcpy(r->lit, buf, l); }
        else if ((*cp == 'X' || *cp == 'x') && cp[1] == '\'') { cp++; unsigned char buf[256]; unsigned l = parse_lit(buf, sizeof buf, 1); r->kind = 2; r->len = l; r->lit = malloc(l ? l : 1); memcpy(r->lit, buf, l); }
        else {
            unsigned v = number();
            skipws();
            if (*cp == 'X' || *cp == 'x') { cp++; r->kind = 1; r->len = v; }
            else if (*cp == ':') { cp++; if (v) syntax("column positions (c:) are not supported"); }
            else { expect(','); r->kind = 0; if (!v) syntax("position starts at 1"); r->pos = v - 1; r->len = number(); }
        }
        (*n)++;
    } while (accept(','));
    expect(')');
}

static void parse_cond_into(cond *cs, unsigned *ncs)
{
    expect('(');
    for (;;) {
        if (*ncs >= MAXF) syntax("too many conditions");
        cond *c = &cs[*ncs]; memset(c, 0, sizeof *c);
        parse_plf(&c->a); expect(',');
        char w[8]; if (!word(w, sizeof w)) syntax("expected a comparison");
        static const char *ops[] = { "EQ", "NE", "GT", "GE", "LT", "LE", 0 }; int op = -1;
        for (int i = 0; ops[i]; i++) if (!strcmp(w, ops[i])) op = i;
        if (op < 0) syntax("expected EQ NE GT GE LT LE"); c->op = op;
        expect(',');
        skipws();
        if ((*cp == 'C' || *cp == 'c') && cp[1] == '\'') { cp++; c->litlen = parse_lit(c->lit, sizeof c->lit, 0); if (c->a.fmt != F_CH) c->num = dec_fs(c->lit, c->litlen); }
        else if ((*cp == 'X' || *cp == 'x') && cp[1] == '\'') { cp++; c->litlen = parse_lit(c->lit, sizeof c->lit, 1); if (c->a.fmt != F_CH) c->num = dec_bi(c->lit, c->litlen, 0); }
        else if (*cp == '+' || *cp == '-' || (isdigit((unsigned char)*cp) && !(strchr(cp, ',') && strchr(cp, ',') < strchr(cp, ')') && isdigit((unsigned char)*cp) && 0))) {
            /* a number, or p,l,f: a number is followed by ',' only when p,l,f -- decide by looking for a format word */
            const char *save = cp; int neg = 0; if (*cp == '+' || *cp == '-') { neg = *cp == '-'; cp++; }
            unsigned v = number(); skipws();
            if (*cp == ',' && !neg && save[0] != '+') {           /* p,l,f  */
                cp = save; c->rhs_is_field = 1; parse_plf(&c->b);
            } else { c->is_num = 1; c->num = neg ? -(long long)v : (long long)v; if (c->a.fmt == F_CH) { c->litlen = (unsigned)snprintf((char *)c->lit, sizeof c->lit, "%lld", c->num); } }
        } else syntax("expected a field or a constant");
        (*ncs)++;
        skipws();
        if (*cp == ')') { cp++; break; }
        expect(',');
        if (!word(w, sizeof w)) syntax("expected AND or OR");
        if (!strcmp(w, "AND")) c->conn = 1; else if (!strcmp(w, "OR")) c->conn = 2; else syntax("expected AND or OR");
        expect(',');
    }
}
static void parse_cond(void) { parse_cond_into(conds, &nconds); }

/* the selection of an arbitrary condition list */
static int selected_by(const unsigned char *rec, const cond *cs, unsigned n, int omit)
{
    if (!n) return 1;
    int any = 0, cur = 1;
    for (unsigned i = 0; i < n; i++) {
        cur = cur && cond_eval(rec, &cs[i]);
        if (i + 1 == n || cs[i].conn == 2) { any = any || cur; cur = 1; }
    }
    return omit ? !any : any;
}

/* a parenthesised list of names: (A,B) or a single name */
static void parse_names(char names[][32], unsigned *n, unsigned cap)
{
    int paren = accept('(');
    do {
        skipws(); unsigned k = 0;
        while (isalnum((unsigned char)*cp) || *cp == '_' || *cp == '#' || *cp == '@' || *cp == '$' || *cp == '-') { if (k < 31) names[*n][k++] = (char)toupper((unsigned char)*cp); cp++; }
        names[*n][k] = 0;
        if (!k) syntax("expected a data set name");
        if (*n >= cap) syntax("too many names");
        (*n)++;
    } while (paren && accept(','));
    if (paren) expect(')');
}

static void parse_outfil(void)
{
    if (nofs >= 32) syntax("too many OUTFIL statements");
    outfil *o = &ofs[nofs]; memset(o, 0, sizeof *o);
    o->endrec = ~0ULL; o->startrec = 1;
    char w[32];
    for (;;) {
        if (!word(w, sizeof w)) break;
        if (!strcmp(w, "FNAMES") || !strcmp(w, "FILES")) { expect('='); parse_names(o->names, &o->nnames, 16); }
        else if (!strcmp(w, "INCLUDE")) { expect('='); parse_cond_into(o->conds, &o->nconds); o->omit = 0; }
        else if (!strcmp(w, "OMIT")) { expect('='); parse_cond_into(o->conds, &o->nconds); o->omit = 1; }
        else if (!strcmp(w, "SAVE")) o->save = 1;
        else if (!strcmp(w, "OUTREC") || !strcmp(w, "BUILD")) { expect('='); parse_items(o->items, &o->nitems); }
        else if (!strcmp(w, "STARTREC")) { expect('='); o->startrec = number(); }
        else if (!strcmp(w, "ENDREC")) { expect('='); o->endrec = number(); }
        else if (!strcmp(w, "SPLIT")) o->split = 1;
        else if (!strcmp(w, "SPLITBY")) { expect('='); o->split = number(); if (!o->split) syntax("SPLITBY needs a count"); }
        else syntax("unknown OUTFIL keyword");
        if (!accept(',')) break;
    }
    if (!o->nnames) syntax("OUTFIL needs FNAMES");
    nofs++;
}

static void parse_joinkeys(void)
{
    char w[32]; joinside *j = 0;
    for (;;) {
        if (!word(w, sizeof w)) break;
        if (!strcmp(w, "F1") || !strcmp(w, "F2") || !strcmp(w, "FILE") || !strcmp(w, "FILES")) {
            int side = w[1] == '2' ? 1 : 0;
            expect('='); char nm[1][32]; unsigned n = 0; parse_names(nm, &n, 1);
            j = &jk[side]; j->dd = strdup(nm[0]);
        } else if (!strcmp(w, "FIELDS")) {
            if (!j) syntax("JOINKEYS: FIELDS before F1/F2");
            expect('=');
            expect('(');
            do {
                if (j->nkeys >= MAXF) syntax("too many join keys");
                field *f = &j->keys[j->nkeys]; parse_pl(f); f->fmt = F_CH; f->desc = 0;
                expect(','); char o[8]; if (!word(o, sizeof o)) syntax("expected A or D");
                if (!strcmp(o, "D")) f->desc = 1; else if (strcmp(o, "A")) syntax("expected A or D");
                j->nkeys++;
            } while (accept(','));
            expect(')');
        } else if (!strcmp(w, "SORTED")) { if (j) j->sorted = 1; }
        else if (!strcmp(w, "NOSEQCK") || !strcmp(w, "SEQCK") || !strcmp(w, "TASKID")) { if (accept('=')) word(w, sizeof w); }
        else if (!strcmp(w, "LENGTH")) { expect('='); if (j) j->len = number(); }
        else if (!strcmp(w, "INCLUDE") || !strcmp(w, "OMIT")) syntax("JOINKEYS INCLUDE/OMIT is not supported: filter with a separate pass");
        else syntax("unknown JOINKEYS keyword");
        if (!accept(',')) break;
    }
    if (!j || !j->dd || !j->nkeys) syntax("JOINKEYS needs F1= or F2= and FIELDS=");
    have_join = 1;
}

static void parse_join(void)
{
    char w[32];
    while (word(w, sizeof w)) {
        if (!strcmp(w, "UNPAIRED")) { join_unpaired1 = join_unpaired2 = 1; }
        else if (!strcmp(w, "F1")) { join_unpaired2 = 0; join_unpaired1 = 1; }
        else if (!strcmp(w, "F2")) { if (join_unpaired1 && !join_unpaired2 && 0) {} join_unpaired2 = 1; }
        else if (!strcmp(w, "ONLY")) join_only = 1;
        else syntax("unknown JOIN keyword");
        if (!accept(',')) break;
    }
}

static void parse_reformat(void)
{
    char w[32];
    for (;;) {
        if (!word(w, sizeof w)) break;
        if (!strcmp(w, "FIELDS") || !strcmp(w, "BUILD")) {
            expect('='); expect('(');
            do {
                if (nreform >= MAXF) syntax("too many REFORMAT items");
                rfitem *r = &reform[nreform]; memset(r, 0, sizeof *r);
                skipws();
                if (*cp == '?') { cp++; r->side = 0; r->len = 1; }
                else {
                    if (!word(w, sizeof w) || (strcmp(w, "F1") && strcmp(w, "F2"))) syntax("REFORMAT item is F1:p,l, F2:p,l or ?");
                    r->side = w[1] == '2' ? 2 : 1; expect(':');
                    unsigned p = number(); if (!p) syntax("position starts at 1"); r->pos = p - 1; expect(','); r->len = number();
                }
                nreform++;
            } while (accept(','));
            expect(')');
        } else if (!strcmp(w, "FILL")) {
            expect('='); skipws(); unsigned char b[4]; unsigned n = 0;
            if (*cp == 'C' || *cp == 'c') { cp++; n = parse_lit(b, 1, 0); } else if (*cp == 'X' || *cp == 'x') { cp++; n = parse_lit(b, 1, 1); } else syntax("FILL=C'c' or X'hh'");
            if (n) join_fill = b[0];
        } else syntax("unknown REFORMAT keyword");
        if (!accept(',')) break;
    }
    if (!nreform) syntax("REFORMAT needs FIELDS");
}

static void parse_control(void)
{
    cp = ctl;
    char w[32];
    while (skipws(), *cp) {
        if (*cp == '*') { while (*cp && *cp != '\n') cp++; continue; }
        if (!word(w, sizeof w)) syntax("expected a statement");
        if (!strcmp(w, "SORT") || !strcmp(w, "MERGE")) {
            is_merge = !strcmp(w, "MERGE");
            for (;;) {
                if (!word(w, sizeof w)) break;
                if (!strcmp(w, "FIELDS")) {
                    expect('=');
                    skipws();
                    if (!strncmp(cp, "COPY", 4)) { cp += 4; copy_only = 1; }
                    else parse_fields_list(sortf, &nsortf, 1);
                } else if (!strcmp(w, "EQUALS") || !strcmp(w, "NOEQUALS")) { }
                else if (!strcmp(w, "FORMAT")) { expect('='); word(w, sizeof w); }
                else syntax("unknown SORT keyword");
                if (!accept(',')) break;
            }
        } else if (!strcmp(w, "RECORD")) {
            for (;;) {
                if (!word(w, sizeof w)) break;
                expect('=');
                if (!strcmp(w, "TYPE")) { word(w, sizeof w); if (!strcmp(w, "L")) rec_text = 1; else if (strcmp(w, "F")) syntax("RECORD TYPE is F or L"); }
                else if (!strcmp(w, "LENGTH")) { accept('('); in_len = number(); accept(')'); }
                else syntax("unknown RECORD keyword");
                if (!accept(',')) break;
            }
        } else if (!strcmp(w, "INCLUDE") || !strcmp(w, "OMIT")) {
            cond_is_omit = !strcmp(w, "OMIT");
            if (!word(w, sizeof w) || strcmp(w, "COND")) syntax("expected COND"); expect('=');
            skipws(); if (!strncmp(cp, "ALL", 3)) { cp += 3; } else if (!strncmp(cp, "NONE", 4)) { cp += 4; cond_is_omit = !cond_is_omit; } else parse_cond();
        } else if (!strcmp(w, "SUM")) {
            have_sum = 1;
            if (!word(w, sizeof w) || strcmp(w, "FIELDS")) syntax("expected FIELDS"); expect('=');
            skipws(); if (!strncmp(cp, "NONE", 4)) { cp += 4; sum_none = 1; } else { parse_fields_list(sumf, &nsumf, 0); for (unsigned i = 0; i < nsumf; i++) if (sumf[i].fmt == F_CH) syntax("SUM field must be numeric"); }
        } else if (!strcmp(w, "INREC") || !strcmp(w, "OUTREC")) {
            int in = !strcmp(w, "INREC");
            if (!word(w, sizeof w) || (strcmp(w, "FIELDS") && strcmp(w, "BUILD"))) syntax("expected FIELDS"); expect('=');
            if (in) parse_items(inrec, &ninrec); else parse_items(outrec, &noutrec);
        } else if (!strcmp(w, "OPTION")) {
            for (;;) { if (!word(w, sizeof w)) break; if (accept('=')) { skipws(); if (*cp == '(') { int d = 0; do { if (*cp == '(') d++; else if (*cp == ')') d--; cp++; } while (d && *cp); } else word(w, sizeof w); } if (!accept(',')) break; }
        } else if (!strcmp(w, "OUTFIL")) parse_outfil();
        else if (!strcmp(w, "JOINKEYS")) parse_joinkeys();
        else if (!strcmp(w, "JOIN")) parse_join();
        else if (!strcmp(w, "REFORMAT")) parse_reformat();
        else if (!strcmp(w, "END")) { break; }
        else syntax("unknown statement");
    }
}

/* ---------------------------------------------------------------- */
/* records in and out                                                */

static unsigned char *inbuf, *recbuf, *keybuf, *outbuf;
static unsigned long long n_in, n_sel, n_out;
static xsort xs; static unsigned klen;

/* Records move in 64K blocks.  Under the emulator a character through the
 * stream costs more than the sort does: copying 55,000 lines with fgets and
 * an fwrite each was 7,000 instructions a record, the sort itself 2,400. */
#define IOBLK 65536u
typedef struct { FILE *fp; unsigned char *b; unsigned n, i; int eof; } inblk;
static inblk rd;
static void in_open(FILE *fp) { rd.fp = fp; rd.n = rd.i = 0; rd.eof = 0; if (!rd.b) { rd.b = malloc(IOBLK); if (!rd.b) fatal("out of memory"); } }
static int in_fill(void)
{
    if (rd.eof) return 0;
    if (rd.i < rd.n) memmove(rd.b, rd.b + rd.i, rd.n - rd.i);
    rd.n -= rd.i; rd.i = 0;
    size_t got = fread(rd.b + rd.n, 1, IOBLK - rd.n, rd.fp);
    if (!got) rd.eof = 1;
    rd.n += (unsigned)got;
    return got > 0;
}
/* one raw record from a fixed or line file into buf (slot_of(in_len) bytes); 0 at end */
static int read_record(FILE *fp, unsigned char *buf)
{
    (void)fp;
    if (!rec_text) {
        while (rd.n - rd.i < in_len) if (!in_fill()) return 0;
        memcpy(buf, rd.b + rd.i, in_len); rd.i += in_len;
        return 1;
    }
    /* a text line: up to the newline, however long; the record keeps in_len */
    for (;;) {
        unsigned char *p = rd.b + rd.i, *e = rd.b + rd.n, *nl = p;
        while (nl < e && *nl != '\n') nl++;
        if (nl < e || rd.eof) {
            if (p == e && rd.eof) return 0;
            unsigned n = (unsigned)(nl - p), take = n;
            if (take && p[take - 1] == '\r') take--;
            if (take > in_len) take = in_len;
            memcpy(buf, p, take);
            memset(buf + take, ' ', in_len - take);
            set_line_len(buf, in_len, take);
            rd.i += n + (nl < e ? 1 : 0);
            return 1;
        }
        if (rd.n - rd.i >= IOBLK) {                    /* a line longer than the block: keep its head */
            memcpy(buf, p, in_len < IOBLK ? in_len : IOBLK); set_line_len(buf, in_len, in_len);
            memset(buf + (in_len < IOBLK ? in_len : IOBLK), ' ', in_len > IOBLK ? in_len - IOBLK : 0);
            rd.i = rd.n; int c; while ((c = getc(rd.fp)) != EOF && c != '\n') { }
            return 1;
        }
        if (!in_fill()) { if (rd.i >= rd.n) return 0; }
    }
}
typedef struct { FILE *fp; unsigned char *b; unsigned n; } outblk;
static outblk wr[40]; static unsigned nwr;
static outblk *out_of(FILE *fp)
{
    for (unsigned i = 0; i < nwr; i++) if (wr[i].fp == fp) return &wr[i];
    if (nwr >= 40) fatal("too many output files");
    wr[nwr].fp = fp; wr[nwr].n = 0; wr[nwr].b = malloc(IOBLK); if (!wr[nwr].b) fatal("out of memory");
    return &wr[nwr++];
}
static void out_flush(outblk *o) { if (o->n && fwrite(o->b, 1, o->n, o->fp) != o->n) fatal("SORTOUT: write failed"); o->n = 0; }
static void out_flush_all(void) { for (unsigned i = 0; i < nwr; i++) out_flush(&wr[i]); }
static void out_put(outblk *o, const unsigned char *p, unsigned n)
{
    if (o->n + n > IOBLK) { out_flush(o); if (n > IOBLK) { if (fwrite(p, 1, n, o->fp) != n) fatal("SORTOUT: write failed"); return; } }
    memcpy(o->b + o->n, p, n); o->n += n;
}
/* a record out: fixed as is; a text line at its own length, or trimmed when it was
 * built by a reformat (exact < 0) */
static void write_record(FILE *fp, const unsigned char *rec, unsigned len, int exact)
{
    outblk *o = out_of(fp);
    if (!rec_text) { out_put(o, rec, len); return; }
    if (exact >= 0) len = (unsigned)exact;
    else while (len && rec[len - 1] == ' ') len--;
    out_put(o, rec, len);
    if (o->n + 1 > IOBLK) out_flush(o);
    o->b[o->n++] = '\n';
}

static void take(const unsigned char *raw, unsigned seq)
{
    n_in++;
    if (!selected(raw)) return;
    n_sel++;
    const unsigned char *rec = raw;
    if (ninrec) { reformat(raw, in_len, recbuf, inrec, ninrec); rec = recbuf; set_line_len(recbuf, rec_len, 0xFFFF); }
    key_build(rec, seq, keybuf);
    xs_put(&xs, keybuf, rec);
}

static unsigned out_len;                   /* after the main OUTREC */
static unsigned long long n_outfil;
static void emit(FILE *out, const unsigned char *rec)
{
    n_out++;
    const unsigned char *r = rec;
    int exact = rec_text ? (int)line_len(rec, rec_len) : -1;      /* the line's own length, 0xFFFF = built, trim */
    if (exact == 0xFFFF) exact = -1;
    if (noutrec) { reformat(rec, rec_len, outbuf, outrec, noutrec); r = outbuf; exact = -1; }
    if (out) write_record(out, r, out_len, exact);
    int taken = 0; outfil *saver = 0;
    for (unsigned i = 0; i < nofs; i++) {
        outfil *o = &ofs[i];
        o->seen++;
        if (o->save) { saver = o; continue; }
        if (o->seen < o->startrec || o->seen > o->endrec) continue;
        if (!selected_by(r, o->conds, o->nconds, o->omit)) continue;
        taken = 1;
        const unsigned char *w = r; unsigned wl = out_len; int ex = exact;
        if (o->nitems) { reformat(r, out_len, o->buf, o->items, o->nitems); w = o->buf; wl = reformat_len(o->items, o->nitems); ex = -1; }
        if (o->split) { write_record(o->fp[o->next], w, wl, ex); if (++o->written % o->split == 0) o->next = (o->next + 1) % o->nnames; }
        else for (unsigned k = 0; k < o->nnames; k++) write_record(o->fp[k], w, wl, ex);
        n_outfil++;
    }
    if (saver && !taken) {
        outfil *o = saver;
        const unsigned char *w = r; unsigned wl = out_len; int ex = exact;
        if (o->nitems) { reformat(r, out_len, o->buf, o->items, o->nitems); w = o->buf; wl = reformat_len(o->items, o->nitems); ex = -1; }
        for (unsigned k = 0; k < o->nnames; k++) write_record(o->fp[k], w, wl, ex);
        n_outfil++;
    }
}

/* ---------------------------------------------------------------- */
/* JOINKEYS: sort each side on its keys, then pair equal-key groups   */

static unsigned jkey_len(const joinside *j) { unsigned k = 4; for (unsigned i = 0; i < j->nkeys; i++) k += j->keys[i].len; return k; }
static void jkey_build(const joinside *j, const unsigned char *rec, unsigned seq, unsigned char *out)
{
    for (unsigned i = 0; i < j->nkeys; i++) {
        const field *f = &j->keys[i];
        memcpy(out, rec + f->pos, f->len);
        if (f->desc) for (unsigned b = 0; b < f->len; b++) out[b] = (unsigned char)~out[b];
        out += f->len;
    }
    out[0] = (unsigned char)(seq >> 24); out[1] = (unsigned char)(seq >> 16); out[2] = (unsigned char)(seq >> 8); out[3] = (unsigned char)seq;
}

/* the F2 group with one key value, held while F1 records of that key pair with it */
static unsigned char *grp; static unsigned grp_n, grp_cap;

static void join_emit(const unsigned char *r1, const unsigned char *r2, char ind, unsigned l1, unsigned l2, unsigned seq_dummy)
{
    (void)seq_dummy;
    static unsigned char *jb; static unsigned jbl;
    unsigned need = 0; for (unsigned i = 0; i < nreform; i++) need += reform[i].len;
    if (jbl < need) { free(jb); jb = malloc(slot_of(need ? need : 1)); jbl = need; if (!jb) fatal("out of memory"); }
    unsigned char *o = jb;
    for (unsigned i = 0; i < nreform; i++) {
        const rfitem *f = &reform[i];
        if (f->side == 0) { o[0] = (unsigned char)ind; }
        else {
            const unsigned char *src = f->side == 1 ? r1 : r2; unsigned sl = f->side == 1 ? l1 : l2;
            for (unsigned k = 0; k < f->len; k++) o[k] = src && f->pos + k < sl ? src[f->pos + k] : join_fill;
        }
        o += f->len;
    }
    static unsigned jseq;
    set_line_len(jb, need, 0xFFFF);
    take(jb, jseq++);
}

static void run_join(void)
{
    const char *p1 = dd_path(jk[0].dd), *p2 = dd_path(jk[1].dd);
    if (!p1 || !p2) fatal("JOINKEYS: F1/F2 data sets not named on the command line (NAME=path)");
    unsigned l1 = jk[0].len ? jk[0].len : in_len, l2 = jk[1].len ? jk[1].len : in_len;
    for (int s = 0; s < 2; s++) for (unsigned i = 0; i < jk[s].nkeys; i++) if (jk[s].keys[i].pos + jk[s].keys[i].len > (s ? l2 : l1)) fatal("a JOINKEYS field lies past the record");
    unsigned k1 = jkey_len(&jk[0]), k2 = jkey_len(&jk[1]);
    if (k1 != k2) fatal("JOINKEYS: F1 and F2 FIELDS must have the same total length");
    xsort x1, x2; char base[600];
    l1 = l1; l2 = l2;
    const char *jb0 = dd_path("SORTOUT") ? dd_path("SORTOUT") : nofs && dd_path(ofs[0].names[0]) ? dd_path(ofs[0].names[0]) : "s32sort";
    snprintf(base, sizeof base, "%s.j1", jb0); xs_init(&x1, k1 + slot_of(l1), k1, mainsize / 2, 32, base, fatal);
    snprintf(base, sizeof base, "%s.j2", jb0); xs_init(&x2, k2 + slot_of(l2), k2, mainsize / 2, 32, base, fatal);
    unsigned char *rb = malloc(slot_of(l1 > l2 ? l1 : l2)), *kb = malloc(k1);
    if (!rb || !kb) fatal("out of memory");
    for (int s = 0; s < 2; s++) {
        const char *path = s ? p2 : p1; unsigned len = s ? l2 : l1; xsort *x = s ? &x2 : &x1; joinside *j = &jk[s];
        FILE *f = fopen(path, "rb"); if (!f) { char m[300]; snprintf(m, sizeof m, "cannot open %s", path); fatal(m); }
        unsigned saved = in_len; in_len = len;
        unsigned seq = 0;
        if (j->sorted) xs_source_begin(x);
        in_open(f);
        while (read_record(f, rb)) { jkey_build(j, rb, seq++, kb); xs_put(x, kb, rb); }
        if (j->sorted) xs_source_end(x);
        in_len = saved; fclose(f);
    }
    xs_finish(&x1); xs_finish(&x2);
    unsigned kc = k1 - 4;                                   /* the key without the arrival number */
    const unsigned char *e2 = xs_next(&x2);
    unsigned char *gkey = malloc(kc ? kc : 1); if (!gkey) fatal("out of memory");
    grp_cap = 64; grp = malloc((size_t)grp_cap * l2); grp_n = 0; if (!grp) fatal("out of memory");   /* the group keeps records without their tag */
    int have_grp = 0, grp_paired = 0;
    const unsigned char *e1;
    while ((e1 = xs_next(&x1)) != 0) {
        /* bring the F2 group up to e1's key, emitting groups that fall below as unpaired F2 */
        for (;;) {
            if (!have_grp) {
                if (!e2) break;
                memcpy(gkey, e2, kc); grp_n = 0;
                while (e2 && !memcmp(e2, gkey, kc)) {
                    if (grp_n == grp_cap) { grp_cap *= 2; unsigned char *ng = realloc(grp, (size_t)grp_cap * l2); if (!ng) fatal("JOINKEYS: out of memory for an F2 key group"); grp = ng; }
                    memcpy(grp + (size_t)grp_n * l2, e2 + k2, l2); grp_n++;
                    e2 = xs_next(&x2);
                }
                have_grp = 1; grp_paired = 0;
            }
            int c = memcmp(gkey, e1, kc);
            if (c < 0) { if (join_unpaired2 && !grp_paired) for (unsigned g = 0; g < grp_n; g++) join_emit(0, grp + (size_t)g * l2, '2', l1, l2, 0); have_grp = 0; continue; }
            break;
        }
        if (have_grp && !memcmp(gkey, e1, kc)) {
            grp_paired = 1;
            if (!join_only) for (unsigned g = 0; g < grp_n; g++) join_emit(e1 + k1, grp + (size_t)g * l2, 'B', l1, l2, 0);
        } else if (join_unpaired1) join_emit(e1 + k1, 0, '1', l1, l2, 0);
    }
    if (join_unpaired2) {
        if (have_grp && !grp_paired) for (unsigned g = 0; g < grp_n; g++) join_emit(0, grp + (size_t)g * l2, '2', l1, l2, 0);
        while (e2) { join_emit(0, e2 + k2, '2', l1, l2, 0); e2 = xs_next(&x2); }
    }
    xs_free(&x1); xs_free(&x2); free(rb); free(kb); free(gkey); free(grp);
}

int main(int argc, char **argv)
{
    const char *sortin[64]; unsigned nin = 0; const char *sortout = 0, *sysin = 0;
    for (int i = 1; i < argc; i++) {
        const char *a = argv[i]; const char *eq = strchr(a, '=');
        if (!eq || eq == a || (size_t)(eq - a) > 31) { fprintf(stderr, "usage: s32sort SORTIN=file [SORTIN=file...] SORTOUT=file [SYSIN=ctl] [MAINSIZE=nM] [NAME=file ...]\n"); return 16; }
        char name[32]; for (int k = 0; k < eq - a; k++) name[k] = (char)toupper((unsigned char)a[k]); name[eq - a] = 0;
        if (!strcmp(name, "MAINSIZE")) { char *e; unsigned long v = strtoul(eq + 1, &e, 10); if (*e == 'K' || *e == 'k') v <<= 10; else if (*e == 'M' || *e == 'm') v <<= 20; mainsize = v; continue; }
        if (!strcmp(name, "SORTIN")) { if (nin < 64) sortin[nin++] = eq + 1; }
        else if (!strcmp(name, "SORTOUT")) sortout = eq + 1;
        else if (!strcmp(name, "SYSIN")) sysin = eq + 1;
        if (ndds < 128) { strcpy(dds[ndds].name, name); dds[ndds].path = eq + 1; ndds++; }
    }

    /* control statements */
    FILE *cf = sysin ? fopen(sysin, "rb") : stdin;
    if (!cf) fatal("cannot open SYSIN");
    ctl_len = (unsigned)fread(ctl, 1, sizeof ctl - 1, cf); ctl[ctl_len] = 0;
    if (sysin) fclose(cf);
    parse_control();
    if (!copy_only && !nsortf) fatal("no SORT or MERGE FIELDS");
    if (have_join) { if (!jk[0].dd || !jk[1].dd) fatal("JOINKEYS needs both F1 and F2"); if (!nreform) fatal("JOINKEYS needs a REFORMAT statement"); if (is_merge) fatal("JOINKEYS with MERGE is not supported"); }
    else if (!nin) fatal("no SORTIN");
    if (!sortout && !nofs) fatal("no SORTOUT and no OUTFIL");
    if (is_merge && nin < 2 && !copy_only) fprintf(stderr, "s32sort: MERGE of one SORTIN\n");

    /* the input record length: RECORD LENGTH, else the first file's size for F (one record), 256 for L */
    if (!in_len && rec_text) {
        /* the longest line across every input; a small extra pass */
        const char *scan[66]; unsigned ns = 0;
        for (unsigned i = 0; i < nin; i++) scan[ns++] = sortin[i];
        if (have_join) { const char *a = dd_path(jk[0].dd), *b2 = dd_path(jk[1].dd); if (a) scan[ns++] = a; if (b2) scan[ns++] = b2; }
        unsigned longest = 0;
        for (unsigned i = 0; i < ns; i++) {
            FILE *f = fopen(scan[i], "rb"); if (!f) { char m[300]; snprintf(m, sizeof m, "cannot open %s", scan[i]); fatal(m); }
            /* in blocks: a getc per character cost more than the sort did */
            static unsigned char *sb; if (!sb) { sb = malloc(IOBLK); if (!sb) fatal("out of memory"); }
            unsigned n = 0; size_t got;
            while ((got = fread(sb, 1, IOBLK, f)) > 0) {
                unsigned char *p = sb, *e = sb + got;
                for (;;) {
                    unsigned char *nl = p; while (nl < e && *nl != '\n') nl++;
                    n += (unsigned)(nl - p);
                    if (nl == e) break;
                    if (n > longest) longest = n; n = 0; p = nl + 1;
                }
            }
            if (n > longest) longest = n;
            fclose(f);
        }
        /* at least what the fields reach (an empty input has no longest line;
         * a CH field may run past the record and is clipped, so its start
         * is what must fit) */
        unsigned need = 1;
        for (unsigned i = 0; i < nsortf; i++) { unsigned e = sortf[i].fmt == F_CH ? sortf[i].pos + 1 : sortf[i].pos + sortf[i].len; if (e > need) need = e; }
        for (unsigned i = 0; i < nsumf; i++) if (sumf[i].pos + sumf[i].len > need) need = sumf[i].pos + sumf[i].len;
        for (unsigned i = 0; i < nconds; i++) { unsigned e = conds[i].a.fmt == F_CH ? conds[i].a.pos + 1 : conds[i].a.pos + conds[i].a.len; if (e > need) need = e; if (conds[i].rhs_is_field) { e = conds[i].b.fmt == F_CH ? conds[i].b.pos + 1 : conds[i].b.pos + conds[i].b.len; if (e > need) need = e; } }
        for (unsigned i = 0; i < ninrec; i++) if (inrec[i].kind == 0 && inrec[i].pos + inrec[i].len > need) need = inrec[i].pos + inrec[i].len;
        in_len = longest > need ? longest : need;
        if (have_join) { if (!jk[0].len) jk[0].len = in_len; if (!jk[1].len) jk[1].len = in_len; }
    }
    if (!in_len) {
        if (have_join) in_len = 256;
        else { FILE *f = fopen(sortin[0], "rb"); if (!f) fatal("cannot open SORTIN"); fseek(f, 0, SEEK_END); long sz = ftell(f); fclose(f); if (sz <= 0) fatal("RECORD LENGTH needed: SORTIN is empty"); in_len = (unsigned)sz; fprintf(stderr, "s32sort: RECORD LENGTH not given; taking the whole first SORTIN as one record (%u bytes)\n", in_len); }
    }
    unsigned side_len = in_len;                              /* the F1/F2 record length when joining */
    if (have_join) { in_len = 0; for (unsigned i = 0; i < nreform; i++) in_len += reform[i].len; }   /* the main task sees REFORMAT records */
    rec_len = ninrec ? reformat_len(inrec, ninrec) : in_len;
    out_len = noutrec ? reformat_len(outrec, noutrec) : rec_len;
    for (unsigned i = 0; i < nofs; i++) {
        outfil *o = &ofs[i];
        for (unsigned c = 0; c < o->nconds; c++) if (o->conds[c].a.pos + o->conds[c].a.len > out_len || (o->conds[c].rhs_is_field && o->conds[c].b.pos + o->conds[c].b.len > out_len)) fatal("an OUTFIL INCLUDE/OMIT field lies past the record");
        o->buf = malloc(o->nitems ? reformat_len(o->items, o->nitems) : 1); if (!o->buf) fatal("out of memory");
        for (unsigned k = 0; k < o->nnames; k++) {
            const char *path = dd_path(o->names[k]);
            if (!path) { char m[100]; snprintf(m, sizeof m, "OUTFIL %s: not named on the command line (%s=path)", o->names[k], o->names[k]); fatal(m); }
            o->fp[k] = fopen(path, rec_text ? "w" : "wb"); if (!o->fp[k]) { char m[300]; snprintf(m, sizeof m, "cannot open %s", path); fatal(m); }
        }
    }
    /* every field must fit; a CH field that runs past the record is clipped to
     * it -- `1,256,CH,A` as a last key means "the rest of the record", whatever
     * the record's length turned out to be */
    for (unsigned i = 0; i < nsortf; i++) if (sortf[i].pos + sortf[i].len > rec_len) {
        if (sortf[i].fmt != F_CH || sortf[i].pos >= rec_len) fatal("a SORT field lies past the record");
        sortf[i].len = rec_len - sortf[i].pos;
    }
    for (unsigned i = 0; i < nsumf; i++) if (sumf[i].pos + sumf[i].len > rec_len) fatal("a SUM field lies past the record");
    for (unsigned i = 0; i < nconds; i++) {
        cond *c = &conds[i];
        if (c->a.pos + c->a.len > in_len) { if (c->a.fmt != F_CH || c->a.pos >= in_len) fatal("an INCLUDE/OMIT field lies past the record"); c->a.len = in_len - c->a.pos; }
        if (c->rhs_is_field && c->b.pos + c->b.len > in_len) { if (c->b.fmt != F_CH || c->b.pos >= in_len) fatal("an INCLUDE/OMIT field lies past the record"); c->b.len = in_len - c->b.pos; }
    }

    inbuf = malloc(slot_of(in_len)); recbuf = malloc(slot_of(rec_len ? rec_len : 1)); outbuf = malloc(noutrec ? reformat_len(outrec, noutrec) : 1);
    klen = key_len(); keybuf = malloc(klen);
    if (!inbuf || !recbuf || !outbuf || !keybuf) fatal("out of memory");
    if (!mainsize) {
        const char *e = getenv("S32_SORT_MEMORY");
        if (e && *e) { char *end; unsigned long v = strtoul(e, &end, 10); if (*end == 'K' || *end == 'k') v <<= 10; else if (*end == 'M' || *end == 'm') v <<= 20; mainsize = v; }
    }
    if (!mainsize) {
#ifdef __slow32__
        extern char __heap_start[];
        extern char __heap_end[];
        mainsize = (size_t)(__heap_end - __heap_start) / 2;
#else
        mainsize = 64u << 20;
#endif
    }
    if (mainsize < 65536) mainsize = 65536;
    /* run files go beside SORTOUT, or beside the first OUTFIL when there is no SORTOUT */
    const char *base = sortout ? sortout : nofs ? dd_path(ofs[0].names[0]) : "s32sort";
    xs_init(&xs, klen + slot_of(rec_len), klen, mainsize, 32, base ? base : "s32sort", fatal);

    /* in */
    int stats = getenv("S32SORT_STATS") != 0; clock_t t0 = clock(), t1, t2, t3;
    unsigned seq = 0;
    if (have_join) {
        unsigned saved = in_len; in_len = side_len; run_join(); in_len = saved;
    } else for (unsigned i = 0; i < nin; i++) {
        FILE *f = fopen(sortin[i], "rb");
        if (!f) { char m[300]; snprintf(m, sizeof m, "cannot open SORTIN %s", sortin[i]); fatal(m); }
        if (is_merge) xs_source_begin(&xs);
        in_open(f);
        while (read_record(f, inbuf)) take(inbuf, seq++);
        if (is_merge) xs_source_end(&xs);
        fclose(f);
    }
    t1 = clock();
    xs_finish(&xs);
    t2 = clock();

    /* out */
    FILE *out = 0;
    if (sortout) { out = fopen(sortout, rec_text ? "w" : "wb"); if (!out) fatal("cannot open SORTOUT"); }
    const unsigned char *e;
    if (!have_sum) {
        while ((e = xs_next(&xs)) != 0) emit(out, e + klen);
    } else {
        /* records equal on the sort fields collapse: the first stays, SUM fields accumulate */
        unsigned char *held = malloc(slot_of(rec_len)), *hk = malloc(klen); int have = 0;
        if (!held || !hk) fatal("out of memory");
        unsigned kcmp = klen - 4;                                       /* the key without the arrival number */
        while ((e = xs_next(&xs)) != 0) {
            if (have && !memcmp(hk, e, kcmp)) {
                for (unsigned i = 0; i < nsumf; i++) enc_num(held, &sumf[i], dec_num(held, &sumf[i]) + dec_num(e + klen, &sumf[i]));
                continue;
            }
            if (have) emit(out, held);
            memcpy(hk, e, klen); memcpy(held, e + klen, slot_of(rec_len)); have = 1;
        }
        if (have) emit(out, held);
    }
    out_flush_all();
    t3 = clock();
    if (stats) fprintf(stderr, "s32sort: read %ld ms, sort %ld ms, write %ld ms (klen %u, slot %u, inmem %d)\n",
        (long)((t1 - t0) * 1000 / CLOCKS_PER_SEC), (long)((t2 - t1) * 1000 / CLOCKS_PER_SEC), (long)((t3 - t2) * 1000 / CLOCKS_PER_SEC), klen, slot_of(rec_len), xs.inmem);
    if (out && fclose(out)) fatal("SORTOUT: close failed");
    for (unsigned i = 0; i < nofs; i++) for (unsigned k = 0; k < ofs[i].nnames; k++) if (fclose(ofs[i].fp[k])) fatal("OUTFIL: close failed");
    xs_free(&xs);
    if (nofs) fprintf(stderr, "s32sort: %llu records in, %llu selected, %llu out, %llu to OUTFIL\n", n_in, n_sel, n_out, n_outfil);
    else fprintf(stderr, "s32sort: %llu records in, %llu selected, %llu out\n", n_in, n_sel, n_out);
    return 0;
}
