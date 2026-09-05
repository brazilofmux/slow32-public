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
 *                                         L: text lines, padded with blanks to n
 *                                         for the sort, trailing blanks trimmed out
 *   INCLUDE COND=(p,l,f,op,{p,l,f|C'..'|X'..'|[+-]n}[,{AND|OR},...])
 *   OMIT    COND=(...)                    op: EQ NE GT GE LT LE
 *   SUM     FIELDS=(p,l,f,...) | NONE     records equal on the sort fields collapse
 *                                         into one, these fields summed (NONE: kept)
 *   INREC   FIELDS=(item,...)             reformat before the sort
 *   OUTREC  FIELDS=(item,...)             reformat after; item = p,l | nX | C'..' | X'..'
 *   OPTION  ...                           EQUALS is always on; the rest is accepted
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
static size_t mainsize;

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

static void parse_cond(void)
{
    expect('(');
    for (;;) {
        if (nconds >= MAXF) syntax("too many conditions");
        cond *c = &conds[nconds]; memset(c, 0, sizeof *c);
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
        nconds++;
        skipws();
        if (*cp == ')') { cp++; break; }
        expect(',');
        if (!word(w, sizeof w)) syntax("expected AND or OR");
        if (!strcmp(w, "AND")) c->conn = 1; else if (!strcmp(w, "OR")) c->conn = 2; else syntax("expected AND or OR");
        expect(',');
    }
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
        } else if (!strcmp(w, "END")) { break; }
        else syntax("unknown statement");
    }
}

/* ---------------------------------------------------------------- */
/* records in and out                                                */

static unsigned char *inbuf, *recbuf, *keybuf, *outbuf;
static unsigned long long n_in, n_sel, n_out;
static xsort xs; static unsigned klen;

/* one raw record from a fixed or line file; 0 at end */
static int read_record(FILE *fp, unsigned char *buf)
{
    if (!rec_text) return fread(buf, 1, in_len, fp) == in_len;
    int c; unsigned n = 0;
    if ((c = getc(fp)) == EOF) return 0;
    for (; c != EOF && c != '\n'; c = getc(fp)) if (n < in_len) buf[n++] = (unsigned char)c;
    if (n && buf[n - 1] == '\r') n--;
    memset(buf + n, ' ', in_len - n);
    return 1;
}
static void write_record(FILE *fp, const unsigned char *rec, unsigned len)
{
    if (!rec_text) { if (fwrite(rec, 1, len, fp) != len) fatal("SORTOUT: write failed"); return; }
    while (len && rec[len - 1] == ' ') len--;
    if ((len && fwrite(rec, 1, len, fp) != len) || putc('\n', fp) == EOF) fatal("SORTOUT: write failed");
}

static void take(const unsigned char *raw, unsigned seq)
{
    n_in++;
    if (!selected(raw)) return;
    n_sel++;
    const unsigned char *rec = raw;
    if (ninrec) { reformat(raw, in_len, recbuf, inrec, ninrec); rec = recbuf; }
    key_build(rec, seq, keybuf);
    xs_put(&xs, keybuf, rec);
}

static void emit(FILE *out, const unsigned char *rec)
{
    n_out++;
    if (noutrec) { reformat(rec, rec_len, outbuf, outrec, noutrec); write_record(out, outbuf, reformat_len(outrec, noutrec)); }
    else write_record(out, rec, rec_len);
}

int main(int argc, char **argv)
{
    const char *sortin[64]; unsigned nin = 0; const char *sortout = 0, *sysin = 0;
    for (int i = 1; i < argc; i++) {
        const char *a = argv[i];
        if (!strncmp(a, "SORTIN=", 7) || !strncmp(a, "sortin=", 7)) { if (nin < 64) sortin[nin++] = a + 7; }
        else if (!strncmp(a, "SORTOUT=", 8) || !strncmp(a, "sortout=", 8)) sortout = a + 8;
        else if (!strncmp(a, "SYSIN=", 6) || !strncmp(a, "sysin=", 6)) sysin = a + 6;
        else if (!strncmp(a, "MAINSIZE=", 9)) { char *e; unsigned long v = strtoul(a + 9, &e, 10); if (*e == 'K' || *e == 'k') v <<= 10; else if (*e == 'M' || *e == 'm') v <<= 20; mainsize = v; }
        else { fprintf(stderr, "usage: s32sort SORTIN=file [SORTIN=file...] SORTOUT=file [SYSIN=ctl] [MAINSIZE=nM]\n"); return 16; }
    }
    if (!nin) fatal("no SORTIN");
    if (!sortout) fatal("no SORTOUT");

    /* control statements */
    FILE *cf = sysin ? fopen(sysin, "rb") : stdin;
    if (!cf) fatal("cannot open SYSIN");
    ctl_len = (unsigned)fread(ctl, 1, sizeof ctl - 1, cf); ctl[ctl_len] = 0;
    if (sysin) fclose(cf);
    parse_control();
    if (!copy_only && !nsortf) fatal("no SORT or MERGE FIELDS");
    if (is_merge && nin < 2 && !copy_only) fprintf(stderr, "s32sort: MERGE of one SORTIN\n");

    /* the input record length: RECORD LENGTH, else the first file's size for F (one record), 256 for L */
    if (!in_len) {
        if (rec_text) in_len = 256;
        else { FILE *f = fopen(sortin[0], "rb"); if (!f) fatal("cannot open SORTIN"); fseek(f, 0, SEEK_END); long sz = ftell(f); fclose(f); if (sz <= 0) fatal("RECORD LENGTH needed: SORTIN is empty"); in_len = (unsigned)sz; fprintf(stderr, "s32sort: RECORD LENGTH not given; taking the whole first SORTIN as one record (%u bytes)\n", in_len); }
    }
    rec_len = ninrec ? reformat_len(inrec, ninrec) : in_len;
    /* every field must fit */
    for (unsigned i = 0; i < nsortf; i++) if (sortf[i].pos + sortf[i].len > rec_len) fatal("a SORT field lies past the record");
    for (unsigned i = 0; i < nsumf; i++) if (sumf[i].pos + sumf[i].len > rec_len) fatal("a SUM field lies past the record");
    for (unsigned i = 0; i < nconds; i++) { if (conds[i].a.pos + conds[i].a.len > in_len || (conds[i].rhs_is_field && conds[i].b.pos + conds[i].b.len > in_len)) fatal("an INCLUDE/OMIT field lies past the record"); }

    inbuf = malloc(in_len); recbuf = malloc(rec_len ? rec_len : 1); outbuf = malloc(noutrec ? reformat_len(outrec, noutrec) : 1);
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
    xs_init(&xs, klen + rec_len, klen, mainsize, 32, sortout, fatal);

    /* in */
    unsigned seq = 0;
    for (unsigned i = 0; i < nin; i++) {
        FILE *f = fopen(sortin[i], "rb");
        if (!f) { char m[300]; snprintf(m, sizeof m, "cannot open SORTIN %s", sortin[i]); fatal(m); }
        if (is_merge) xs_source_begin(&xs);
        while (read_record(f, inbuf)) take(inbuf, seq++);
        if (is_merge) xs_source_end(&xs);
        fclose(f);
    }
    xs_finish(&xs);

    /* out */
    FILE *out = fopen(sortout, rec_text ? "w" : "wb");
    if (!out) fatal("cannot open SORTOUT");
    const unsigned char *e;
    if (!have_sum) {
        while ((e = xs_next(&xs)) != 0) emit(out, e + klen);
    } else {
        /* records equal on the sort fields collapse: the first stays, SUM fields accumulate */
        unsigned char *held = malloc(rec_len), *hk = malloc(klen); int have = 0;
        if (!held || !hk) fatal("out of memory");
        unsigned kcmp = klen - 4;                                       /* the key without the arrival number */
        while ((e = xs_next(&xs)) != 0) {
            if (have && !memcmp(hk, e, kcmp)) {
                for (unsigned i = 0; i < nsumf; i++) enc_num(held, &sumf[i], dec_num(held, &sumf[i]) + dec_num(e + klen, &sumf[i]));
                continue;
            }
            if (have) emit(out, held);
            memcpy(hk, e, klen); memcpy(held, e + klen, rec_len); have = 1;
        }
        if (have) emit(out, held);
    }
    if (fclose(out)) fatal("SORTOUT: close failed");
    xs_free(&xs);
    fprintf(stderr, "s32sort: %llu records in, %llu selected, %llu out\n", n_in, n_sel, n_out);
    return 0;
}
