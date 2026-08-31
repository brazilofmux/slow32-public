/* libf77.c -- Fortran 77 formatted I/O runtime for SLOW-32.
 *
 * FORMAT is an interpreted mini-language, not something a compiler can
 * expand inline: the edit descriptors are consumed on demand as the I/O
 * list supplies items, repeat counts and nested groups have run-time
 * state, and when the list outlasts the format the format REVERTS --
 * restarting at the last top-level group rather than at the beginning.
 * So the compiler emits a format string plus a sequence of item calls,
 * and this walks the format between them.
 *
 * Numeric rendering defers to snprintf, which on SLOW-32 is David Gay's
 * dtoa; that is what makes F/E/D/G output match a reference Fortran's
 * digits instead of merely being close.
 *
 * Built for SLOW-32 with clang (see build.sh).  fortran/ is in the
 * tree's ordinary universe, so using the host toolchain to produce
 * guest code is the normal arrangement here.
 */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define FIO_MAXFMT   1024
#define FIO_MAXLINE  1024
#define FIO_MAXGROUP 16

static const char *fio_fmt;      /* format text, without outer parens */
static int   fio_pos;            /* cursor into fio_fmt */
static int   fio_unit;
static char  fio_line[FIO_MAXLINE];
static int   fio_len;

/* Repeat state for the descriptor currently being consumed. */
static int   fio_rep;            /* repeats left on the active descriptor */
static int   fio_desc;           /* active descriptor letter, 0 = none */
static int   fio_w, fio_d;       /* width and digits of the active one */

/* Group stack: start position and remaining count of each open ( ). */
static int   fio_gstart[FIO_MAXGROUP];
static int   fio_gcount[FIO_MAXGROUP];
static int   fio_gdepth;
static int   fio_scale;    /* the kP scale factor in force */

/* Where the last TOP-LEVEL group began, for format reversion. */
static int   fio_revert;

static int   fio_listed;         /* 1 = list-directed (FMT=*) */
static int   fio_first_item;

static void fio_putc(int c) {
    if (fio_len < FIO_MAXLINE - 1) fio_line[fio_len++] = (char)c;
}

static void fio_puts(const char *s) {
    while (*s) fio_putc(*s++);
}

static void fio_flush_line(void) {
    fio_line[fio_len] = 0;
    fputs(fio_line, stdout);
    fputc('\n', stdout);
    fio_len = 0;
}

static void fio_pad(int n) { while (n-- > 0) fio_putc(' '); }

/* Right-justify `s` in a field of `w`; a field too narrow is filled
 * with asterisks, which is what Fortran does rather than overflowing. */
static void fio_field(const char *s, int w) {
    int n = (int)strlen(s);
    if (w <= 0) { fio_puts(s); return; }
    if (n > w) { while (w-- > 0) fio_putc('*'); return; }
    fio_pad(w - n);
    fio_puts(s);
}

static int fio_isdigit(int c) { return c >= '0' && c <= '9'; }

static int fio_number(void) {
    int v = 0;
    while (fio_isdigit(fio_fmt[fio_pos]))
        v = v * 10 + (fio_fmt[fio_pos++] - '0');
    return v;
}

/* Walk the format, emitting literal items, until a DATA descriptor is
 * reached.  Returns its letter, or 0 if the format ran out with no data
 * descriptor left (which ends the record). */
static int fio_next_desc(void) {
    int c;
    int n;
    int item_start;
    int nneg;
    int guard = 0;

    for (;;) {
        if (++guard > 100000) return 0;          /* malformed format */
        c = fio_fmt[fio_pos];

        if (c == 0) {
            /* End of format.  If items remain, revert to the last
             * top-level group; otherwise the record is done. */
            if (fio_gdepth > 0) { fio_gdepth = 0; }
            if (fio_revert < 0) return 0;
            fio_pos = fio_revert;
            if (fio_fmt[fio_pos] == 0) return 0;
            fio_flush_line();
            continue;
        }
        if (c == ' ' || c == ',') { fio_pos++; continue; }

        if (c == '/') { fio_pos++; fio_flush_line(); continue; }

        if (c == '\'') {
            fio_pos++;
            while (fio_fmt[fio_pos]) {
                if (fio_fmt[fio_pos] == '\'') {
                    if (fio_fmt[fio_pos + 1] == '\'') { fio_putc('\''); fio_pos += 2; continue; }
                    fio_pos++;
                    break;
                }
                fio_putc(fio_fmt[fio_pos++]);
            }
            continue;
        }

        if (c == ')') {
            fio_pos++;
            if (fio_gdepth > 0) {
                fio_gdepth--;
                if (--fio_gcount[fio_gdepth] > 0) {
                    fio_pos = fio_gstart[fio_gdepth];
                    fio_gdepth++;
                }
            }
            continue;
        }

        item_start = fio_pos;
        nneg = 0;
        if (c == '-' && fio_isdigit(fio_fmt[fio_pos + 1])) { fio_pos++; nneg = 1; c = fio_fmt[fio_pos]; }
        n = fio_isdigit(c) ? fio_number() : 1;
        c = fio_fmt[fio_pos];

        if (c == '(') {
            fio_pos++;
            if (fio_gdepth < FIO_MAXGROUP) {
                fio_gstart[fio_gdepth] = fio_pos;
                fio_gcount[fio_gdepth] = n;
                fio_gdepth++;
                /* Reversion restarts at the last TOP-LEVEL group
                 * INCLUDING its repeat count, so remember where the
                 * count began -- not where the paren is.  Pointing at
                 * the paren silently drops the count, so `2(I2,'-')'
                 * printed one pair per record after reversion instead
                 * of two. */
                if (fio_gdepth == 1) fio_revert = item_start;
            }
            continue;
        }
        if (c == 'X' || c == 'x') { fio_pos++; fio_pad(n); continue; }
        if (c == 'H' || c == 'h') {
            fio_pos++;
            while (n-- > 0 && fio_fmt[fio_pos]) fio_putc(fio_fmt[fio_pos++]);
            continue;
        }
        if (c == 'P' || c == 'p') { fio_pos++; fio_scale = nneg ? -n : n; continue; }   /* kP: sticky until the next P (13.5.9) */

        /* Data descriptors */
        if (c=='I'||c=='i'||c=='F'||c=='f'||c=='E'||c=='e'||
            c=='D'||c=='d'||c=='G'||c=='g'||c=='A'||c=='a'||
            c=='L'||c=='l') {
            fio_pos++;
            fio_w = fio_isdigit(fio_fmt[fio_pos]) ? fio_number() : 0;
            fio_d = 0;
            if (fio_fmt[fio_pos] == '.') { fio_pos++; fio_d = fio_number(); }
            /* Ew.dEe -- the exponent-width suffix is accepted and ignored */
            if (fio_fmt[fio_pos] == 'E' || fio_fmt[fio_pos] == 'e') {
                int save = fio_pos;
                fio_pos++;
                if (fio_isdigit(fio_fmt[fio_pos])) fio_number();
                else fio_pos = save;
            }
            fio_rep = n;
            fio_desc = (c >= 'a' && c <= 'z') ? c - 'a' + 'A' : c;
            return fio_desc;
        }

        fio_pos++;   /* unknown descriptor: skip it rather than hang */
    }
}

/* Obtain the descriptor for the next item, honouring repeat counts. */
static int fio_want(void) {
    if (fio_desc && fio_rep > 0) return fio_desc;
    fio_desc = 0;
    return fio_next_desc();
}

static void fio_consumed(void) {
    if (fio_rep > 0) fio_rep--;
    if (fio_rep == 0) fio_desc = 0;
}

/* Fortran's E output is NOT C's.  C's %E gives `d.dddddd E-07`; the
 * Fortran E descriptor normalises the mantissa to less than one, giving
 * `0.dddddddE-06` -- same value, mantissa shifted one place right and
 * the exponent incremented.  Getting this wrong is invisible on small
 * examples and wrong on every real numeric report, so it is done by
 * rewriting C's output rather than by trying to coax printf into it.
 *
 * `letter` is the exponent letter to emit (E or D).
 */
static void fio_efmt(char *out, int outsz, double v, int nd, int letter) {
    char tmp[64];
    char cf[16];
    char *p;
    char *q;
    int expv;
    int neg;
    int i;
    int n;
    int k;
    int nsig;

    if (nd <= 0) nd = 6;
    k = fio_scale;
    if (k <= -nd || k >= nd + 2) k = 0;             /* out of 13.5.9's range: no effect */
    nsig = k > 0 ? nd + 1 : nd + k;                 /* significant digits shown */
    if (nsig < 1) nsig = 1;
    snprintf(cf, sizeof cf, "%%.%dE", nsig - 1);
    snprintf(tmp, sizeof tmp, cf, v);

    p = tmp;
    neg = 0;
    if (*p == '-') { neg = 1; p++; }
    else if (*p == '+') p++;

    /* Collect the significant digits, dropping the decimal point. */
    n = 0;
    q = out;
    i = 0;
    while (p[i] && p[i] != 'E' && p[i] != 'e') {
        if (p[i] != '.') { tmp[n + 40] = p[i]; n++; }
        i++;
    }
    expv = 0;
    if (p[i] == 'E' || p[i] == 'e') expv = (int)strtol(p + i + 1, 0, 10);
    expv = expv + 1;                  /* mantissa moved below one */

    if (n == 0 || (n == 1 && tmp[40] == '0')) expv = 0;   /* exact zero */

    q = out;
    if (neg) *q++ = '-';
    if (k > 0) {                                    /* k digits ahead of the point */
        for (i = 0; i < k && i < n; i++) *q++ = tmp[40 + i];
        *q++ = '.';
        for (; i < n && (q - out) < outsz - 8; i++) *q++ = tmp[40 + i];
    } else {
        *q++ = '0';
        *q++ = '.';
        for (i = k; i < 0 && (q - out) < outsz - 8; i++) *q++ = '0';   /* |k| leading zeros */
        for (i = 0; i < n && (q - out) < outsz - 8; i++) *q++ = tmp[40 + i];
    }
    expv = expv - k;
    *q++ = (char)letter;
    if (expv < 0) { *q++ = '-'; expv = -expv; }
    else *q++ = '+';
    if (expv >= 100) {
        *q++ = (char)('0' + expv / 100);
        *q++ = (char)('0' + (expv / 10) % 10);
        *q++ = (char)('0' + expv % 10);
    } else {
        *q++ = (char)('0' + (expv / 10) % 10);
        *q++ = (char)('0' + expv % 10);
    }
    *q = 0;
}

/* --- entry points called by compiled Fortran ------------------------ */

void f77_wr_begin(int unit, const char *fmt) {
    if (unit != 6 && unit != 5 && unit != 0) {
        fprintf(stderr, "f77: WRITE to unit %d is not supported\n", unit);
        exit(2);
    }
    fio_unit = unit;
    fio_len = 0;
    fio_gdepth = 0;
    fio_scale = 0;
    fio_rep = 0;
    fio_desc = 0;
    fio_first_item = 1;
    if (fmt == 0) {
        fio_listed = 1;
        fio_fmt = "";
        fio_pos = 0;
        fio_revert = -1;
        return;
    }
    fio_listed = 0;
    fio_fmt = fmt;
    fio_pos = 0;
    fio_revert = -1;
    /* Skip the outer parenthesis; it is the reversion point. */
    while (fio_fmt[fio_pos] == ' ') fio_pos++;
    if (fio_fmt[fio_pos] == '(') { fio_revert = fio_pos; fio_pos++; }
}

void f77_wr_i(int v) {
    char buf[32];
    int d;
    if (fio_listed) {
        snprintf(buf, sizeof buf, "%d", v);
        fio_pad(fio_first_item ? 1 : 1);
        fio_puts(buf);
        fio_first_item = 0;
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (d == 'L') {
        if (fio_w > 1) fio_pad(fio_w - 1);
        fio_putc(v ? 'T' : 'F');
        fio_consumed();
        return;
    }
    snprintf(buf, sizeof buf, "%d", v);
    fio_field(buf, fio_w);
    fio_consumed();
}

void f77_wr_d(double v) {
    char buf[64];
    char cf[16];
    int d;
    if (fio_listed) {
        snprintf(buf, sizeof buf, "%g", v);
        fio_putc(' ');
        fio_puts(buf);
        fio_first_item = 0;
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (d == 'I') {
        snprintf(buf, sizeof buf, "%d", (int)v);
    } else if (d == 'F') {
        double fv = v;
        int k = fio_scale;
        while (k > 0) { fv *= 10.0; k--; }
        while (k < 0) { fv /= 10.0; k++; }
        snprintf(cf, sizeof cf, "%%.%df", fio_d);
        snprintf(buf, sizeof buf, cf, fv);
    } else if (d == 'G') {
        /* Gw.d: F form when 0.1 <= |v| < 10^d, else E. The F form
         * occupies w-4 columns and is followed by four blanks. */
        double a = v < 0 ? -v : v;
        double lim = 1.0;
        int i;
        int use_f = 0;
        int nd = fio_d > 0 ? fio_d : 1;
        for (i = 0; i < nd; i++) lim *= 10.0;
        if (a == 0.0 || (a >= 0.1 && a < lim)) use_f = 1;
        if (use_f) {
            int fw = fio_w >= 4 ? fio_w - 4 : fio_w;
            int k = 0;
            double t = a;
            if (t >= 1.0) {
                while (t >= 10.0 && k < nd) { t /= 10.0; k++; }
                k++;
            }
            snprintf(cf, sizeof cf, "%%.%df", nd - k > 0 ? nd - k : 0);
            snprintf(buf, sizeof buf, cf, v);
            fio_field(buf, fw);
            fio_pad(fio_w >= 4 ? 4 : 0);
            fio_consumed();
            return;
        }
        fio_efmt(buf, sizeof buf, v, nd, 'E');
    } else if (d == 'E' || d == 'D') {
        fio_efmt(buf, sizeof buf, v, fio_d, d);
    } else {
        snprintf(buf, sizeof buf, "%g", v);
    }
    fio_field(buf, fio_w);
    fio_consumed();
}

void f77_wr_r(float v) { f77_wr_d((double)v); }

void f77_wr_a(const char *s, int len) {
    int d;
    int i;
    if (fio_listed) {
        fio_putc(' ');
        for (i = 0; i < len; i++) fio_putc(s[i]);
        fio_first_item = 0;
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (fio_w > 0 && fio_w < len) len = fio_w;
    else if (fio_w > len) fio_pad(fio_w - len);
    for (i = 0; i < len; i++) fio_putc(s[i]);
    fio_consumed();
}

void f77_wr_l(int v) {
    int d;
    d = fio_want();
    if (d == 0) { return; }
    if (fio_w > 1) fio_pad(fio_w - 1);
    fio_putc(v ? 'T' : 'F');
    fio_consumed();
}

/* Finish the record: emit any trailing literal text, but stop at the
 * first data descriptor that has no item to feed it. */
void f77_wr_end(void) {
    if (!fio_listed) {
        int save_revert = fio_revert;
        fio_revert = -1;              /* no reversion while draining */
        fio_desc = 0;
        fio_rep = 0;
        fio_next_desc();
        fio_revert = save_revert;
    }
    fio_flush_line();
}
