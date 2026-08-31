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

/* Input state.  The format walker is shared between WRITE and READ;
 * fio_reading flips the direction of the record-affecting descriptors
 * (literals, X, /, end-of-format reversion). */
static int   fio_reading;        /* 1 = READ in progress */
static char  fio_rec[FIO_MAXLINE];  /* current input record */
static int   fio_rlen;
static int   fio_rpos;
static int   fio_ldone;          /* list-directed: '/' seen, rest untouched */
static int   fio_l_pending;      /* list-directed r*c: repeats left on fio_ltok */
static char  fio_ltok[128];

/* Open units.  0/5/6 are preconnected (stderr/stdin/stdout, matching
 * gfortran) and may not be OPENed; everything else goes through the
 * table.  The name is kept for CLOSE (STATUS='DELETE'). */
#define FIO_MAXUNIT 32
#define FIO_MAXNAME 256
static FILE *fio_ufile[FIO_MAXUNIT];
static char  fio_ufname[FIO_MAXUNIT][FIO_MAXNAME];
static FILE *fio_in;             /* stream of the READ in progress */
static FILE *fio_out;            /* stream of the WRITE in progress */

static FILE *fio_resolve(int unit, int writing) {
    if (unit >= 0 && unit < FIO_MAXUNIT && fio_ufile[unit]) return fio_ufile[unit];
    if (writing) {
        if (unit == 6) return stdout;
        if (unit == 0) return stderr;
        if (unit == 5) return stdout;    /* tolerated historically */
    } else {
        if (unit == 5 || unit == 0) return stdin;
    }
    fprintf(stderr, "f77: unit %d is not open for %s\n",
            unit, writing ? "WRITE" : "READ");
    exit(2);
    return 0;
}

/* Case-insensitive match of a counted, possibly blank-padded Fortran
 * string against a keyword. */
static int fio_str_is(const char *s, int len, const char *kw) {
    int i;
    for (i = 0; i < len && kw[i]; i++) {
        int c = s[i];
        if (c >= 'a' && c <= 'z') c = c - 'a' + 'A';
        if (c != kw[i]) return 0;
    }
    if (kw[i]) return 0;
    for (; i < len; i++) if (s[i] != ' ') return 0;
    return 1;
}

static void fio_putc(int c) {
    if (fio_len < FIO_MAXLINE - 1) fio_line[fio_len++] = (char)c;
}

static void fio_puts(const char *s) {
    while (*s) fio_putc(*s++);
}

static void fio_flush_line(void) {
    FILE *f = fio_out ? fio_out : stdout;
    fio_line[fio_len] = 0;
    fputs(fio_line, f);
    fputc('\n', f);
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

/* Fetch the next input record.  Returns 1 at end of file.  A record
 * shorter than the fields read from it behaves as if blank-padded,
 * which fio_field_in provides by supplying blanks past fio_rlen. */
static int fio_getrec(void) {
    if (!fgets(fio_rec, FIO_MAXLINE, fio_in ? fio_in : stdin)) return 1;
    fio_rlen = (int)strlen(fio_rec);
    while (fio_rlen > 0 &&
           (fio_rec[fio_rlen - 1] == '\n' || fio_rec[fio_rlen - 1] == '\r'))
        fio_rlen--;
    fio_rpos = 0;
    return 0;
}

/* Mid-statement end of file is an error: END= is checked only when the
 * READ begins, which covers the read-until-EOF idiom. */
static void fio_next_record(void) {
    if (fio_getrec()) {
        fprintf(stderr, "f77: end of file on READ\n");
        exit(2);
    }
}

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
             * top-level group; otherwise the record is done.  On input,
             * a new record is taken where output would start a new
             * line. */
            if (fio_gdepth > 0) { fio_gdepth = 0; }
            if (fio_revert < 0) return 0;
            fio_pos = fio_revert;
            if (fio_fmt[fio_pos] == 0) return 0;
            if (fio_reading) fio_next_record();
            else fio_flush_line();
            continue;
        }
        if (c == ' ' || c == ',') { fio_pos++; continue; }

        if (c == '/') {
            fio_pos++;
            if (fio_reading) fio_next_record();
            else fio_flush_line();
            continue;
        }

        if (c == '\'') {
            /* Apostrophe editing is output-only in F77; on input the
             * text is skipped rather than transferred. */
            fio_pos++;
            while (fio_fmt[fio_pos]) {
                if (fio_fmt[fio_pos] == '\'') {
                    if (fio_fmt[fio_pos + 1] == '\'') {
                        if (!fio_reading) fio_putc('\'');
                        fio_pos += 2;
                        continue;
                    }
                    fio_pos++;
                    break;
                }
                if (!fio_reading) fio_putc(fio_fmt[fio_pos]);
                fio_pos++;
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
        if (c == 'X' || c == 'x') {
            fio_pos++;
            if (fio_reading) fio_rpos += n;   /* skip columns */
            else fio_pad(n);
            continue;
        }
        if (c == 'H' || c == 'h') {
            fio_pos++;
            while (n-- > 0 && fio_fmt[fio_pos]) {
                if (!fio_reading) fio_putc(fio_fmt[fio_pos]);
                fio_pos++;
            }
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
    fio_out = fio_resolve(unit, 1);
    fio_reading = 0;
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

/* --- formatted input ------------------------------------------------- */

/* Copy the next `w` columns of the record into `buf` with every blank
 * squeezed out (F77's default BLANK='NULL' for preconnected units: a
 * blank is no character at all, and an all-blank field is zero).
 * Columns past the end of the record read as blanks. */
static void fio_field_in(char *buf, int w) {
    int i;
    int c;
    int n = 0;
    for (i = 0; i < w; i++) {
        c = (fio_rpos + i < fio_rlen) ? fio_rec[fio_rpos + i] : ' ';
        if (c != ' ' && c != '\t' && n < FIO_MAXLINE - 1) buf[n++] = (char)c;
    }
    buf[n] = 0;
    fio_rpos += w;
}

static int fio_in_int(int w) {
    char buf[FIO_MAXLINE];
    fio_field_in(buf, w);
    if (!buf[0]) return 0;
    return (int)strtol(buf, 0, 10);
}

static int fio_in_logical(int w) {
    char buf[FIO_MAXLINE];
    int i = 0;
    fio_field_in(buf, w);
    if (buf[i] == '.') i++;
    if (buf[i] == 'T' || buf[i] == 't') return 1;
    if (buf[i] == 'F' || buf[i] == 'f') return 0;
    if (buf[i] == 0) return 0;             /* all-blank field */
    fprintf(stderr, "f77: bad LOGICAL input field '%s'\n", buf);
    exit(2);
    return 0;   /* not reached; -fno-builtin hides exit's noreturn */
}

/* F/E/D/G input.  The field is normalised into one string that strtod
 * parses in a single call -- building the string rather than scaling
 * the value keeps the conversion correctly rounded, which is what lets
 * the digits match a reference Fortran after formatting.
 *
 * Rules folded in: D/Q exponent letters mean E; a bare +/- after the
 * mantissa starts an exponent ('1.5+3'); a field with no decimal point
 * has its rightmost `d` mantissa digits taken as fractional; the kP
 * scale factor divides by 10^k, but only when the field itself has no
 * exponent. */
static double fio_in_real(int w, int d) {
    char raw[FIO_MAXLINE];
    char mant[FIO_MAXLINE];     /* mantissa digits only, no sign, no point */
    char num[FIO_MAXLINE + 16];
    int i;
    int n;
    int neg = 0;
    int has_point = 0;
    int has_exp = 0;
    int pfrac = 0;              /* mantissa digits after the point so far */
    int expv = 0;
    int esign = 1;
    int intlen;
    int k;

    fio_field_in(raw, w);
    if (!raw[0]) return 0.0;

    i = 0;
    if (raw[i] == '+') i++;
    else if (raw[i] == '-') { neg = 1; i++; }

    n = 0;
    for (; raw[i] && !has_exp; i++) {
        int c = raw[i];
        if (c == '.') { has_point = 1; continue; }
        if (fio_isdigit(c)) {
            if (n < FIO_MAXLINE - 1) mant[n++] = (char)c;
            if (has_point) pfrac++;
            continue;
        }
        if (c == 'E' || c == 'e' || c == 'D' || c == 'd' ||
            c == 'Q' || c == 'q') {
            has_exp = 1;
            if (raw[i + 1] == '+') i++;
            else if (raw[i + 1] == '-') { esign = -1; i++; }
            continue;
        }
        if (c == '+' || c == '-') {         /* bare signed exponent */
            has_exp = 1;
            if (c == '-') esign = -1;
            continue;
        }
        break;                              /* junk: stop, keep what parsed */
    }
    if (has_exp) {
        for (; raw[i]; i++)
            if (fio_isdigit(raw[i])) expv = expv * 10 + (raw[i] - '0');
        expv = expv * esign;
    }
    mant[n] = 0;
    if (n == 0) return 0.0;

    /* Where does the point go?  Explicit point: pfrac digits are
     * fractional.  No point: the rightmost d are. */
    intlen = has_point ? n - pfrac : n - d;
    if (!has_exp) expv = expv - fio_scale;

    k = 0;
    if (neg) num[k++] = '-';
    if (intlen <= 0) {
        num[k++] = '0';
        num[k++] = '.';
        for (i = intlen; i < 0; i++) num[k++] = '0';
        for (i = 0; i < n; i++) num[k++] = mant[i];
    } else {
        for (i = 0; i < intlen && i < n; i++) num[k++] = mant[i];
        num[k++] = '.';
        for (; i < n; i++) num[k++] = mant[i];
    }
    snprintf(num + k, sizeof num - k, "E%d", expv);
    return strtod(num, 0);
}

/* --- list-directed input --------------------------------------------- */

/* Next list-directed value into fio_ltok.  Returns 0 with the token
 * filled, or 1 for the `/` terminator.  Handles r*c repeats and value
 * lists spanning records.  Null values (`1,,3`) are refused honestly
 * rather than mis-assigned. */
static int fio_list_tok(void) {
    int c;
    int n;
    int comma_seen = 0;

    if (fio_l_pending > 0) { fio_l_pending--; return 0; }

    for (;;) {
        if (fio_rpos >= fio_rlen) { fio_next_record(); comma_seen = 0; continue; }
        c = fio_rec[fio_rpos];
        if (c == ' ' || c == '\t') { fio_rpos++; continue; }
        if (c == ',') {
            if (comma_seen) {
                fprintf(stderr, "f77: null value in list-directed input is not supported\n");
                exit(2);
            }
            comma_seen = 1;
            fio_rpos++;
            continue;
        }
        break;
    }
    if (c == '/') { fio_rpos++; return 1; }

    n = 0;
    while (fio_rpos < fio_rlen) {
        c = fio_rec[fio_rpos];
        if (c == ' ' || c == '\t' || c == ',' || c == '/') break;
        if (n < (int)sizeof fio_ltok - 1) fio_ltok[n++] = (char)c;
        fio_rpos++;
    }
    fio_ltok[n] = 0;

    /* r*value: unsigned repeat count, a star, then the value. */
    n = 0;
    while (fio_isdigit(fio_ltok[n])) n++;
    if (n > 0 && fio_ltok[n] == '*') {
        int r = (int)strtol(fio_ltok, 0, 10);
        if (r > 1) fio_l_pending = r - 1;
        memmove(fio_ltok, fio_ltok + n + 1, strlen(fio_ltok + n + 1) + 1);
    }
    return 0;
}

static double fio_list_real(void) {
    char num[sizeof fio_ltok];
    int i;
    for (i = 0; fio_ltok[i]; i++) {
        int c = fio_ltok[i];
        if (c == 'D' || c == 'd' || c == 'Q' || c == 'q') c = 'E';
        num[i] = (char)c;
    }
    num[i] = 0;
    return strtod(num, 0);
}

/* --- READ entry points ------------------------------------------------ */

/* Begin a READ.  Returns 1 at end of file when the statement carries
 * END= (has_end); without END=, end of file is fatal here.  The first
 * record is fetched now, which is what makes the read-until-EOF idiom
 * (`READ (5, *, END=99) X` in a loop) work; end of file in the middle
 * of a statement is always fatal (see fio_next_record). */
int f77_rd_begin(int unit, const char *fmt, int has_end) {
    fio_in = fio_resolve(unit, 0);
    fio_reading = 1;
    fio_ldone = 0;
    fio_l_pending = 0;
    fio_gdepth = 0;
    fio_scale = 0;
    fio_rep = 0;
    fio_desc = 0;
    fio_len = 0;
    fio_listed = (fmt == 0);
    fio_fmt = fmt ? fmt : "";
    fio_pos = 0;
    fio_revert = -1;
    if (!fio_listed) {
        while (fio_fmt[fio_pos] == ' ') fio_pos++;
        if (fio_fmt[fio_pos] == '(') { fio_revert = fio_pos; fio_pos++; }
    }
    if (fio_getrec()) {
        fio_reading = 0;
        if (has_end) return 1;
        fprintf(stderr, "f77: end of file on READ\n");
        exit(2);
    }
    return 0;
}

/* INTEGER and LOGICAL targets share TY_INT in the compiler, so both
 * arrive here; the format descriptor (or the token's first character,
 * list-directed) says which conversion applies. */
void f77_rd_i(int *p) {
    int d;
    if (fio_listed) {
        int c0;
        if (fio_ldone) return;
        if (fio_list_tok()) { fio_ldone = 1; return; }
        c0 = fio_ltok[0];
        if (c0 == '.' || c0 == 'T' || c0 == 't' || c0 == 'F' || c0 == 'f') {
            int i = (c0 == '.') ? 1 : 0;
            *p = (fio_ltok[i] == 'T' || fio_ltok[i] == 't') ? 1 : 0;
            return;
        }
        *p = (int)strtol(fio_ltok, 0, 10);
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (d == 'L') *p = fio_in_logical(fio_w);
    else if (d == 'I') *p = fio_in_int(fio_w);
    else if (d == 'A') {
        fprintf(stderr, "f77: A editing on READ needs CHARACTER (not implemented)\n");
        exit(2);
    }
    else *p = (int)fio_in_real(fio_w, fio_d);
    fio_consumed();
}

void f77_rd_d(double *p) {
    int d;
    if (fio_listed) {
        if (fio_ldone) return;
        if (fio_list_tok()) { fio_ldone = 1; return; }
        *p = fio_list_real();
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (d == 'I') *p = (double)fio_in_int(fio_w);
    else if (d == 'A' || d == 'L') {
        fprintf(stderr, "f77: %c editing does not match a numeric READ item\n", d);
        exit(2);
    }
    else *p = fio_in_real(fio_w, fio_d);
    fio_consumed();
}

void f77_rd_r(float *p) {
    int d;
    if (fio_listed) {
        if (fio_ldone) return;
        if (fio_list_tok()) { fio_ldone = 1; return; }
        *p = (float)fio_list_real();
        return;
    }
    d = fio_want();
    if (d == 0) return;
    if (d == 'I') *p = (float)fio_in_int(fio_w);
    else if (d == 'A' || d == 'L') {
        fprintf(stderr, "f77: %c editing does not match a numeric READ item\n", d);
        exit(2);
    }
    else *p = (float)fio_in_real(fio_w, fio_d);
    fio_consumed();
}

void f77_rd_end(void) {
    fio_reading = 0;
}

/* --- ** with an INTEGER exponent -------------------------------------- */

/* Binary exponentiation, the same shape gcc's __builtin_powi uses, so
 * rounding of the multiply chain matches a reference compiler's.  A
 * negative INTEGER exponent follows F77 integer division: 1/I**n,
 * which is 0 for |I| > 1.  Real bases take the reciprocal at the end
 * (one rounding, like powi). */
int f77_ipow(int a, int n) {
    int r;
    if (n < 0) {
        if (a == 1) return 1;
        if (a == -1) return (n & 1) ? -1 : 1;
        return 0;               /* |a| > 1 truncates; 0**negative is 0 */
    }
    r = 1;
    while (n > 0) {
        if (n & 1) r = r * a;
        a = a * a;
        n = n >> 1;
    }
    return r;
}

double f77_dpow_i(double a, int n) {
    double r = 1.0;
    int neg = 0;
    unsigned int m;
    if (n < 0) { neg = 1; m = (unsigned int)0 - (unsigned int)n; }
    else m = (unsigned int)n;
    while (m) {
        if (m & 1u) r = r * a;
        a = a * a;
        m = m >> 1;
    }
    return neg ? 1.0 / r : r;
}

float f77_rpow_i(float a, int n) {
    float r = 1.0f;
    int neg = 0;
    unsigned int m;
    if (n < 0) { neg = 1; m = (unsigned int)0 - (unsigned int)n; }
    else m = (unsigned int)n;
    while (m) {
        if (m & 1u) r = r * a;
        a = a * a;
        m = m >> 1;
    }
    return neg ? 1.0f / r : r;
}

/* --- OPEN / CLOSE / REWIND -------------------------------------------- */

/* OPEN (u, FILE='name' [, STATUS='OLD'|'NEW'|'UNKNOWN']).
 * Sequential formatted access, the only kind this runtime does.
 *   OLD:     the file must exist ("r+", read-only "r" as fallback).
 *   NEW:     the file must NOT exist; created "w+".
 *   UNKNOWN: OLD if it exists (no truncation!), otherwise NEW.
 * An OPEN on an already-open unit closes it first. */
void f77_open(int unit, const char *name, int nlen,
              const char *status, int slen) {
    FILE *f;
    char nm[FIO_MAXNAME];

    if (unit <= 0 || unit >= FIO_MAXUNIT || unit == 5 || unit == 6) {
        fprintf(stderr, "f77: cannot OPEN unit %d\n", unit);
        exit(2);
    }
    if (nlen <= 0 || nlen >= FIO_MAXNAME) {
        fprintf(stderr, "f77: bad FILE= name in OPEN\n");
        exit(2);
    }
    memcpy(nm, name, nlen);
    nm[nlen] = 0;
    while (nlen > 0 && nm[nlen - 1] == ' ') nm[--nlen] = 0;   /* Fortran pads */

    if (fio_ufile[unit]) { fclose(fio_ufile[unit]); fio_ufile[unit] = 0; }

    if (status && slen > 0 && fio_str_is(status, slen, "OLD")) {
        f = fopen(nm, "r+");
        if (!f) f = fopen(nm, "r");
        if (!f) {
            fprintf(stderr, "f77: OPEN STATUS='OLD': no such file '%s'\n", nm);
            exit(2);
        }
    } else if (status && slen > 0 && fio_str_is(status, slen, "NEW")) {
        f = fopen(nm, "r");
        if (f) {
            fclose(f);
            fprintf(stderr, "f77: OPEN STATUS='NEW': '%s' already exists\n", nm);
            exit(2);
        }
        f = fopen(nm, "w+");
        if (!f) {
            fprintf(stderr, "f77: OPEN cannot create '%s'\n", nm);
            exit(2);
        }
    } else if (!status || slen <= 0 ||
               fio_str_is(status, slen, "UNKNOWN")) {
        f = fopen(nm, "r+");
        if (!f) f = fopen(nm, "w+");
        if (!f) {
            fprintf(stderr, "f77: OPEN cannot open '%s'\n", nm);
            exit(2);
        }
    } else {
        fprintf(stderr, "f77: OPEN STATUS value is not supported\n");
        exit(2);
    }
    fio_ufile[unit] = f;
    strcpy(fio_ufname[unit], nm);
}

/* CLOSE (u [, STATUS='KEEP'|'DELETE']).  Closing a unit that is not
 * open is permitted and does nothing, as the standard says. */
void f77_close(int unit, const char *status, int slen) {
    int del;
    if (unit < 0 || unit >= FIO_MAXUNIT || !fio_ufile[unit]) return;
    del = (status && slen > 0 && fio_str_is(status, slen, "DELETE"));
    if (status && slen > 0 && !del && !fio_str_is(status, slen, "KEEP")) {
        fprintf(stderr, "f77: CLOSE STATUS value is not supported\n");
        exit(2);
    }
    fclose(fio_ufile[unit]);
    fio_ufile[unit] = 0;
    if (del) remove(fio_ufname[unit]);
    fio_ufname[unit][0] = 0;
}

void f77_rewind(int unit) {
    if (unit < 0 || unit >= FIO_MAXUNIT || !fio_ufile[unit]) {
        fprintf(stderr, "f77: REWIND: unit %d is not open\n", unit);
        exit(2);
    }
    fseek(fio_ufile[unit], 0L, SEEK_SET);
}
