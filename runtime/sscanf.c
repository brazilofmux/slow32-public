/* The scanf family (C11 7.21.6.2, 7.21.6.4, 7.21.6.7, 7.21.6.9, 7.21.6.11,
 * 7.21.6.12): one scanner over a source that is a string or a stream.
 *
 * Both C libraries build this file (the clang runtime and the self-hosted
 * one), so it is plain C that stage08 compiles.
 *
 * The scanner reads one character at a time and pushes back at most
 * one, the lookahead that ended an input item -- for a stream that is
 * exactly ungetc's one guaranteed character, so a string and a stream
 * scan alike.  An input item is the longest run that is, or begins, a
 * matching sequence (7.21.6.2p9); one that is only a beginning ("0x"
 * for %x, "1e+" for %f) is a matching failure and has been consumed
 * (p10, footnote 285).
 *
 * Every conversion (d i o u x X a A e E f F g G c s [ p n %) and every
 * length modifier (hh h l ll j z t L) is taken.  long double is double
 * here; intmax_t is long long; size_t, ptrdiff_t and long are 32 bits.
 * %lc, %ls and %l[ store wide characters as the "C" locale has them,
 * one byte each.  Integers convert as strtoll/strtoull would; a value
 * out of range is undefined behaviour (p10), and wraps.  Floating
 * items go through strtod. */

#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>

/* --- the source --- */

struct scan_src {
    const char *str;    /* a string source, or NULL */
    FILE *fp;           /* a stream source */
    int count;          /* characters consumed, for %n */
    int ahead;          /* a character read and pushed back, or -2 */
};

static int src_get(struct scan_src *s) {
    int c;
    if (s->ahead != -2) {
        c = s->ahead;
        s->ahead = -2;
    } else if (s->str) {
        c = (unsigned char)*s->str;
        if (c == 0) c = EOF;
        else s->str++;
    } else {
        c = fgetc(s->fp);
    }
    if (c != EOF) s->count++;
    return c;
}

/* Give c back: the next src_get returns it.  A stream gets it back
 * through ungetc too, so what the caller leaves unread is unread. */
static void src_unget(struct scan_src *s, int c) {
    if (c == EOF) return;
    s->count--;
    s->ahead = c;
}

/* At the end of a call the pushed-back character goes back to the
 * stream (a string needs nothing). */
static void src_done(struct scan_src *s) {
    if (s->ahead != -2 && s->fp && s->ahead != EOF) ungetc(s->ahead, s->fp);
    s->ahead = -2;
}

static int sc_space(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r';
}

static int sc_digit_value(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'z') return c - 'a' + 10;
    if (c >= 'A' && c <= 'Z') return c - 'A' + 10;
    return 99;
}

/* skip white space; returns the first other character (pushed back),
 * or EOF */
static int sc_skip_space(struct scan_src *s) {
    int c;
    do {
        c = src_get(s);
    } while (c != EOF && sc_space(c));
    src_unget(s, c);
    return c;
}

/* --- the input item buffer (grows: a floating item may be long) --- */

struct scan_buf {
    char *p;
    int n;
    int cap;
    char small[128];
};

static void buf_init(struct scan_buf *b) {
    b->p = b->small;
    b->n = 0;
    b->cap = (int)sizeof b->small;
}

static int buf_add(struct scan_buf *b, int c) {
    if (b->n + 1 >= b->cap) {
        char *q;
        int ncap = b->cap * 2;
        int i;
        q = (char *)malloc((size_t)ncap);
        if (!q) return 0;
        for (i = 0; i < b->n; i++) q[i] = b->p[i];
        if (b->p != b->small) free(b->p);
        b->p = q;
        b->cap = ncap;
    }
    b->p[b->n++] = (char)c;
    b->p[b->n] = 0;
    return 1;
}

static void buf_free(struct scan_buf *b) {
    if (b->p != b->small) free(b->p);
}

/* --- integers: [sign] [0x] digits, as strtol's subject sequence --- */

/* Read an integer input item of at most width characters in base (0:
 * as %i, prefix decides).  Returns 1 and the value when the item is a
 * matching sequence, 0 on a matching failure, -1 on an input failure
 * (end of input before any character of the item). */
static int sc_integer(struct scan_src *s, int base, int width, unsigned long long *out) {
    int c;
    int neg = 0;
    int digits = 0;
    unsigned long long v = 0;
    int d;

    c = src_get(s);
    if (c == EOF) return -1;
    if ((c == '+' || c == '-') && width > 0) {
        neg = (c == '-');
        width--;
        if (width == 0) return 0;
        c = src_get(s);
    }
    if ((base == 0 || base == 16) && c == '0' && width > 0) {
        /* "0", or the start of "0x" */
        width--;
        digits = 1;
        if (width == 0) { *out = 0; return 1; }
        c = src_get(s);
        if (c == 'x' || c == 'X') {
            width--;
            /* 0x needs a hex digit after it, or the item ("0x") is only
             * the beginning of a matching sequence */
            if (width == 0) return 0;
            c = src_get(s);
            if (sc_digit_value(c) >= 16) {
                src_unget(s, c);
                return 0;
            }
            base = 16;
            digits = 0;
        } else if (base == 0) {
            base = 8;
        }
    } else if (base == 0) {
        base = 10;
    }
    while (width > 0 && c != EOF && (d = sc_digit_value(c)) < base) {
        v = v * (unsigned long long)base + (unsigned long long)d;
        digits++;
        width--;
        if (width == 0) { c = EOF; break; }
        c = src_get(s);
    }
    if (width > 0 || c != EOF) src_unget(s, c);
    if (digits == 0) return 0;
    *out = neg ? 0 - v : v;
    return 1;
}

/* --- floating: strtod's subject sequence --- */

static int sc_lower(int c) {
    return (c >= 'A' && c <= 'Z') ? c + 32 : c;
}

/* Take c into the item if it is (any case) the character want. */
static int sc_take(struct scan_src *s, struct scan_buf *b, int *c, int *width, int want) {
    if (*width <= 0 || *c == EOF || sc_lower(*c) != want) return 0;
    buf_add(b, *c);
    (*width)--;
    *c = (*width > 0) ? src_get(s) : EOF;
    return 1;
}

/* Read a floating input item into b.  Returns 1 when it is a matching
 * sequence, 0 on a matching failure, -1 on an input failure. */
static int sc_float_item(struct scan_src *s, struct scan_buf *b, int width) {
    int c;
    int hex = 0;
    int mant = 0;
    int ok = 0;
    const char *word;
    int i;

    c = src_get(s);
    if (c == EOF) return -1;
    if ((c == '+' || c == '-') && width > 0) {
        buf_add(b, c);
        width--;
        c = (width > 0) ? src_get(s) : EOF;
    }
    if (c != EOF && width > 0 && (sc_lower(c) == 'i' || sc_lower(c) == 'n')) {
        /* inf, infinity, nan, nan(n-char-sequence) */
        word = (sc_lower(c) == 'i') ? "infinity" : "nan";
        i = 0;
        while (word[i] && sc_take(s, b, &c, &width, word[i])) {
            i++;
            if (word[0] == 'i' && i == 3) ok = 1;      /* "inf" matches */
        }
        if (word[i] == 0) ok = 1;
        else if (word[0] == 'i' && i > 3) ok = 0;      /* "infin": a beginning only */
        if (word[0] == 'n' && ok && c == '(' && width > 0) {
            buf_add(b, c);
            width--;
            c = (width > 0) ? src_get(s) : EOF;
            while (c != EOF && width > 0 && (c == '_' || sc_digit_value(c) < 36)) {
                buf_add(b, c);
                width--;
                c = (width > 0) ? src_get(s) : EOF;
            }
            if (c == ')' && width > 0) {
                buf_add(b, c);
                width--;
                c = EOF;
            } else {
                ok = 0;
            }
        }
        if (c != EOF) src_unget(s, c);
        return ok;
    }
    if (c == '0' && width > 0) {
        buf_add(b, c);
        width--;
        mant = 1;
        c = (width > 0) ? src_get(s) : EOF;
        if (c != EOF && sc_lower(c) == 'x' && width > 0) {
            buf_add(b, c);
            width--;
            hex = 1;
            mant = 0;
            c = (width > 0) ? src_get(s) : EOF;
        }
    }
    while (c != EOF && width > 0 && sc_digit_value(c) < (hex ? 16 : 10)) {
        buf_add(b, c);
        width--;
        mant = 1;
        c = (width > 0) ? src_get(s) : EOF;
    }
    if (c == '.' && width > 0) {
        buf_add(b, c);
        width--;
        c = (width > 0) ? src_get(s) : EOF;
        while (c != EOF && width > 0 && sc_digit_value(c) < (hex ? 16 : 10)) {
            buf_add(b, c);
            width--;
            mant = 1;
            c = (width > 0) ? src_get(s) : EOF;
        }
    }
    ok = mant;
    if (mant && c != EOF && width > 0 && sc_lower(c) == (hex ? 'p' : 'e')) {
        buf_add(b, c);
        width--;
        ok = 0;                 /* an exponent marker wants digits */
        c = (width > 0) ? src_get(s) : EOF;
        if ((c == '+' || c == '-') && width > 0) {
            buf_add(b, c);
            width--;
            c = (width > 0) ? src_get(s) : EOF;
        }
        while (c != EOF && width > 0 && c >= '0' && c <= '9') {
            buf_add(b, c);
            width--;
            ok = 1;
            c = (width > 0) ? src_get(s) : EOF;
        }
    }
    if (c != EOF) src_unget(s, c);
    return ok;
}

/* --- storing --- */

enum { LEN_NONE, LEN_HH, LEN_H, LEN_L, LEN_LL, LEN_J, LEN_Z, LEN_T, LEN_BIGL };

static void store_int(va_list *ap, int len, unsigned long long v) {
    switch (len) {
    case LEN_HH: *va_arg(*ap, signed char *) = (signed char)v; break;
    case LEN_H:  *va_arg(*ap, short *) = (short)v; break;
    case LEN_L:  *va_arg(*ap, long *) = (long)v; break;
    case LEN_LL:
    case LEN_J:  *va_arg(*ap, long long *) = (long long)v; break;
    case LEN_Z:  *va_arg(*ap, size_t *) = (size_t)v; break;
    case LEN_T:  *va_arg(*ap, ptrdiff_t *) = (ptrdiff_t)v; break;
    default:     *va_arg(*ap, int *) = (int)v; break;
    }
}

/* --- the scanset of %[ --- */

/* Parse the scanset after "%[" into set (256 flags); returns the format
 * past the closing ']', or NULL when there is none. */
static const unsigned char *sc_scanset(const unsigned char *f, char *set) {
    int neg = 0;
    int i;
    int prev = -1;
    for (i = 0; i < 256; i++) set[i] = 0;
    if (*f == '^') { neg = 1; f++; }
    if (*f == ']') { set[']'] = 1; prev = ']'; f++; }
    while (*f && *f != ']') {
        /* a-z: a range (implementation-defined, 7.21.6.2p12) when '-'
         * stands between two characters, low to high */
        if (*f == '-' && prev >= 0 && f[1] && f[1] != ']' && f[1] >= prev) {
            int hi = f[1];
            for (i = prev; i <= hi; i++) set[i] = 1;
            prev = -1;
            f += 2;
            continue;
        }
        set[*f] = 1;
        prev = *f;
        f++;
    }
    if (*f != ']') return NULL;
    if (neg) for (i = 0; i < 256; i++) set[i] = !set[i];
    return f + 1;
}

/* --- the scanner --- */

static int sc_vscan(struct scan_src *s, const char *format, va_list ap0) {
    const unsigned char *f = (const unsigned char *)format;
    int assigned = 0;
    int converted = 0;      /* a conversion has completed (for EOF) */
    int c;
    va_list ap;
    char set[256];

    va_copy(ap, ap0);
    while (*f) {
        if (sc_space(*f)) {
            while (sc_space(*f)) f++;
            sc_skip_space(s);
            continue;
        }
        if (*f != '%') {
            c = src_get(s);
            if (c == EOF) goto input_failure;
            if (c != *f) { src_unget(s, c); goto done; }
            f++;
            continue;
        }
        f++;
        {
            int suppress = 0;
            int width = 0;
            int len = LEN_NONE;
            int conv;
            if (*f == '*') { suppress = 1; f++; }
            while (*f >= '0' && *f <= '9') { width = width * 10 + (*f - '0'); f++; }
            if (*f == 'h') { f++; if (*f == 'h') { f++; len = LEN_HH; } else len = LEN_H; }
            else if (*f == 'l') { f++; if (*f == 'l') { f++; len = LEN_LL; } else len = LEN_L; }
            else if (*f == 'j') { f++; len = LEN_J; }
            else if (*f == 'z') { f++; len = LEN_Z; }
            else if (*f == 't') { f++; len = LEN_T; }
            else if (*f == 'L') { f++; len = LEN_BIGL; }
            conv = *f;
            if (conv == 0) goto done;
            f++;
            if (conv != '[' && conv != 'c' && conv != 'n') {
                if (sc_skip_space(s) == EOF && conv != '%') goto input_failure;
            }
            switch (conv) {
            case '%':
                c = src_get(s);
                if (c == EOF) goto input_failure;
                if (c != '%') { src_unget(s, c); goto done; }
                break;
            case 'n':
                if (!suppress) store_int(&ap, len, (unsigned long long)(long long)s->count);
                break;
            case 'd': case 'i': case 'o': case 'u': case 'x': case 'X': case 'p': {
                unsigned long long v;
                int base = (conv == 'd' || conv == 'u') ? 10 : conv == 'i' ? 0 : conv == 'o' ? 8 : 16;
                int r = sc_integer(s, base, width > 0 ? width : 0x7FFFFFFF, &v);
                if (r < 0) goto input_failure;
                if (r == 0) goto done;
                if (!suppress) {
                    if (conv == 'p') *va_arg(ap, void **) = (void *)(size_t)v;
                    else store_int(&ap, len, v);
                    assigned++;
                }
                converted = 1;
                break;
            }
            case 'a': case 'A': case 'e': case 'E': case 'f': case 'F': case 'g': case 'G': {
                struct scan_buf b;
                int r;
                double d;
                buf_init(&b);
                r = sc_float_item(s, &b, width > 0 ? width : 0x7FFFFFFF);
                if (r <= 0) {
                    buf_free(&b);
                    if (r < 0) goto input_failure;
                    goto done;
                }
                d = strtod(b.p, NULL);
                buf_free(&b);
                if (!suppress) {
                    if (len == LEN_L || len == LEN_BIGL) *va_arg(ap, double *) = d;
                    else *va_arg(ap, float *) = (float)d;
                    assigned++;
                }
                converted = 1;
                break;
            }
            case 'c': {
                int n = width > 0 ? width : 1;
                int i;
                char *out = NULL;
                int *wout = NULL;
                if (!suppress) {
                    if (len == LEN_L) wout = va_arg(ap, int *);
                    else out = va_arg(ap, char *);
                }
                for (i = 0; i < n; i++) {
                    c = src_get(s);
                    if (c == EOF) goto input_failure;
                    if (out) out[i] = (char)c;
                    if (wout) wout[i] = c;
                }
                if (!suppress) assigned++;
                converted = 1;
                break;
            }
            case 's':
            case '[': {
                int n = width > 0 ? width : 0x7FFFFFFF;
                int got = 0;
                char *out = NULL;
                int *wout = NULL;
                if (conv == '[') {
                    f = sc_scanset(f, set);
                    if (!f) goto done;
                }
                if (!suppress) {
                    if (len == LEN_L) wout = va_arg(ap, int *);
                    else out = va_arg(ap, char *);
                }
                while (got < n) {
                    c = src_get(s);
                    if (c == EOF) break;
                    if (conv == 's' ? sc_space(c) : !set[c]) { src_unget(s, c); break; }
                    if (out) out[got] = (char)c;
                    if (wout) wout[got] = c;
                    got++;
                }
                if (got == 0) {
                    if (c == EOF) goto input_failure;
                    goto done;
                }
                if (out) out[got] = 0;
                if (wout) wout[got] = 0;
                if (!suppress) assigned++;
                converted = 1;
                break;
            }
            default:
                goto done;      /* an invalid conversion: undefined; stop */
            }
        }
    }
done:
    va_end(ap);
    src_done(s);
    return assigned;
input_failure:
    va_end(ap);
    src_done(s);
    return (converted || assigned) ? assigned : EOF;
}

int vsscanf(const char *str, const char *format, va_list ap) {
    struct scan_src s;
    s.str = str;
    s.fp = NULL;
    s.count = 0;
    s.ahead = -2;
    return sc_vscan(&s, format, ap);
}

int vfscanf(FILE *stream, const char *format, va_list ap) {
    struct scan_src s;
    s.str = NULL;
    s.fp = stream;
    s.count = 0;
    s.ahead = -2;
    return sc_vscan(&s, format, ap);
}

int vscanf(const char *format, va_list ap) {
    return vfscanf(stdin, format, ap);
}

int sscanf(const char *str, const char *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vsscanf(str, format, ap);
    va_end(ap);
    return r;
}

int fscanf(FILE *stream, const char *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfscanf(stream, format, ap);
    va_end(ap);
    return r;
}

int scanf(const char *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfscanf(stdin, format, ap);
    va_end(ap);
    return r;
}
