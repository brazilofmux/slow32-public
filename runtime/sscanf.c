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
 * %lc, %ls and %l[ store the wide characters of the multibyte characters
 * read (mbrtowc), their field widths counting those characters, as glibc's
 * and the BSDs' do.  Integers convert as strtoll/strtoull would; a value
 * out of range is undefined behaviour (p10), and wraps.  Floating
 * items go through strtod.
 *
 * The wide family (7.29.2.2, .4, .6, .8, .10, .12) is the same scanner
 * over wide characters: a wide format, a wide string or a stream read with
 * fgetwc, white space as iswspace has it, field widths and %n counting
 * wide characters, and %c, %s and %[ without l storing multibyte forms
 * (wcrtomb).  Its entry points are wstdio.c's, which hand this file the
 * wide functions it needs (__s32_vwscan), so that a program scanning only
 * bytes does not link the Unicode tables. */

#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>

/* --- the source --- */

struct scan_src {
    const char *str;    /* a string source, or NULL */
    const wchar_t *wstr;    /* a wide string source, or NULL */
    FILE *fp;           /* a stream source */
    int count;          /* characters consumed, for %n */
    int ahead;          /* a character read and pushed back, or -2 */
    int wide;           /* the wide family: what follows is set */
    wint_t (*wget)(FILE *);             /* fgetwc */
    wint_t (*wunget)(wint_t, FILE *);   /* ungetwc */
    int (*wspace)(wint_t);              /* iswspace */
};

static void src_init(struct scan_src *s) {
    s->str = NULL;
    s->wstr = NULL;
    s->fp = NULL;
    s->count = 0;
    s->ahead = -2;
    s->wide = 0;
    s->wget = NULL;
    s->wunget = NULL;
    s->wspace = NULL;
}

static int src_get(struct scan_src *s) {
    int c;
    if (s->ahead != -2) {
        c = s->ahead;
        s->ahead = -2;
    } else if (s->str) {
        c = (unsigned char)*s->str;
        if (c == 0) c = EOF;
        else s->str++;
    } else if (s->wstr) {
        c = (int)*s->wstr;
        if (c == 0) c = EOF;
        else s->wstr++;
    } else if (s->wide) {
        wint_t w = s->wget(s->fp);
        c = (w == WEOF) ? EOF : (int)w;
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
    if (s->ahead != -2 && s->fp && s->ahead != EOF) {
        if (s->wide) s->wunget((wint_t)s->ahead, s->fp);
        else ungetc(s->ahead, s->fp);
    }
    s->ahead = -2;
}

static int sc_space(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\v' || c == '\f' || c == '\r';
}

/* white space in the input or the format: a wide one is iswspace's */
static int sc_white(struct scan_src *s, int c) {
    if (c == EOF) return 0;
    if (s->wide && c >= 0x80) return s->wspace((wint_t)c);
    return sc_space(c);
}

/* --- the format: bytes, or wide characters --- */

struct scan_fmt {
    const unsigned char *n;     /* narrow, or NULL */
    const wchar_t *w;           /* wide */
};

static int fch(const struct scan_fmt *f, int k) {
    return f->w ? (int)f->w[k] : (int)f->n[k];
}

static void fadv(struct scan_fmt *f, int k) {
    if (f->w) f->w += k;
    else f->n += k;
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
    } while (c != EOF && sc_white(s, c));
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

/* Is c in the scanset that begins at f (just after "%[")?  With c ==
 * EOF, only find the set's end: f is moved past the closing ']', and the
 * answer is whether there is one.  The set is read again for each
 * character, so it may hold any wide character. */
static int sc_scanset(struct scan_fmt *f, int c) {
    int neg = 0;
    int in = 0;
    int prev = -1;
    struct scan_fmt p = *f;
    if (fch(&p, 0) == '^') { neg = 1; fadv(&p, 1); }
    if (fch(&p, 0) == ']') { in |= (c == ']'); prev = ']'; fadv(&p, 1); }
    while (fch(&p, 0) && fch(&p, 0) != ']') {
        int x = fch(&p, 0);
        /* a-z: a range (implementation-defined, 7.21.6.2p12) when '-'
         * stands between two characters, low to high */
        if (x == '-' && prev >= 0 && fch(&p, 1) && fch(&p, 1) != ']' && fch(&p, 1) >= prev) {
            in |= (c >= prev && c <= fch(&p, 1));
            prev = -1;
            fadv(&p, 2);
            continue;
        }
        in |= (c == x);
        prev = x;
        fadv(&p, 1);
    }
    if (c == EOF) {
        if (fch(&p, 0) != ']') return 0;
        fadv(&p, 1);
        *f = p;
        return 1;
    }
    return neg ? !in : in;
}

/* --- the scanner --- */

/* Store one character c of %c, %s or %[: into the char array out at *at
 * (as itself, or in the wide family as its multibyte form), or into the
 * wchar_t array wout at *wat (as itself in the wide family, or decoded
 * from the bytes of a multibyte character).  0 on an encoding error. */
static int sc_store(struct scan_src *s, int c, char *out, int *at, wchar_t *wout, int *wat, mbstate_t *st,
                    int *pending) {
    if (s->wide) {
        if (out) {
            size_t r = wcrtomb(out + *at, (wchar_t)c, st);
            if (r == (size_t)-1) return 0;
            *at += (int)r;
        }
        if (wout) wout[(*wat)++] = (wchar_t)c;
        return 1;
    }
    if (out) out[(*at)++] = (char)c;
    if (wout || pending) {
        char ch = (char)c;
        wchar_t wc;
        size_t r = mbrtowc(&wc, &ch, 1, st);
        if (r == (size_t)-1) return 0;
        if (r == (size_t)-2) {
            *pending = 1;
            return 1;
        }
        *pending = 0;
        if (wout) wout[(*wat)++] = wc;
    }
    return 1;
}

static int sc_vscan(struct scan_src *s, struct scan_fmt *f, va_list ap0) {
    int assigned = 0;
    int converted = 0;      /* a conversion has completed (for EOF) */
    int c;
    va_list ap;

    va_copy(ap, ap0);
    while (fch(f, 0)) {
        if (sc_white(s, fch(f, 0))) {
            while (sc_white(s, fch(f, 0))) fadv(f, 1);
            sc_skip_space(s);
            continue;
        }
        if (fch(f, 0) != '%') {
            c = src_get(s);
            if (c == EOF) goto input_failure;
            if (c != fch(f, 0)) { src_unget(s, c); goto done; }
            fadv(f, 1);
            continue;
        }
        fadv(f, 1);
        {
            int suppress = 0;
            int width = 0;
            int len = LEN_NONE;
            int conv;
            if (fch(f, 0) == '*') { suppress = 1; fadv(f, 1); }
            while (fch(f, 0) >= '0' && fch(f, 0) <= '9') { width = width * 10 + (fch(f, 0) - '0'); fadv(f, 1); }
            if (fch(f, 0) == 'h') { fadv(f, 1); if (fch(f, 0) == 'h') { fadv(f, 1); len = LEN_HH; } else len = LEN_H; }
            else if (fch(f, 0) == 'l') { fadv(f, 1); if (fch(f, 0) == 'l') { fadv(f, 1); len = LEN_LL; } else len = LEN_L; }
            else if (fch(f, 0) == 'j') { fadv(f, 1); len = LEN_J; }
            else if (fch(f, 0) == 'z') { fadv(f, 1); len = LEN_Z; }
            else if (fch(f, 0) == 't') { fadv(f, 1); len = LEN_T; }
            else if (fch(f, 0) == 'L') { fadv(f, 1); len = LEN_BIGL; }
            conv = fch(f, 0);
            if (conv == 0) goto done;
            fadv(f, 1);
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
            case 'c':
            case 's':
            case '[': {
                /* %c: exactly width characters (1 without one), white
                 * space included; %s and %[: a nonempty run, at most
                 * width, of non-white-space or of the scanset's */
                int n = width > 0 ? width : (conv == 'c' ? 1 : 0x7FFFFFFF);
                int got = 0, at = 0, wat = 0, pending = 0;
                char *out = NULL;
                wchar_t *wout = NULL;
                struct scan_fmt set = *f;
                mbstate_t st;
                memset(&st, 0, sizeof st);
                if (conv == '[' && !sc_scanset(f, EOF)) goto done;
                if (!suppress) {
                    if (len == LEN_L) wout = va_arg(ap, wchar_t *);
                    else out = va_arg(ap, char *);
                }
                while (got < n) {
                    c = src_get(s);
                    if (c == EOF) break;
                    if (conv == 's' ? sc_white(s, c) : conv == '[' ? !sc_scanset(&set, c) : 0) {
                        src_unget(s, c);
                        break;
                    }
                    if (!sc_store(s, c, out, &at, wout, &wat, &st, len == LEN_L ? &pending : NULL))
                        goto done;      /* an encoding error */
                    if (!pending) got++;    /* the width counts whole characters */
                }
                if (got == 0 || (conv == 'c' && got < n)) {
                    if (c == EOF) goto input_failure;
                    goto done;
                }
                if (pending) goto done;     /* the last multibyte character is cut off */
                if (conv != 'c') {
                    if (out) out[at] = 0;
                    if (wout) wout[wat] = 0;
                }
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
    struct scan_fmt f;
    src_init(&s);
    s.str = str;
    f.n = (const unsigned char *)format;
    f.w = NULL;
    return sc_vscan(&s, &f, ap);
}

int vfscanf(FILE *stream, const char *format, va_list ap) {
    struct scan_src s;
    struct scan_fmt f;
    src_init(&s);
    s.fp = stream;
    f.n = (const unsigned char *)format;
    f.w = NULL;
    return sc_vscan(&s, &f, ap);
}

/* The wide family's scanner (wstdio.c): a wide string str, or the stream
 * fp, read with the wide functions given. */
int __s32_vwscan(const wchar_t *str, FILE *fp, const wchar_t *format, va_list ap,
                 wint_t (*wget)(FILE *), wint_t (*wunget)(wint_t, FILE *), int (*wspace)(wint_t)) {
    struct scan_src s;
    struct scan_fmt f;
    src_init(&s);
    s.wide = 1;
    s.wget = wget;
    s.wunget = wunget;
    s.wspace = wspace;
    if (str) s.wstr = str;
    else s.fp = fp;
    f.n = NULL;
    f.w = format;
    return sc_vscan(&s, &f, ap);
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
