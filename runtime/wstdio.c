/* Wide input and output (C11 7.29.2, 7.29.3), for both C libraries.
 *
 * A wide stream is a byte stream that holds multibyte characters: fputwc
 * writes a character's multibyte form (wcrtomb) and fgetwc reads one
 * (mbrtowc), through the byte functions.  Each library's FILE carries the
 * rest (struct __s32_wstate, reached by __s32_wstate): the orientation
 * fwide reports, and the one wide character ungetwc guarantees.
 *
 * The first wide function on a stream with no orientation makes it wide;
 * the first byte function, byte oriented (the byte functions mark the
 * stream as used, and an unoriented stream so marked is a byte one).  A
 * wide function on a byte-oriented stream is undefined (7.21.2p5); here it
 * does what it would on a wide one, so a program that mixes printf and
 * wprintf on stdout prints both.
 *
 * The wprintf family formats with printf's engine in its wide mode
 * (printf_enhanced.c), over the multibyte form of the format; the wscanf
 * family is the scanner's wide mode (sscanf.c). */
#include <errno.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

size_t __s32_vformat_wide(char *buffer, size_t buffer_size, const char *format, va_list ap, size_t *nchars);
int __s32_vwscan(const wchar_t *str, FILE *fp, const wchar_t *format, va_list ap,
                 wint_t (*wget)(FILE *), wint_t (*wunget)(wint_t, FILE *), int (*wspace)(wint_t));

static int orientation(struct __s32_wstate *w) {
    if (w->orient) return w->orient;
    return w->bytes ? -1 : 0;
}

/* a wide function's use: an unoriented stream becomes wide */
static struct __s32_wstate *use_wide(FILE *fp) {
    struct __s32_wstate *w = __s32_wstate(fp);
    if (orientation(w) == 0) w->orient = 1;
    return w;
}

int fwide(FILE *stream, int mode) {
    struct __s32_wstate *w = __s32_wstate(stream);
    int o = orientation(w);
    if (o == 0 && mode != 0) {
        w->orient = mode > 0 ? 1 : -1;
        o = w->orient;
    }
    return o;
}

/* --- characters --- */

/* the next wide character, or WEOF -- *bad set when for an encoding
 * error (errno EILSEQ), not end of file or a read error */
static wint_t get_wide(FILE *stream, int *bad) {
    struct __s32_wstate *w = use_wide(stream);
    mbstate_t st;
    int c;
    *bad = 0;
    if (w->nback) {
        w->nback = 0;
        return w->back;
    }
    memset(&st, 0, sizeof st);
    while ((c = fgetc(stream)) != EOF) {
        char ch = (char)c;
        wchar_t wc;
        size_t r = mbrtowc(&wc, &ch, 1, &st);
        if (r == (size_t)-2) continue;
        if (r == (size_t)-1) {
            *bad = 1;
            return WEOF;
        }
        return (wint_t)wc;
    }
    if (!mbsinit(&st)) {                        /* the file ends inside a character */
        errno = EILSEQ;
        *bad = 1;
    }
    return WEOF;
}

wint_t fgetwc(FILE *stream) {
    int bad;
    return get_wide(stream, &bad);
}

wint_t getwc(FILE *stream) {
    return fgetwc(stream);
}

wint_t getwchar(void) {
    return fgetwc(stdin);
}

wint_t fputwc(wchar_t c, FILE *stream) {
    char buf[8];
    mbstate_t st;
    size_t r;
    use_wide(stream);
    memset(&st, 0, sizeof st);
    r = wcrtomb(buf, c, &st);
    if (r == (size_t)-1) return WEOF;           /* errno is EILSEQ */
    if (fwrite(buf, 1, r, stream) != r) return WEOF;
    return (wint_t)c;
}

wint_t putwc(wchar_t c, FILE *stream) {
    return fputwc(c, stream);
}

wint_t putwchar(wchar_t c) {
    return fputwc(c, stdout);
}

/* One wide character back, held by the stream until it is read again;
 * a second before the first is read fails.  End of file is cleared, as
 * ungetc clears it -- unless an error is also set, which clearerr would
 * take with it. */
wint_t ungetwc(wint_t c, FILE *stream) {
    struct __s32_wstate *w = use_wide(stream);
    if (c == WEOF || w->nback) return WEOF;
    w->nback = 1;
    w->back = c;
    if (feof(stream) && !ferror(stream)) clearerr(stream);
    return c;
}

wchar_t *fgetws(wchar_t *s, int n, FILE *stream) {
    int i = 0;
    if (n <= 0) return NULL;
    use_wide(stream);
    while (i < n - 1) {
        int bad;
        wint_t c = get_wide(stream, &bad);
        if (c == WEOF) {
            /* end of file after a character ends the line; a read or an
             * encoding error leaves nothing (7.29.3.2p3) */
            if (i == 0 || bad || ferror(stream)) return NULL;
            break;
        }
        s[i++] = (wchar_t)c;
        if (c == L'\n') break;
    }
    s[i] = 0;
    return s;
}

int fputws(const wchar_t *s, FILE *stream) {
    use_wide(stream);
    for (; *s; s++)
        if (fputwc(*s, stream) == WEOF) return EOF;
    return 0;
}

/* --- formatted output --- */

/* the multibyte form of a wide format, malloc'd; NULL with errno set */
static char *mb_format(const wchar_t *format) {
    mbstate_t st;
    const wchar_t *src = format;
    size_t n;
    char *mb;
    memset(&st, 0, sizeof st);
    n = wcsrtombs(NULL, &src, 0, &st);
    if (n == (size_t)-1) return NULL;
    mb = (char *)malloc(n + 1);
    if (!mb) {
        errno = ENOMEM;
        return NULL;
    }
    src = format;
    memset(&st, 0, sizeof st);
    wcsrtombs(mb, &src, n + 1, &st);
    return mb;
}

/* format into a malloc'd multibyte string: its bytes in *len, the return
 * its characters, or -1 */
static int format_mb(char **out, size_t *len, const wchar_t *format, va_list ap) {
    char *fmt = mb_format(format);
    char *buf;
    size_t n, nc = 0;
    va_list again;
    if (!fmt) return -1;
    va_copy(again, ap);
    n = __s32_vformat_wide(NULL, 0, fmt, again, &nc);
    va_end(again);
    if (n == (size_t)-1 || n > 0x7FFFFFFF) {
        free(fmt);
        return -1;
    }
    buf = (char *)malloc(n + 1);
    if (!buf) {
        free(fmt);
        errno = ENOMEM;
        return -1;
    }
    __s32_vformat_wide(buf, n + 1, fmt, ap, &nc);
    free(fmt);
    *out = buf;
    *len = n;
    return (int)nc;
}

int vfwprintf(FILE *stream, const wchar_t *format, va_list arg) {
    char *buf;
    size_t len;
    int nc;
    use_wide(stream);
    nc = format_mb(&buf, &len, format, arg);
    if (nc < 0) return -1;
    if (len > 0 && fwrite(buf, 1, len, stream) != len) nc = -1;
    free(buf);
    return nc;
}

int vwprintf(const wchar_t *format, va_list arg) {
    return vfwprintf(stdout, format, arg);
}

/* n wide characters at most, the null included: more is an error (a
 * negative return, 7.29.2.7p3), with what fits written and terminated */
int vswprintf(wchar_t *s, size_t n, const wchar_t *format, va_list arg) {
    char *buf;
    size_t len;
    const char *src;
    mbstate_t st;
    int nc;
    if (n == 0) return -1;
    nc = format_mb(&buf, &len, format, arg);
    if (nc < 0) {
        s[0] = 0;
        return -1;
    }
    src = buf;
    memset(&st, 0, sizeof st);
    if ((size_t)nc >= n) {
        mbsrtowcs(s, &src, n - 1, &st);
        s[n - 1] = 0;
        nc = -1;
    } else {
        mbsrtowcs(s, &src, n, &st);
    }
    free(buf);
    return nc;
}

int fwprintf(FILE *stream, const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfwprintf(stream, format, ap);
    va_end(ap);
    return r;
}

int wprintf(const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfwprintf(stdout, format, ap);
    va_end(ap);
    return r;
}

int swprintf(wchar_t *s, size_t n, const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vswprintf(s, n, format, ap);
    va_end(ap);
    return r;
}

/* --- formatted input --- */

int vfwscanf(FILE *stream, const wchar_t *format, va_list arg) {
    use_wide(stream);
    return __s32_vwscan(NULL, stream, format, arg, fgetwc, ungetwc, iswspace);
}

int vwscanf(const wchar_t *format, va_list arg) {
    return vfwscanf(stdin, format, arg);
}

int vswscanf(const wchar_t *s, const wchar_t *format, va_list arg) {
    return __s32_vwscan(s, NULL, format, arg, fgetwc, ungetwc, iswspace);
}

int fwscanf(FILE *stream, const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfwscanf(stream, format, ap);
    va_end(ap);
    return r;
}

int wscanf(const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vfwscanf(stdin, format, ap);
    va_end(ap);
    return r;
}

int swscanf(const wchar_t *s, const wchar_t *format, ...) {
    va_list ap;
    int r;
    va_start(ap, format);
    r = vswscanf(s, format, ap);
    va_end(ap);
    return r;
}
