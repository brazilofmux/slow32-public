/* The wide strings (C11 7.29.4, 7.29.5, and 7.8.2.4's wcstoimax and
 * wcstoumax), for both C libraries.
 *
 * A wide character is a code point (wchar.c), and wchar_t is unsigned,
 * so comparing two is comparing their values.  The numeric conversions
 * and wcsftime use the narrow functions over the multibyte form of the
 * string, which is the same text: wcstod reads what strtod reads,
 * LC_NUMERIC's radix included, and wcsftime writes what strftime writes. */
#include <inttypes.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <wchar.h>

size_t wcslen(const wchar_t *s) {
    const wchar_t *p = s;
    while (*p) p++;
    return (size_t)(p - s);
}

size_t wcsnlen(const wchar_t *s, size_t maxlen) {
    size_t n = 0;
    while (n < maxlen && s[n]) n++;
    return n;
}

wchar_t *wcscpy(wchar_t *s1, const wchar_t *s2) {
    wchar_t *d = s1;
    while ((*d++ = *s2++) != 0) {}
    return s1;
}

wchar_t *wcsncpy(wchar_t *s1, const wchar_t *s2, size_t n) {
    size_t i = 0;
    for (; i < n && s2[i]; i++) s1[i] = s2[i];
    for (; i < n; i++) s1[i] = 0;
    return s1;
}

wchar_t *wcscat(wchar_t *s1, const wchar_t *s2) {
    wcscpy(s1 + wcslen(s1), s2);
    return s1;
}

wchar_t *wcsncat(wchar_t *s1, const wchar_t *s2, size_t n) {
    wchar_t *d = s1 + wcslen(s1);
    while (n-- > 0 && *s2) *d++ = *s2++;
    *d = 0;
    return s1;
}

wchar_t *wcsdup(const wchar_t *s) {
    size_t n = wcslen(s) + 1;
    wchar_t *d = (wchar_t *)malloc(n * sizeof(wchar_t));
    if (d) wmemcpy(d, s, n);
    return d;
}

int wcscmp(const wchar_t *s1, const wchar_t *s2) {
    while (*s1 && *s1 == *s2) {
        s1++;
        s2++;
    }
    return *s1 < *s2 ? -1 : *s1 > *s2;
}

int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n) {
    for (; n > 0; n--, s1++, s2++) {
        if (*s1 != *s2) return *s1 < *s2 ? -1 : 1;
        if (*s1 == 0) break;
    }
    return 0;
}

wchar_t *wcschr(const wchar_t *s, wchar_t c) {
    for (;; s++) {
        if (*s == c) return (wchar_t *)s;
        if (*s == 0) return NULL;
    }
}

wchar_t *wcsrchr(const wchar_t *s, wchar_t c) {
    const wchar_t *last = NULL;
    for (;; s++) {
        if (*s == c) last = s;
        if (*s == 0) return (wchar_t *)last;
    }
}

size_t wcsspn(const wchar_t *s1, const wchar_t *s2) {
    size_t n = 0;
    while (s1[n] && wcschr(s2, s1[n])) n++;
    return n;
}

size_t wcscspn(const wchar_t *s1, const wchar_t *s2) {
    size_t n = 0;
    while (s1[n] && !wcschr(s2, s1[n])) n++;
    return n;
}

wchar_t *wcspbrk(const wchar_t *s1, const wchar_t *s2) {
    s1 += wcscspn(s1, s2);
    return *s1 ? (wchar_t *)s1 : NULL;
}

wchar_t *wcsstr(const wchar_t *s1, const wchar_t *s2) {
    size_t n = wcslen(s2);
    if (n == 0) return (wchar_t *)s1;
    for (; *s1; s1++)
        if (*s1 == *s2 && wmemcmp(s1, s2, n) == 0) return (wchar_t *)s1;
    return NULL;
}

wchar_t *wcstok(wchar_t *s1, const wchar_t *s2, wchar_t **ptr) {
    wchar_t *end;
    if (!s1) s1 = *ptr;
    if (!s1) return NULL;
    s1 += wcsspn(s1, s2);
    if (*s1 == 0) {
        *ptr = NULL;
        return NULL;
    }
    end = s1 + wcscspn(s1, s2);
    if (*end) {
        *end = 0;
        *ptr = end + 1;
    } else {
        *ptr = NULL;
    }
    return s1;
}

wchar_t *wmemchr(const wchar_t *s, wchar_t c, size_t n) {
    for (; n > 0; n--, s++)
        if (*s == c) return (wchar_t *)s;
    return NULL;
}

int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n) {
    for (; n > 0; n--, s1++, s2++)
        if (*s1 != *s2) return *s1 < *s2 ? -1 : 1;
    return 0;
}

wchar_t *wmemcpy(wchar_t *s1, const wchar_t *s2, size_t n) {
    return (wchar_t *)memcpy(s1, s2, n * sizeof(wchar_t));
}

wchar_t *wmemmove(wchar_t *s1, const wchar_t *s2, size_t n) {
    return (wchar_t *)memmove(s1, s2, n * sizeof(wchar_t));
}

wchar_t *wmemset(wchar_t *s, wchar_t c, size_t n) {
    size_t i;
    for (i = 0; i < n; i++) s[i] = c;
    return s;
}

/* --- through the multibyte form --- */

/* s's multibyte form, malloc'd, or NULL (no memory, or a wide character
 * the encoding has not).  With at, at[k] is where the form of s[k] begins,
 * for k up to and including the terminating null's. */
static char *to_mb(const wchar_t *s, size_t **at) {
    size_t n = wcslen(s), k, pos = 0;
    char *out = (char *)malloc(n * 4 + 1);
    size_t *idx = NULL;
    mbstate_t st;
    memset(&st, 0, sizeof st);
    if (at) {
        idx = (size_t *)malloc((n + 1) * sizeof(size_t));
        if (!idx) {
            free(out);
            return NULL;
        }
    }
    if (!out) {
        free(idx);
        return NULL;
    }
    for (k = 0; k < n; k++) {
        size_t r = wcrtomb(out + pos, s[k], &st);
        if (r == (size_t)-1) {
            free(out);
            free(idx);
            return NULL;
        }
        if (idx) idx[k] = pos;
        pos += r;
    }
    out[pos] = 0;
    if (idx) {
        idx[n] = pos;
        *at = idx;
    }
    return out;
}

/* the wide position of the narrow one a conversion stopped at */
static wchar_t *back(const wchar_t *nptr, const char *mb, const char *end, const size_t *at) {
    size_t off = (size_t)(end - mb), k = 0;
    while (at[k] < off) k++;
    return (wchar_t *)(nptr + k);
}

/* A numeric conversion: a wide character the encoding has not cannot be
 * part of a number, so the subject sequence ends before the first one --
 * the conversion reads only the prefix that converts. */
#define WIDE_CONVERT(type, call, zero)                                      \
    {                                                                       \
        size_t n = 0, *at = NULL;                                           \
        wchar_t *pre;                                                       \
        char *mb, *end;                                                     \
        type r;                                                             \
        while (nptr[n] && nptr[n] < 0x80) n++;    /* every number is ASCII */\
        pre = (wchar_t *)malloc((n + 1) * sizeof(wchar_t));                 \
        if (!pre) {                                                         \
            if (endptr) *endptr = (wchar_t *)nptr;                          \
            return zero;                                                    \
        }                                                                   \
        wmemcpy(pre, nptr, n);                                              \
        pre[n] = 0;                                                         \
        mb = to_mb(pre, &at);                                               \
        free(pre);                                                          \
        if (!mb) {                                                          \
            if (endptr) *endptr = (wchar_t *)nptr;                          \
            return zero;                                                    \
        }                                                                   \
        r = call;                                                           \
        if (endptr) *endptr = back(nptr, mb, end, at);                      \
        free(mb);                                                           \
        free(at);                                                           \
        return r;                                                           \
    }

double wcstod(const wchar_t *nptr, wchar_t **endptr) WIDE_CONVERT(double, strtod(mb, &end), 0)
float wcstof(const wchar_t *nptr, wchar_t **endptr) WIDE_CONVERT(float, strtof(mb, &end), 0)
long double wcstold(const wchar_t *nptr, wchar_t **endptr) WIDE_CONVERT(long double, strtold(mb, &end), 0)
long wcstol(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(long, strtol(mb, &end, base), 0)
long long wcstoll(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(long long, strtoll(mb, &end, base), 0)
unsigned long wcstoul(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(unsigned long, strtoul(mb, &end, base), 0)
unsigned long long wcstoull(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(unsigned long long, strtoull(mb, &end, base), 0)
intmax_t wcstoimax(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(intmax_t, strtoimax(mb, &end, base), 0)
uintmax_t wcstoumax(const wchar_t *nptr, wchar_t **endptr, int base) WIDE_CONVERT(uintmax_t, strtoumax(mb, &end, base), 0)

/* LC_COLLATE's order over the multibyte forms; a string the encoding
 * cannot hold sorts by code point (errno EILSEQ, which 7.29.4.4.2 allows) */
int wcscoll(const wchar_t *s1, const wchar_t *s2) {
    char *a = to_mb(s1, NULL), *b = to_mb(s2, NULL);
    int r;
    if (a && b) r = strcoll(a, b);
    else r = wcscmp(s1, s2);
    free(a);
    free(b);
    return r;
}

/* strxfrm's bytes, a wide character each: wcscmp of two results orders
 * as strcmp of the byte strings, so as wcscoll of the originals */
size_t wcsxfrm(wchar_t *s1, const wchar_t *s2, size_t n) {
    char *mb = to_mb(s2, NULL);
    char *x;
    size_t len, i;
    if (!mb) {
        len = wcslen(s2);
        if (len < n) wcscpy(s1, s2);
        return len;
    }
    len = strxfrm(NULL, mb, 0);
    x = (char *)malloc(len + 1);
    if (!x) {
        free(mb);
        return (size_t)-1;
    }
    strxfrm(x, mb, len + 1);
    if (len < n) {
        for (i = 0; i <= len; i++) s1[i] = (unsigned char)x[i];
    }
    free(x);
    free(mb);
    return len;
}

size_t wcsftime(wchar_t *s, size_t maxsize, const wchar_t *format, const struct tm *timeptr) {
    char *fmt = to_mb(format, NULL);
    size_t cap = 64, r = 0;
    char *buf = NULL;
    if (!fmt) return 0;
    /* strftime's 0 is "too long" or an empty result: grow until it fits
     * (an empty result is found when the format is empty, or when a
     * buffer as large as the wide limit allows still gives 0) */
    for (;;) {
        size_t got;
        char *nb = (char *)realloc(buf, cap);
        if (!nb) break;
        buf = nb;
        got = strftime(buf, cap, fmt, timeptr);
        if (got > 0 || fmt[0] == 0 || (cap - 64) / 4 > maxsize) {
            mbstate_t st;
            const char *src = buf;
            size_t w;
            if (got == 0) buf[0] = 0;
            memset(&st, 0, sizeof st);
            w = mbsrtowcs(NULL, &src, 0, &st);
            if (w != (size_t)-1 && w < maxsize) {
                memset(&st, 0, sizeof st);
                src = buf;
                mbsrtowcs(s, &src, maxsize, &st);
                r = w;
            }
            break;
        }
        cap *= 2;
    }
    free(buf);
    free(fmt);
    return r;
}
