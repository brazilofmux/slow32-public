/* The multibyte and wide characters (C11 7.22.7, 7.22.8, 7.28.1, 7.29.6),
 * for both C libraries.
 *
 * The encoding is LC_CTYPE's (__s32_mb_utf8: runtime/locale_state.c, the
 * self-hosted library's locale_c.c):
 *   "C", "POSIX"   single bytes, ASCII: a byte past 0x7F is no character
 *                  (EILSEQ), as glibc has it
 *   the others     UTF-8 -- C.UTF-8 and every named locale -- and a wide
 *                  character is the code point.  Strict: an overlong form,
 *                  a surrogate or a value past U+10FFFF is EILSEQ either way.
 * MB_CUR_MAX is 1 and 4.
 *
 * mbstate_t's zero is the initial state.  Otherwise __count holds, for a
 * character begun and not ended, (lead byte << 8) | bytes still wanted,
 * and __value its bits so far; mbrtoc16's low surrogate still to come, or
 * c16rtomb's high surrogate waiting for its partner, is a flag bit with
 * the half in __value. */
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <uchar.h>
#include <wchar.h>

int __s32_mb_utf8(void);

#define ST_LOW_PENDING  0x40000000u     /* mbrtoc16: a low surrogate to give */
#define ST_HIGH_WAITING 0x20000000u     /* c16rtomb: a high surrogate held */

size_t __s32_mb_cur_max(void) {
    return __s32_mb_utf8() ? 4 : 1;
}

static size_t ilseq(mbstate_t *ps) {
    ps->__count = 0;
    ps->__value = 0;
    errno = EILSEQ;
    return (size_t)-1;
}

/* the bounds of a UTF-8 sequence's second byte, from its lead */
static int second_ok(unsigned lead, unsigned b) {
    if (lead == 0xE0) return b >= 0xA0 && b <= 0xBF;
    if (lead == 0xED) return b >= 0x80 && b <= 0x9F;  /* no surrogates */
    if (lead == 0xF0) return b >= 0x90 && b <= 0xBF;
    if (lead == 0xF4) return b >= 0x80 && b <= 0x8F;  /* none past U+10FFFF */
    return b >= 0x80 && b <= 0xBF;
}

size_t mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps) {
    static mbstate_t internal;
    const unsigned char *p = (const unsigned char *)s;
    size_t used = 0;
    unsigned lead, need, v;
    if (!ps) ps = &internal;
    if (!s) {
        p = (const unsigned char *)"";
        n = 1;
        pwc = NULL;
    }
    if (n == 0) return (size_t)-2;
    if (!__s32_mb_utf8()) {
        if (p[0] >= 0x80) return ilseq(ps);
        if (pwc) *pwc = p[0];
        return p[0] ? 1 : 0;
    }
    if (ps->__count & 0xFF) {                   /* a character begun */
        lead = ps->__count >> 8 & 0xFF;
        need = ps->__count & 0xFF;
        v = ps->__value;
    } else {
        unsigned b = p[used++];
        if (b < 0x80) {
            if (pwc) *pwc = b;
            ps->__count = 0;
            return b ? 1 : 0;
        }
        if (b >= 0xC2 && b <= 0xDF) { need = 1; v = b & 0x1F; }
        else if (b >= 0xE0 && b <= 0xEF) { need = 2; v = b & 0x0F; }
        else if (b >= 0xF0 && b <= 0xF4) { need = 3; v = b & 0x07; }
        else return ilseq(ps);
        lead = b;
    }
    while (need > 0) {
        unsigned b;
        unsigned total = lead >= 0xF0 ? 3 : lead >= 0xE0 ? 2 : 1;
        if (used == n) {
            ps->__count = lead << 8 | need;
            ps->__value = v;
            return (size_t)-2;
        }
        b = p[used++];
        if (need == total ? !second_ok(lead, b) : (b & 0xC0) != 0x80) return ilseq(ps);
        v = v << 6 | (b & 0x3F);
        need--;
    }
    ps->__count = 0;
    ps->__value = 0;
    if (pwc) *pwc = (wchar_t)v;
    return v ? used : 0;
}

/* the UTF-8 form of c into s (room for 4); 0 when c is no character */
static size_t utf8_put(char *s, unsigned c) {
    unsigned char *o = (unsigned char *)s;
    if (c < 0x80) { o[0] = (unsigned char)c; return 1; }
    if (c < 0x800) {
        o[0] = (unsigned char)(0xC0 | c >> 6);
        o[1] = (unsigned char)(0x80 | (c & 0x3F));
        return 2;
    }
    if (c >= 0xD800 && c <= 0xDFFF) return 0;
    if (c < 0x10000) {
        o[0] = (unsigned char)(0xE0 | c >> 12);
        o[1] = (unsigned char)(0x80 | (c >> 6 & 0x3F));
        o[2] = (unsigned char)(0x80 | (c & 0x3F));
        return 3;
    }
    if (c > 0x10FFFF) return 0;
    o[0] = (unsigned char)(0xF0 | c >> 18);
    o[1] = (unsigned char)(0x80 | (c >> 12 & 0x3F));
    o[2] = (unsigned char)(0x80 | (c >> 6 & 0x3F));
    o[3] = (unsigned char)(0x80 | (c & 0x3F));
    return 4;
}

size_t wcrtomb(char *s, wchar_t wc, mbstate_t *ps) {
    static mbstate_t internal;
    char buf[4];
    size_t k;
    unsigned c = (unsigned)wc;
    if (!ps) ps = &internal;
    if (!s) {
        s = buf;
        c = 0;
    }
    ps->__count = 0;
    ps->__value = 0;
    if (!__s32_mb_utf8()) {
        if (c >= 0x80) return ilseq(ps);
        s[0] = (char)c;
        return 1;
    }
    k = utf8_put(s, c);
    return k ? k : ilseq(ps);
}

int mbsinit(const mbstate_t *ps) {
    return !ps || ps->__count == 0;
}

size_t mbrlen(const char *s, size_t n, mbstate_t *ps) {
    static mbstate_t internal;
    return mbrtowc(NULL, s, n, ps ? ps : &internal);
}

wint_t btowc(int c) {
    if (c == EOF || c < 0 || c >= 0x80) return WEOF;    /* a byte past ASCII starts no character alone, in either encoding */
    return (wint_t)c;
}

int wctob(wint_t c) {
    return c < 0x80 ? (int)c : EOF;
}

/* --- strings --- */

size_t mbsnrtowcs(wchar_t *dst, const char **src, size_t nms, size_t len, mbstate_t *ps) {
    static mbstate_t internal;
    const char *s = *src;
    size_t count = 0;
    if (!ps) ps = &internal;
    while (!dst || count < len) {
        wchar_t wc;
        size_t r = mbrtowc(&wc, s, nms, ps);
        if (r == (size_t)-1) {
            if (dst) *src = s;
            return (size_t)-1;
        }
        if (r == (size_t)-2) {                  /* nms ran out mid-character */
            if (dst) *src = s + nms;
            return count;
        }
        if (dst) dst[count] = wc;
        if (r == 0) {
            if (dst) *src = NULL;
            return count;
        }
        s += r;
        nms -= r;
        count++;
    }
    *src = s;
    return count;
}

size_t mbsrtowcs(wchar_t *dst, const char **src, size_t len, mbstate_t *ps) {
    return mbsnrtowcs(dst, src, (size_t)-1, len, ps);
}

size_t wcsnrtombs(char *dst, const wchar_t **src, size_t nwc, size_t len, mbstate_t *ps) {
    static mbstate_t internal;
    const wchar_t *w = *src;
    size_t count = 0;
    if (!ps) ps = &internal;
    for (; nwc > 0; nwc--, w++) {
        char buf[4];
        size_t r = wcrtomb(buf, *w, ps);
        if (r == (size_t)-1) {
            if (dst) *src = w;
            return (size_t)-1;
        }
        if (*w == 0) {
            if (dst) {
                if (count >= len) break;    /* no room for the null: stop before it */
                dst[count] = 0;
                *src = NULL;
            }
            return count;
        }
        if (dst) {
            if (count + r > len) break;     /* a whole character or none */
            memcpy(dst + count, buf, r);
        }
        count += r;
    }
    if (dst) *src = w;
    return count;
}

size_t wcsrtombs(char *dst, const wchar_t **src, size_t len, mbstate_t *ps) {
    return wcsnrtombs(dst, src, (size_t)-1, len, ps);
}

/* --- <stdlib.h>'s, each with its own state (7.22.7p1) --- */

int mblen(const char *s, size_t n) {
    static mbstate_t st;
    size_t r;
    if (!s) {
        memset(&st, 0, sizeof st);
        return 0;                           /* no state-dependent encodings */
    }
    r = mbrtowc(NULL, s, n, &st);
    if (r == (size_t)-2) {
        memset(&st, 0, sizeof st);
        errno = EILSEQ;
        return -1;
    }
    return (int)r;
}

int mbtowc(wchar_t *pwc, const char *s, size_t n) {
    static mbstate_t st;
    size_t r;
    if (!s) {
        memset(&st, 0, sizeof st);
        return 0;
    }
    r = mbrtowc(pwc, s, n, &st);
    if (r == (size_t)-2) {
        memset(&st, 0, sizeof st);
        errno = EILSEQ;
        return -1;
    }
    return (int)r;
}

int wctomb(char *s, wchar_t wc) {
    mbstate_t st;
    if (!s) return 0;
    memset(&st, 0, sizeof st);
    return (int)wcrtomb(s, wc, &st);
}

size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n) {
    mbstate_t st;
    memset(&st, 0, sizeof st);
    return mbsrtowcs(pwcs, &s, n, &st);
}

size_t wcstombs(char *s, const wchar_t *pwcs, size_t n) {
    mbstate_t st;
    memset(&st, 0, sizeof st);
    return wcsrtombs(s, &pwcs, n, &st);
}

/* --- <uchar.h> --- */

size_t mbrtoc32(char32_t *pc32, const char *s, size_t n, mbstate_t *ps) {
    static mbstate_t internal;
    wchar_t wc;
    size_t r = mbrtowc(&wc, s, n, ps ? ps : &internal);
    if (pc32 && r <= 4) *pc32 = (char32_t)wc;
    return r;
}

size_t c32rtomb(char *s, char32_t c32, mbstate_t *ps) {
    static mbstate_t internal;
    return wcrtomb(s, (wchar_t)c32, ps ? ps : &internal);
}

size_t mbrtoc16(char16_t *pc16, const char *s, size_t n, mbstate_t *ps) {
    static mbstate_t internal;
    wchar_t wc;
    size_t r;
    if (!ps) ps = &internal;
    if (!s) {
        s = "";
        n = 1;
        pc16 = NULL;
    }
    if (ps->__count & ST_LOW_PENDING) {         /* the second half of the last */
        if (pc16) *pc16 = (char16_t)ps->__value;
        ps->__count = 0;
        ps->__value = 0;
        return (size_t)-3;
    }
    r = mbrtowc(&wc, s, n, ps);
    if (r > 4) return r;
    if ((unsigned)wc >= 0x10000) {
        unsigned v = (unsigned)wc - 0x10000;
        if (pc16) *pc16 = (char16_t)(0xD800 + (v >> 10));
        ps->__count = ST_LOW_PENDING;
        ps->__value = 0xDC00 + (v & 0x3FF);
    } else if (pc16) {
        *pc16 = (char16_t)wc;
    }
    return r;
}

size_t c16rtomb(char *s, char16_t c16, mbstate_t *ps) {
    static mbstate_t internal;
    char buf[4];
    unsigned c = c16;
    if (!ps) ps = &internal;
    if (!s) {
        s = buf;
        c = 0;
    }
    if (ps->__count & ST_HIGH_WAITING) {
        unsigned hi = ps->__value;
        if (c < 0xDC00 || c > 0xDFFF) return ilseq(ps);
        ps->__count = 0;
        ps->__value = 0;
        return wcrtomb(s, (wchar_t)(0x10000 + ((hi - 0xD800) << 10) + (c - 0xDC00)), ps);
    }
    if (c >= 0xD800 && c <= 0xDBFF) {
        if (!__s32_mb_utf8()) return ilseq(ps);
        ps->__count = ST_HIGH_WAITING;
        ps->__value = c;
        return 0;
    }
    return wcrtomb(s, (wchar_t)c, ps);
}
