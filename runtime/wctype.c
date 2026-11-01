/* <wctype.h> (C11 7.30), for both C libraries.
 *
 * In the "C" locale the wide characters are ASCII's, classified and
 * mapped as <ctype.h> has them; nothing past 0x7F is in a class.  In the
 * UTF-8 locales they are Unicode's: runtime/utf/wctype_tables.h, generated
 * by runtime/utf/gen-wctype.py from the Unicode data libutf is built from
 * and checked, code point for code point, against glibc's C.UTF-8.  For
 * the ASCII range the two agree. */
#include <string.h>
#include <wctype.h>
#include "utf/wctype_tables.h"

int __s32_mb_utf8(void);

#define N_RUNS (sizeof __s32_wc_runs / sizeof __s32_wc_runs[0])

static unsigned classes(wint_t c) {
    unsigned lo = 0, hi = N_RUNS;
    if (c >= 0x110000 || (c >= 0x80 && !__s32_mb_utf8())) return 0;
    /* the last run starting at or before c */
    while (hi - lo > 1) {
        unsigned mid = (lo + hi) / 2;
        if ((__s32_wc_runs[mid] >> 11) <= c) lo = mid;
        else hi = mid;
    }
    return __s32_wc_runs[lo] & 0x7FF;
}

static wint_t map(wint_t c, const struct __s32_wcmap *m, unsigned n) {
    unsigned lo = 0, hi = n;
    if (c >= 0x110000 || (c >= 0x80 && !__s32_mb_utf8())) return c;
    while (lo < hi) {
        unsigned mid = (lo + hi) / 2;
        if (m[mid].hi < c) lo = mid + 1;
        else hi = mid;
    }
    if (lo < n && m[lo].lo <= c && (c - m[lo].lo) % (unsigned)m[lo].stride == 0)
        return (wint_t)((int)c + m[lo].delta);
    return c;
}

int iswalnum(wint_t c)  { return (classes(c) & (__S32_WC_ALPHA | __S32_WC_DIGIT)) != 0; }
int iswalpha(wint_t c)  { return (classes(c) & __S32_WC_ALPHA) != 0; }
int iswblank(wint_t c)  { return (classes(c) & __S32_WC_BLANK) != 0; }
int iswcntrl(wint_t c)  { return (classes(c) & __S32_WC_CNTRL) != 0; }
int iswdigit(wint_t c)  { return c >= '0' && c <= '9'; }
int iswgraph(wint_t c)  { return (classes(c) & __S32_WC_GRAPH) != 0; }
int iswlower(wint_t c)  { return (classes(c) & __S32_WC_LOWER) != 0; }
int iswprint(wint_t c)  { return (classes(c) & __S32_WC_PRINT) != 0; }
int iswpunct(wint_t c)  { return (classes(c) & __S32_WC_PUNCT) != 0; }
int iswspace(wint_t c)  { return (classes(c) & __S32_WC_SPACE) != 0; }
int iswupper(wint_t c)  { return (classes(c) & __S32_WC_UPPER) != 0; }
int iswxdigit(wint_t c) { return (classes(c) & __S32_WC_XDIGIT) != 0; }

static const struct { const char *name; unsigned bits; } class_names[] = {
    { "alnum", __S32_WC_ALPHA | __S32_WC_DIGIT }, { "alpha", __S32_WC_ALPHA },
    { "blank", __S32_WC_BLANK }, { "cntrl", __S32_WC_CNTRL },
    { "digit", __S32_WC_DIGIT }, { "graph", __S32_WC_GRAPH },
    { "lower", __S32_WC_LOWER }, { "print", __S32_WC_PRINT },
    { "punct", __S32_WC_PUNCT }, { "space", __S32_WC_SPACE },
    { "upper", __S32_WC_UPPER }, { "xdigit", __S32_WC_XDIGIT },
};

wctype_t wctype(const char *property) {
    unsigned i;
    for (i = 0; i < sizeof class_names / sizeof class_names[0]; i++)
        if (strcmp(property, class_names[i].name) == 0) return class_names[i].bits;
    return 0;
}

int iswctype(wint_t c, wctype_t desc) {
    return (classes(c) & desc) != 0;
}

wint_t towupper(wint_t c) {
    return map(c, __s32_wc_toupper, sizeof __s32_wc_toupper / sizeof __s32_wc_toupper[0]);
}

wint_t towlower(wint_t c) {
    return map(c, __s32_wc_tolower, sizeof __s32_wc_tolower / sizeof __s32_wc_tolower[0]);
}

wctrans_t wctrans(const char *property) {
    if (strcmp(property, "toupper") == 0) return 1;
    if (strcmp(property, "tolower") == 0) return 2;
    return 0;
}

wint_t towctrans(wint_t c, wctrans_t desc) {
    if (desc == 1) return towupper(c);
    if (desc == 2) return towlower(c);
    return c;
}
