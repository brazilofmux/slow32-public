/*
 * The locale state -- the C locale's record, the global locale and
 * uselocale's -- and what reads it: localeconv, nl_langinfo, strcoll and
 * strxfrm.  Apart from locale.c (setlocale and the locale objects) so
 * that printf, strtod, strftime and strcoll, which ask the current
 * locale, do not bring setlocale into a program that never calls it: a
 * program may have a setlocale of its own (a COBOL program can be named
 * that), and one archive member defining both would collide with it.
 */

#include <limits.h>
#include <locale.h>
#include <string.h>
#include "locale_impl.h"

/* locales.s32o's, when a program links it: null otherwise */
extern int __s32_locale_strcoll(const struct __s32_locdata *d, const char *a, const char *b) __attribute__((weak));
extern size_t __s32_locale_strxfrm(const struct __s32_locdata *d, char *dest, const char *src, size_t n) __attribute__((weak));

const struct __s32_locdata __s32_c_data = {
    "C",
    {
        [CODESET] = "ANSI_X3.4-1968",
        [D_T_FMT] = "%a %b %e %H:%M:%S %Y",
        [D_FMT] = "%m/%d/%y",
        [T_FMT] = "%H:%M:%S",
        [T_FMT_AMPM] = "%I:%M:%S %p",
        [AM_STR] = "AM", [PM_STR] = "PM",
        [DAY_1] = "Sunday", [DAY_2] = "Monday", [DAY_3] = "Tuesday",
        [DAY_4] = "Wednesday", [DAY_5] = "Thursday", [DAY_6] = "Friday",
        [DAY_7] = "Saturday",
        [ABDAY_1] = "Sun", [ABDAY_2] = "Mon", [ABDAY_3] = "Tue", [ABDAY_4] = "Wed",
        [ABDAY_5] = "Thu", [ABDAY_6] = "Fri", [ABDAY_7] = "Sat",
        [MON_1] = "January", [MON_2] = "February", [MON_3] = "March",
        [MON_4] = "April", [MON_5] = "May", [MON_6] = "June", [MON_7] = "July",
        [MON_8] = "August", [MON_9] = "September", [MON_10] = "October",
        [MON_11] = "November", [MON_12] = "December",
        [ABMON_1] = "Jan", [ABMON_2] = "Feb", [ABMON_3] = "Mar", [ABMON_4] = "Apr",
        [ABMON_5] = "May", [ABMON_6] = "Jun", [ABMON_7] = "Jul", [ABMON_8] = "Aug",
        [ABMON_9] = "Sep", [ABMON_10] = "Oct", [ABMON_11] = "Nov", [ABMON_12] = "Dec",
        [RADIXCHAR] = ".", [THOUSEP] = "",
        [YESEXPR] = "^[yY]", [NOEXPR] = "^[nN]", [CRNCYSTR] = "-",
    },
    {
        ".", "", "", "", "", "", "", "", "", "",
        CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX,
        CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX
    }
};

#define C_LOCALE_INIT { \
    { 0, &__s32_c_data, &__s32_c_data, &__s32_c_data, &__s32_c_data, &__s32_c_data, &__s32_c_data }, \
    { "", "C", "C", "C", "C", "C", "C" }, \
    { ".", "", "", "", "", "", "", "", "", "", \
      CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, \
      CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX } }

struct __s32_locale __s32_c_locale = C_LOCALE_INIT;     /* newlocale's for "C"; never freed */
struct __s32_locale __s32_global_locale = C_LOCALE_INIT; /* setlocale's */

/* uselocale's per-thread locale (one thread); null means the global one */
locale_t __s32_current_locale;


struct lconv *localeconv(void) {
    return &__s32_cur()->conv;
}

locale_t uselocale(locale_t loc) {
    locale_t old = __s32_current_locale ? __s32_current_locale : LC_GLOBAL_LOCALE;
    if (loc) __s32_current_locale = (loc == LC_GLOBAL_LOCALE) ? NULL : loc;
    return old;
}

/* ---------------------------------------------------------------- */

/* the category an nl_langinfo item belongs to */
static int item_cat(nl_item item) {
    switch (item) {
    case CODESET: return LC_CTYPE;
    case RADIXCHAR: case THOUSEP: return LC_NUMERIC;
    case YESEXPR: case NOEXPR: return LC_MESSAGES;
    case CRNCYSTR: return LC_MONETARY;
    default: return LC_TIME;
    }
}

char *nl_langinfo_l(nl_item item, locale_t loc) {
    if (item < 0 || item >= __S32_NL_ITEMS) return (char *)"";
    const char *v = __s32_obj(loc)->data[item_cat(item)]->info[item];
    return (char *)(v ? v : "");
}

char *nl_langinfo(nl_item item) {
    return nl_langinfo_l(item, __s32_cur());
}

/* ---------------------------------------------------------------- */
/* LC_COLLATE: the C locale's is byte order */

int strcoll_l(const char *a, const char *b, locale_t l) {
    const struct __s32_locdata *d = __s32_obj(l)->data[LC_COLLATE];
    if (d != &__s32_c_data && __s32_locale_strcoll) return __s32_locale_strcoll(d, a, b);
    return strcmp(a, b);
}

size_t strxfrm_l(char *dest, const char *src, size_t n, locale_t l) {
    const struct __s32_locdata *d = __s32_obj(l)->data[LC_COLLATE];
    if (d != &__s32_c_data && __s32_locale_strxfrm) return __s32_locale_strxfrm(d, dest, src, n);
    size_t len = strlen(src);
    if (n > 0) {
        size_t i = 0;
        for (; i < n - 1 && i < len; i++) dest[i] = src[i];
        dest[i] = '\0';
    }
    return len;
}

int strcoll(const char *a, const char *b) { return strcoll_l(a, b, __s32_cur()); }
size_t strxfrm(char *dest, const char *src, size_t n) { return strxfrm_l(dest, src, n, __s32_cur()); }
