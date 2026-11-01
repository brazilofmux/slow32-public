/* setlocale and localeconv (C11 7.11) for the self-hosted library, whose
 * locales are "C" -- "C" and "POSIX" name it, and "" (the native locale)
 * is it too -- and "C.UTF-8", the C locale with UTF-8 for its encoding
 * (LC_CTYPE: runtime/wchar.c, wctype.c).  Any other name is refused (a
 * null pointer, the locale unchanged).  localeconv gives the "C" locale's
 * values (7.11.2.1p3). */
#include <limits.h>
#include <locale.h>
#include <string.h>

static char lc_empty[1];
static char lc_point[2] = ".";
static char lc_c[2] = "C";
static char lc_c_utf8[8] = "C.UTF-8";
static int ctype_utf8;      /* LC_CTYPE is C.UTF-8 */

int __s32_mb_utf8(void) {
    return ctype_utf8;
}

char *setlocale(int category, const char *locale) {
    int utf8;
    if (category < LC_ALL || category > LC_TIME) return (char *)0;
    if (locale) {
        if (locale[0] == 0 || strcmp(locale, "C") == 0 || strcmp(locale, "POSIX") == 0) utf8 = 0;
        else if (strcmp(locale, "C.UTF-8") == 0 || strcmp(locale, "C.utf8") == 0) utf8 = 1;
        else return (char *)0;
        if (category == LC_ALL || category == LC_CTYPE) ctype_utf8 = utf8;
    }
    if ((category == LC_ALL || category == LC_CTYPE) && ctype_utf8) return lc_c_utf8;
    return lc_c;
}

struct lconv *localeconv(void) {
    static struct lconv c;
    c.decimal_point = lc_point;
    c.thousands_sep = lc_empty;
    c.grouping = lc_empty;
    c.mon_decimal_point = lc_empty;
    c.mon_thousands_sep = lc_empty;
    c.mon_grouping = lc_empty;
    c.positive_sign = lc_empty;
    c.negative_sign = lc_empty;
    c.currency_symbol = lc_empty;
    c.int_curr_symbol = lc_empty;
    c.frac_digits = CHAR_MAX;
    c.p_cs_precedes = CHAR_MAX;
    c.n_cs_precedes = CHAR_MAX;
    c.p_sep_by_space = CHAR_MAX;
    c.n_sep_by_space = CHAR_MAX;
    c.p_sign_posn = CHAR_MAX;
    c.n_sign_posn = CHAR_MAX;
    c.int_frac_digits = CHAR_MAX;
    c.int_p_cs_precedes = CHAR_MAX;
    c.int_n_cs_precedes = CHAR_MAX;
    c.int_p_sep_by_space = CHAR_MAX;
    c.int_n_sep_by_space = CHAR_MAX;
    c.int_p_sign_posn = CHAR_MAX;
    c.int_n_sign_posn = CHAR_MAX;
    return &c;
}
