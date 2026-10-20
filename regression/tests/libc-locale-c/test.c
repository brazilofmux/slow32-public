/* The C locale through <locale.h> and <langinfo.h>: setlocale,
 * localeconv, the POSIX 2008 locale objects, nl_langinfo, a few *_l
 * functions, asprintf.  expected.txt is a host build's output; only
 * what the C locale fixes is printed (the codeset name and YESEXPR vary). */
#define _GNU_SOURCE
#include <errno.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#ifdef __APPLE__
#include <xlocale.h>
#endif

int main(void) {
    const char *r = setlocale(LC_ALL, NULL);
    printf("query: %s\n", r ? r : "(null)");
    r = setlocale(LC_ALL, "C");
    printf("C: %s\n", r ? r : "(null)");
    r = setlocale(LC_NUMERIC, "POSIX");
    printf("POSIX numeric: %s\n", r ? "set" : "(null)");   /* its name is implementation-defined */
    r = setlocale(LC_ALL, "xx_QQ.nowhere");
    printf("unknown: %s\n", r ? r : "(null)");

    struct lconv *lc = localeconv();
    printf("lconv: [%s] [%s] [%s] frac=%d\n", lc->decimal_point, lc->thousands_sep,
           lc->currency_symbol, lc->frac_digits == CHAR_MAX);

    errno = 0;
    locale_t bad = newlocale(LC_ALL_MASK, "xx_QQ.nowhere", (locale_t)0);
    printf("newlocale unknown: %s errno=%s\n", bad ? "made" : "null", errno == ENOENT ? "ENOENT" : "other");
    locale_t c = newlocale(LC_ALL_MASK, "C", (locale_t)0);
    printf("newlocale C: %s\n", c ? "made" : "null");
    locale_t old = uselocale(c);
    printf("uselocale was global: %d\n", old == LC_GLOBAL_LOCALE);
    printf("uselocale now: %d\n", uselocale((locale_t)0) == c);
    uselocale(old);
    printf("toupper_l: %c%c isdigit_l: %d\n", toupper_l('q', c), tolower_l('Q', c), !!isdigit_l('7', c));
    printf("strcoll_l: %d strtod_l: %g\n", strcoll_l("abc", "abd", c) < 0, strtod_l("2.5e2", NULL, c));
    struct tm tm;
    memset(&tm, 0, sizeof tm);
    tm.tm_year = 126; tm.tm_mon = 9; tm.tm_mday = 19; tm.tm_wday = 1;
    char buf[64];
    strftime_l(buf, sizeof buf, "%A %d %B %Y", &tm, c);
    printf("strftime_l: %s\n", buf);
    freelocale(c);

    printf("days: %s %s | %s %s\n", nl_langinfo(DAY_1), nl_langinfo(DAY_7),
           nl_langinfo(ABDAY_2), nl_langinfo(ABDAY_6));
    printf("months: %s %s | %s %s\n", nl_langinfo(MON_1), nl_langinfo(MON_12),
           nl_langinfo(ABMON_5), nl_langinfo(ABMON_9));
    printf("formats: [%s] [%s] [%s] [%s]\n", nl_langinfo(D_T_FMT), nl_langinfo(D_FMT),
           nl_langinfo(T_FMT), nl_langinfo(T_FMT_AMPM));
    printf("am/pm: %s %s radix: %s\n", nl_langinfo(AM_STR), nl_langinfo(PM_STR), nl_langinfo(RADIXCHAR));

    char *s = NULL;
    int n = asprintf(&s, "%s-%04d-%.2f", "lamp", 42, 3.14159);
    printf("asprintf: %d [%s]\n", n, s ? s : "(null)");
    free(s);
    return 0;
}
