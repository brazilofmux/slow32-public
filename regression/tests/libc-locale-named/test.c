/* The named locales (runtime/locales.s32o): names and how they resolve,
 * the environment's locale, LC_TIME through strftime and nl_langinfo,
 * LC_NUMERIC through printf and strtod, localeconv, collation, and the
 * locale objects.  The values are CLDR 46's, as runtime/gen_locales.py
 * renders them; a host's C library has its own locale data, so this
 * expected.txt is pinned, not a host build's.  env.txt gives the guest
 * LANG=nb_NO.UTF-8, LC_TIME=fi_FI.UTF-8 and LC_COLLATE=sv-SE. */
#include <errno.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

static struct tm when(void) {
    struct tm tm;
    memset(&tm, 0, sizeof tm);
    tm.tm_year = 127; tm.tm_mon = 2; tm.tm_mday = 7; tm.tm_wday = 0; tm.tm_yday = 65;
    tm.tm_hour = 16; tm.tm_min = 4; tm.tm_sec = 9;
    return tm;
}

static void show_time(const char *label) {
    struct tm tm = when();
    char buf[160];
    strftime(buf, sizeof buf, "%c | %x | %X | %A %a %B %b | %p | %-d.%-m. %e", &tm);
    printf("%-8s %s\n", label, buf);
}

static const char *words[] = { "zebra", "\303\244pple", "apple", "Zulu", "\303\266l", "orm", "\303\245ker" };
#define NWORDS (sizeof words / sizeof words[0])

static int by_coll(const void *a, const void *b) { return strcoll(*(const char *const *)a, *(const char *const *)b); }

static void show_order(const char *label) {
    const char *w[NWORDS];
    memcpy(w, words, sizeof w);
    qsort(w, NWORDS, sizeof w[0], by_coll);
    printf("%-8s", label);
    for (size_t i = 0; i < NWORDS; i++) printf(" %s", w[i]);
    /* the strxfrm keys sort the same way under strcmp */
    int agree = 1;
    for (size_t i = 0; i + 1 < NWORDS; i++) {
        char ka[256], kb[256];
        size_t na = strxfrm(ka, w[i], sizeof ka), nb = strxfrm(kb, w[i + 1], sizeof kb);
        if (na >= sizeof ka || nb >= sizeof kb || strcmp(ka, kb) > 0) agree = 0;
        if (strlen(ka) != na) agree = 0;
    }
    printf("  [strxfrm %s]\n", agree ? "agrees" : "DISAGREES");
}

int main(void) {
    /* ---- names */
    const char *names[] = { "de_DE.UTF-8", "de-CH", "sr_Latn_RS", "pt_BR.utf8", "C.UTF-8",
                            "de_DE.ISO-8859-1", "xx_QQ", "en_US@calendar", "" };
    for (size_t i = 0; i < sizeof names / sizeof names[0]; i++) {
        errno = 0;
        const char *r = setlocale(LC_TIME, names[i]);
        printf("setlocale(LC_TIME, \"%s\") = %s%s\n", names[i], r ? r : "(null)",
               r || errno != ENOENT ? "" : " ENOENT");
    }

    /* ---- the environment: LANG, then the categories that have their own */
    printf("env: %s\n", setlocale(LC_ALL, ""));
    printf("LC_TIME %s, LC_NUMERIC %s\n", setlocale(LC_TIME, NULL), setlocale(LC_NUMERIC, NULL));
    char *saved = strdup(setlocale(LC_ALL, NULL));
    show_time("fi");
    show_order("sv");
    printf("%.3f %s\n", 1234.5, nl_langinfo(CODESET));
    setlocale(LC_ALL, "C");
    printf("C again: %s\n", setlocale(LC_ALL, NULL));
    printf("back: %s\n", setlocale(LC_ALL, saved) ? "taken" : "REFUSED");
    printf("LC_TIME %s, LC_COLLATE %s\n", setlocale(LC_TIME, NULL), setlocale(LC_COLLATE, NULL));
    free(saved);
    printf("bad composite: %s\n", setlocale(LC_ALL, "LC_CTYPE=de;LC_BOGUS=fr") ? "taken" : "(null)");
    printf("unchanged: %s\n", setlocale(LC_TIME, NULL));

    /* ---- LC_TIME */
    setlocale(LC_ALL, "C");
    const char *times[] = { "C", "en", "de", "fr", "pl", "hu", "fr_CA" };
    for (size_t i = 0; i < sizeof times / sizeof times[0]; i++) {
        setlocale(LC_TIME, times[i]);
        show_time(times[i]);
    }
    setlocale(LC_TIME, "de");
    printf("de: D_T_FMT [%s] D_FMT [%s] T_FMT [%s] AMPM [%s] %s %s\n", nl_langinfo(D_T_FMT), nl_langinfo(D_FMT),
           nl_langinfo(T_FMT), nl_langinfo(T_FMT_AMPM), nl_langinfo(MON_3), nl_langinfo(ABDAY_1));
    printf("de time, C numbers: %.2f %s\n", 2.75, nl_langinfo(RADIXCHAR));

    /* ---- LC_NUMERIC: printf and strtod */
    setlocale(LC_NUMERIC, "de_AT");
    char *end;
    double a = strtod("2,5x", &end), b = strtod("2.5", NULL);
    printf("de_AT: %.2f %g %e | strtod 2,5 = %g (stops at '%c'), 2.5 = %g\n", 3.14159, 0.5, 1500.0, a, *end, b);
    char buf[64];
    snprintf(buf, sizeof buf, "%.1f", 7.25);
    printf("snprintf: %s\n", buf);

    /* ---- localeconv */
    const char *convs[] = { "C", "de", "en", "fr", "pl", "pt", "eo" };
    for (size_t i = 0; i < sizeof convs / sizeof convs[0]; i++) {
        setlocale(LC_ALL, convs[i]);
        struct lconv *lc = localeconv();
        printf("%-3s [%s] [%s] g=%d | [%s] [%s] [%s] [%s] [%s] g=%d frac=%d pre=%d sep=%d sign=%d [%s]\n", convs[i],
               lc->decimal_point, lc->thousands_sep, lc->grouping[0], lc->int_curr_symbol, lc->currency_symbol,
               lc->mon_decimal_point, lc->mon_thousands_sep, lc->negative_sign, lc->mon_grouping[0],
               lc->frac_digits, lc->p_cs_precedes, lc->p_sep_by_space, lc->n_sign_posn, nl_langinfo(CRNCYSTR));
    }

    /* ---- LC_COLLATE */
    const char *colls[] = { "C", "de", "sv", "en" };
    for (size_t i = 0; i < sizeof colls / sizeof colls[0]; i++) {
        setlocale(LC_COLLATE, colls[i]);
        show_order(colls[i]);
    }

    /* ---- locale objects */
    setlocale(LC_ALL, "C");
    locale_t tr = newlocale(LC_TIME_MASK | LC_NUMERIC_MASK, "tr_TR.UTF-8", (locale_t)0);
    printf("newlocale tr: %s\n", tr ? "made" : "null");
    struct tm tm = when();
    strftime_l(buf, sizeof buf, "%A %d %B %Y", &tm, tr);
    printf("strftime_l: %s | %s\n", buf, nl_langinfo_l(MON_12, tr));
    printf("strtod_l: %g strcoll_l: %d\n", strtod_l("0,125", NULL, tr), strcoll_l("b", "a", tr) > 0);
    locale_t old = uselocale(tr);
    printf("under tr: %.1f", 9.5);
    uselocale(old);
    printf(" | global: %.1f\n", 9.5);
    locale_t both = newlocale(LC_COLLATE_MASK, "de", tr);      /* tr is consumed */
    printf("added collation: %s, radix still [%s]\n", both ? "made" : "null", nl_langinfo_l(RADIXCHAR, both));
    errno = 0;
    printf("refused: %s errno=%s\n", newlocale(LC_ALL_MASK, "xx", (locale_t)0) ? "made" : "null",
           errno == ENOENT ? "ENOENT" : "other");
    locale_t dup = duplocale(both);
    freelocale(both);
    printf("dup survives: %s\n", nl_langinfo_l(DAY_2, dup));
    freelocale(dup);
    return 0;
}
