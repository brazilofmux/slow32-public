/*
 * Locales: setlocale and localeconv, the POSIX 2008 locale objects
 * (newlocale, duplocale, freelocale, uselocale), nl_langinfo, and the
 * *_l forms of the functions a locale changes.
 *
 * The "C" locale, also named "POSIX", is the only one, so every locale
 * object is the same static one and the *_l functions are their plain
 * forms.  Another name is refused -- setlocale returns NULL, newlocale
 * fails with ENOENT -- rather than answered with "C" as if it had been
 * found.  setlocale(..., "") takes the environment's locale, which here
 * is "C" whatever LANG says.
 *
 * The locale object keeps a name per category so that named locales
 * can be added without changing its users.
 */

#include <ctype.h>
#include <errno.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

struct __s32_locale {
    const char *name[LC_MESSAGES + 1];    /* [0] unused: LC_ALL is per category */
};

static struct __s32_locale c_locale = {
    { "C", "C", "C", "C", "C", "C", "C" }
};

/* uselocale's per-thread locale (one thread); null means the global one */
static locale_t current;

static int is_c_name(const char *name) {
    return strcmp(name, "C") == 0 || strcmp(name, "POSIX") == 0 || name[0] == '\0';
}

/* ---------------------------------------------------------------- */

char *setlocale(int category, const char *name) {
    if (category < LC_ALL || category > LC_MESSAGES) return NULL;
    if (name && !is_c_name(name)) return NULL;
    return (char *)"C";
}

static struct lconv c_lconv = {
    ".", "", "", "", "", "", "", "", "", "",
    CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX,
    CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX, CHAR_MAX
};

struct lconv *localeconv(void) {
    return &c_lconv;
}

locale_t newlocale(int category_mask, const char *name, locale_t base) {
    (void)base;
    if ((category_mask & ~LC_ALL_MASK) != 0 || !name) {
        errno = EINVAL;
        return NULL;
    }
    if (!is_c_name(name)) {
        errno = ENOENT;
        return NULL;
    }
    return &c_locale;
}

locale_t duplocale(locale_t loc) {
    (void)loc;
    return &c_locale;
}

void freelocale(locale_t loc) {
    (void)loc;                      /* every object is the static C locale */
}

locale_t uselocale(locale_t loc) {
    locale_t old = current ? current : LC_GLOBAL_LOCALE;
    if (loc) current = (loc == LC_GLOBAL_LOCALE) ? NULL : loc;
    return old;
}

/* ---------------------------------------------------------------- */

static const char *const c_langinfo[__S32_NL_ITEMS] = {
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
    [ERA] = "", [ERA_D_FMT] = "", [ALT_DIGITS] = "", [ERA_D_T_FMT] = "", [ERA_T_FMT] = "",
    [RADIXCHAR] = ".", [THOUSEP] = "",
    [YESEXPR] = "^[yY]", [NOEXPR] = "^[nN]", [CRNCYSTR] = "-",
};

char *nl_langinfo_l(nl_item item, locale_t loc) {
    (void)loc;
    if (item < 0 || item >= __S32_NL_ITEMS) return (char *)"";
    return (char *)c_langinfo[item];
}

char *nl_langinfo(nl_item item) {
    return nl_langinfo_l(item, LC_GLOBAL_LOCALE);
}

/* ---------------------------------------------------------------- */
/* The *_l forms: in the C locale, the functions themselves. */

int isalnum_l(int c, locale_t l)  { (void)l; return isalnum(c); }
int isalpha_l(int c, locale_t l)  { (void)l; return isalpha(c); }
int isblank_l(int c, locale_t l)  { (void)l; return isblank(c); }
int iscntrl_l(int c, locale_t l)  { (void)l; return iscntrl(c); }
int isdigit_l(int c, locale_t l)  { (void)l; return isdigit(c); }
int isgraph_l(int c, locale_t l)  { (void)l; return isgraph(c); }
int islower_l(int c, locale_t l)  { (void)l; return islower(c); }
int isprint_l(int c, locale_t l)  { (void)l; return isprint(c); }
int ispunct_l(int c, locale_t l)  { (void)l; return ispunct(c); }
int isspace_l(int c, locale_t l)  { (void)l; return isspace(c); }
int isupper_l(int c, locale_t l)  { (void)l; return isupper(c); }
int isxdigit_l(int c, locale_t l) { (void)l; return isxdigit(c); }
int tolower_l(int c, locale_t l)  { (void)l; return tolower(c); }
int toupper_l(int c, locale_t l)  { (void)l; return toupper(c); }

int strcoll_l(const char *a, const char *b, locale_t l) { (void)l; return strcoll(a, b); }
size_t strxfrm_l(char *d, const char *s, size_t n, locale_t l) { (void)l; return strxfrm(d, s, n); }

double strtod_l(const char *s, char **e, locale_t l)       { (void)l; return strtod(s, e); }
float strtof_l(const char *s, char **e, locale_t l)        { (void)l; return strtof(s, e); }
long double strtold_l(const char *s, char **e, locale_t l) { (void)l; return strtold(s, e); }
long strtol_l(const char *s, char **e, int b, locale_t l)  { (void)l; return strtol(s, e, b); }
unsigned long strtoul_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoul(s, e, b); }
long long strtoll_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoll(s, e, b); }
unsigned long long strtoull_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoull(s, e, b); }

size_t strftime_l(char *s, size_t max, const char *fmt, const struct tm *tm, locale_t l) {
    (void)l;
    return strftime(s, max, fmt, tm);
}
