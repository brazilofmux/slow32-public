/*
 * Locales: setlocale, the POSIX 2008 locale objects (newlocale,
 * duplocale, freelocale), and the *_l forms of the functions a locale
 * changes.  The state they set, and localeconv, uselocale, nl_langinfo,
 * strcoll and strxfrm, which read it, are locale_state.c's.
 *
 * A locale object holds a constant record per category (locale_impl.h).
 * libc has the C locale's, which is also "POSIX" (and "C.UTF-8": the
 * bytes are the same).  The named locales are in locales.s32o, which
 * a program links to have them; without it, another name is refused --
 * setlocale returns NULL, newlocale fails with ENOENT -- rather than
 * answered with "C" as if it had been found.
 *
 * setlocale(..., "") takes the environment's locale the POSIX way:
 * LC_ALL, else the category's own variable, else LANG, else "C"; when one
 * category's name is refused, nothing changes.  A query of LC_ALL whose
 * categories differ answers "LC_COLLATE=...;LC_CTYPE=...;..." in category
 * order, a string setlocale(LC_ALL, ...) takes back.  Each category keeps
 * the name it was set by, as it was written.
 *
 * What a category changes here: LC_COLLATE strcoll and strxfrm (byte
 * order in C); LC_TIME strftime's names and %c %x %X %r %p (time_std.c,
 * built with S32_LOCALE_STRFTIME); LC_NUMERIC the radix of printf
 * (S32_PRINTF_LOCALE) and of strtod (dtoa.c's USE_LOCALE), and with
 * LC_MONETARY what localeconv gives; LC_CTYPE only nl_langinfo(CODESET),
 * since the ctype functions classify bytes and every locale's characters
 * past ASCII are UTF-8 sequences, in no byte class; LC_MESSAGES only
 * YESEXPR and NOEXPR.
 */

#include <ctype.h>
#include <errno.h>
#include <langinfo.h>
#include <limits.h>
#include <locale.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "locale_impl.h"

/* locales.s32o's, when a program links it: null otherwise */
extern const struct __s32_locdata *__s32_locale_find(const char *name) __attribute__((weak));

static const char *const cat_name[N_CAT] = {
    "LC_ALL", "LC_COLLATE", "LC_CTYPE", "LC_MONETARY", "LC_NUMERIC", "LC_TIME", "LC_MESSAGES"
};

static int is_c_name(const char *name) {
    return strcmp(name, "C") == 0 || strcmp(name, "POSIX") == 0 ||
           strcmp(name, "C.UTF-8") == 0 || strcmp(name, "C.utf8") == 0;
}

/* the record for a name, or null with errno set */
static const struct __s32_locdata *lookup(const char *name) {
    const struct __s32_locdata *d = NULL;
    if (strlen(name) >= NAME_MAX_LEN) {
        errno = EINVAL;
        return NULL;
    }
    if (is_c_name(name)) return &__s32_c_data;
    if (__s32_locale_find) d = __s32_locale_find(name);
    if (!d) errno = ENOENT;
    return d;
}

/* the environment's name for a category */
static const char *env_name(int cat) {
    const char *v = getenv("LC_ALL");
    if (v && *v) return v;
    v = getenv(cat_name[cat]);
    if (v && *v) return v;
    v = getenv("LANG");
    if (v && *v) return v;
    return "C";
}

static void assemble_conv(struct __s32_locale *L) {
    const struct lconv *n = &L->data[LC_NUMERIC]->conv;
    L->conv = L->data[LC_MONETARY]->conv;
    L->conv.decimal_point = n->decimal_point;
    L->conv.thousands_sep = n->thousands_sep;
    L->conv.grouping = n->grouping;
}

/* Set the categories in mask of L to a name: "" is the environment's, a
 * composite one names each category.  All or nothing: 0, L untouched and
 * errno set, when a name is refused. */
static int apply(struct __s32_locale *L, int mask, const char *name) {
    const struct __s32_locdata *d[N_CAT] = { 0 };
    const char *nm[N_CAT] = { 0 };
    char part[N_CAT][NAME_MAX_LEN];
    if (strchr(name, '=')) {
        /* "LC_x=name;LC_y=name;...": every category of mask must be there */
        const char *p = name;
        while (*p) {
            const char *eq = strchr(p, '=');
            const char *end = strchr(p, ';');
            if (!end) end = p + strlen(p);
            if (!eq || eq > end) { errno = EINVAL; return 0; }
            int c = 0;
            for (int k = 1; k < N_CAT; k++)
                if (strlen(cat_name[k]) == (size_t)(eq - p) && strncmp(p, cat_name[k], (size_t)(eq - p)) == 0) c = k;
            size_t len = (size_t)(end - eq - 1);
            if (c == 0 || len >= NAME_MAX_LEN) { errno = EINVAL; return 0; }
            memcpy(part[c], eq + 1, len);
            part[c][len] = 0;
            nm[c] = part[c];
            p = *end ? end + 1 : end;
        }
        for (int c = 1; c < N_CAT; c++)
            if ((mask & (1 << c)) && !nm[c]) { errno = EINVAL; return 0; }
    } else {
        for (int c = 1; c < N_CAT; c++)
            if (mask & (1 << c)) nm[c] = name[0] ? name : env_name(c);
    }
    for (int c = 1; c < N_CAT; c++)
        if ((mask & (1 << c)) && !(d[c] = lookup(nm[c]))) return 0;
    for (int c = 1; c < N_CAT; c++) {
        if (!(mask & (1 << c))) continue;
        L->data[c] = d[c];
        strcpy(L->name[c], nm[c]);
    }
    assemble_conv(L);
    return 1;
}

/* ---------------------------------------------------------------- */

char *setlocale(int category, const char *name) {
    static char all[N_CAT * (NAME_MAX_LEN + 16)];
    if (category < LC_ALL || category > LC_MESSAGES) {
        errno = EINVAL;
        return NULL;
    }
    if (name && !apply(&__s32_global_locale, category == LC_ALL ? LC_ALL_MASK : 1 << category, name))
        return NULL;
    if (category != LC_ALL) return __s32_global_locale.name[category];
    int same = 1;
    for (int c = 2; c < N_CAT; c++)
        if (strcmp(__s32_global_locale.name[c], __s32_global_locale.name[1]) != 0) same = 0;
    if (same) return __s32_global_locale.name[1];
    all[0] = 0;
    for (int c = 1; c < N_CAT; c++) {
        if (c > 1) strcat(all, ";");
        strcat(all, cat_name[c]);
        strcat(all, "=");
        strcat(all, __s32_global_locale.name[c]);
    }
    return all;
}

locale_t newlocale(int category_mask, const char *name, locale_t base) {
    if ((category_mask & ~LC_ALL_MASK) != 0 || !name || base == LC_GLOBAL_LOCALE) {
        errno = EINVAL;
        return NULL;
    }
    if (!base && (category_mask == 0 || is_c_name(name))) return &__s32_c_locale;
    struct __s32_locale *L = malloc(sizeof *L);
    if (!L) {
        errno = ENOMEM;
        return NULL;
    }
    *L = base ? *base : __s32_c_locale;
    if (!apply(L, category_mask, name)) {
        free(L);
        return NULL;
    }
    freelocale(base);
    return L;
}

locale_t duplocale(locale_t loc) {
    struct __s32_locale *L = malloc(sizeof *L);
    if (!L) {
        errno = ENOMEM;
        return NULL;
    }
    *L = *__s32_obj(loc);
    return L;
}

void freelocale(locale_t loc) {
    if (loc && loc != &__s32_c_locale && loc != &__s32_global_locale && loc != LC_GLOBAL_LOCALE) free(loc);
}

/* ---------------------------------------------------------------- */

/* ---------------------------------------------------------------- */
/* LC_CTYPE: every locale classifies bytes as the C locale does */

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


/* the rest under the locale they are given: uselocale for the call */
#define UNDER(l, call) do { locale_t old_ = uselocale(l); __typeof__(call) r_ = (call); uselocale(old_); return r_; } while (0)

double strtod_l(const char *s, char **e, locale_t l)       { UNDER(l, strtod(s, e)); }
float strtof_l(const char *s, char **e, locale_t l)        { UNDER(l, strtof(s, e)); }
long double strtold_l(const char *s, char **e, locale_t l) { UNDER(l, strtold(s, e)); }
long strtol_l(const char *s, char **e, int b, locale_t l)  { (void)l; return strtol(s, e, b); }
unsigned long strtoul_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoul(s, e, b); }
long long strtoll_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoll(s, e, b); }
unsigned long long strtoull_l(const char *s, char **e, int b, locale_t l) { (void)l; return strtoull(s, e, b); }

size_t strftime_l(char *s, size_t max, const char *fmt, const struct tm *tm, locale_t l) {
    UNDER(l, strftime(s, max, fmt, tm));
}
