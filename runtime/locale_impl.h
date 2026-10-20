/* locale_impl.h -- what libc's locale.c and the named locales of
 * locales.c share (private to the runtime; not installed).
 *
 * A locale's data is one constant record; a locale object (locale_t)
 * holds one per category.  libc has the C locale's record.  The named
 * ones are in locales.s32o, which a program links when it wants them:
 * libc reaches them only through the weak references below, so without
 * that object every name but "C" and "POSIX" is refused. */
#ifndef S32_LOCALE_IMPL_H
#define S32_LOCALE_IMPL_H
#include <langinfo.h>
#include <locale.h>
#include <stddef.h>

struct __s32_locdata {
    const char *name;                   /* the record's own: "C", "de", "sr_Latn" */
    const char *info[__S32_NL_ITEMS];   /* nl_langinfo; a null item is "" */
    struct lconv conv;                  /* LC_NUMERIC's fields and LC_MONETARY's */
};

#define N_CAT (LC_MESSAGES + 1)     /* [0], LC_ALL, unused: a locale is its categories */
#define NAME_MAX_LEN 64

struct __s32_locale {
    const struct __s32_locdata *data[N_CAT];
    char name[N_CAT][NAME_MAX_LEN];
    struct lconv conv;          /* LC_NUMERIC's fields, then LC_MONETARY's */
};

/* locale_state.c: the C locale's record, newlocale's object for "C", the
 * global locale, and uselocale's (null: the global one) */
extern const struct __s32_locdata __s32_c_data;
extern struct __s32_locale __s32_c_locale, __s32_global_locale;
extern locale_t __s32_current_locale;

static inline struct __s32_locale *__s32_cur(void) {
    return __s32_current_locale ? __s32_current_locale : &__s32_global_locale;
}
static inline struct __s32_locale *__s32_obj(locale_t l) {
    return l == LC_GLOBAL_LOCALE || !l ? &__s32_global_locale : l;
}

/* locales.c: the record for a locale name, or null when there is none */
const struct __s32_locdata *__s32_locale_find(const char *name);
/* locales.c: strcoll and strxfrm under a named record's collation */
int __s32_locale_strcoll(const struct __s32_locdata *d, const char *a, const char *b);
size_t __s32_locale_strxfrm(const struct __s32_locdata *d, char *dest, const char *src, size_t n);

#endif
