/* locale_impl.h -- what libc's locale.c and the named locales of
 * locales.c share (private to the runtime; not installed).
 *
 * A locale's data is one constant record; a locale object (locale_t)
 * holds one per category.  libc has the records of "C" and "C.UTF-8".
 * The named ones are in locales.s32o, which a program links when it wants
 * them; without that object every other name is refused.
 *
 * Both C libraries build locale.c and locale_state.c.  The clang
 * runtime reaches locales.s32o through weak references, null when it is
 * not linked.  stage08 has no weak symbols, so the self-hosted library
 * has an archive member instead, locale_none.c, that defines the same
 * three functions as finding nothing; a program that links locales.s32o
 * ahead of the library has them already and never pulls it in. */
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
extern const struct __s32_locdata __s32_c_data, __s32_c_utf8_data;
extern struct __s32_locale __s32_c_locale, __s32_global_locale;
extern locale_t __s32_current_locale;

/* C's record, or C.UTF-8's, which differs only in the encoding */
static inline int __s32_is_c_data(const struct __s32_locdata *d) {
    return d == &__s32_c_data || d == &__s32_c_utf8_data;
}

static inline struct __s32_locale *__s32_cur(void) {
    return __s32_current_locale ? __s32_current_locale : &__s32_global_locale;
}
static inline struct __s32_locale *__s32_obj(locale_t l) {
    return l == LC_GLOBAL_LOCALE || !l ? &__s32_global_locale : l;
}

#ifdef __clang__
#define __S32_LOCALES_WEAK __attribute__((weak))
#define __S32_HAVE_LOCALES(f) ((f) != 0)
#else
#define __S32_LOCALES_WEAK
#define __S32_HAVE_LOCALES(f) 1     /* locale_none.c's, if not locales.c's */
#endif

/* locales.c: the record for a locale name, or null when there is none */
const struct __s32_locdata *__s32_locale_find(const char *name);
/* locales.c: strcoll and strxfrm under a named record's collation */
int __s32_locale_strcoll(const struct __s32_locdata *d, const char *a, const char *b);
size_t __s32_locale_strxfrm(const struct __s32_locdata *d, char *dest, const char *src, size_t n);

#endif
