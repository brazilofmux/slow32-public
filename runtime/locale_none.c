/* The self-hosted library's stand-in for locales.s32o (locale_impl.h):
 * the three functions libc calls into it, finding no named locale.  An
 * archive member of that library only; a program that links locales.s32o
 * defines them first and never pulls this in.  (The clang runtime reaches
 * locales.s32o through weak references and does not build this.)
 *
 * strcoll and strxfrm are called only for a record __s32_locale_find
 * returned, and this one returns none; they are here for the linker. */
#include <string.h>
#include "locale_impl.h"

const struct __s32_locdata *__s32_locale_find(const char *name) {
    (void)name;
    return NULL;
}

int __s32_locale_strcoll(const struct __s32_locdata *d, const char *a, const char *b) {
    (void)d;
    return strcmp(a, b);
}

size_t __s32_locale_strxfrm(const struct __s32_locdata *d, char *dest, const char *src, size_t n) {
    size_t len = strlen(src);
    (void)d;
    if (n > 0) {
        size_t k = len < n - 1 ? len : n - 1;
        memcpy(dest, src, k);
        dest[k] = 0;
    }
    return len;
}
