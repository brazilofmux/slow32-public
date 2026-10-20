/*
 * The named locales: POSIX's 53 companions from CLDR 46, the ones libutf
 * has collators for, with libutf's collation built in.
 *
 * A program has them by linking locales.s32o (ahead of libc); without it
 * the C library knows "C" and "POSIX" and refuses every other name.  The
 * object is large -- the collation tables are most of a megabyte of
 * rodata, so a program that links it wants --rodata-size 2M -- which is
 * why it is not in libc: libc's locale.c reaches it only through weak
 * references (locale_impl.h), and what a program does not link it does
 * not carry.
 *
 * A name is resolved as the COBOL runtime resolves one
 * (cobol/libcob/locale_names.h): POSIX spelling or BCP 47, case-
 * insensitively, a modifier dropped, and a name the table lacks falls
 * back a subtag at a time -- "de_CH.UTF-8" is "de", with de's currency.
 * The codeset, when there is one, must be UTF-8: CLDR's strings are, and
 * a program that asked for Latin-1 would get bytes it cannot read.
 *
 * Collation is UCA over the DUCET with the locale's CLDR tailoring, on
 * UTF-8 strings.  strxfrm's result is libutf's sort key with its zero
 * bytes escaped (0 -> 1 1, 1 -> 1 2), which keeps the order strcmp sees
 * and leaves no NUL inside it.
 */
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "locale_impl.h"
#include "utf/collate.h"

#include "locales_data.h"

/* libutf, in this translation unit so that the object carries all of it;
 * collate.c and nfc.c each have a static UTF-8 decoder of the same name */
#include "utf/src/collate.c"
#define utf8_decode_raw nfc_utf8_decode_raw
#define utf8_is_valid_scalar nfc_utf8_is_valid_scalar
#include "utf/src/nfc.c"
#undef utf8_decode_raw
#undef utf8_is_valid_scalar
#include "utf/tables/ducet_cetable.c"
#include "utf/tables/ducet_dfa_tables.c"
#include "utf/tables/nfc_tables.c"
#include "utf/tables/unicode_tables.c"

static int codeset_ok(const char *cs, size_t n) {
    char w[16];
    size_t k = 0;
    for (size_t i = 0; i < n && k < sizeof w - 1; i++)
        if (cs[i] != '-') w[k++] = cs[i];
    w[k] = 0;
    return k < sizeof w - 1 && strcasecmp(w, "UTF8") == 0;
}

const struct __s32_locdata *__s32_locale_find(const char *name) {
    char w[64];
    size_t k = 0;
    const char *p = name;
    for (; *p && *p != '.' && *p != '@'; p++) {
        if (k == sizeof w - 1) return NULL;
        w[k++] = *p == '-' ? '_' : *p;
    }
    w[k] = 0;
    if (*p == '.') {
        const char *cs = ++p;
        while (*p && *p != '@') p++;
        if (!codeset_ok(cs, (size_t)(p - cs))) return NULL;
    }
    if (k == 0) return NULL;
    for (;;) {
        for (int i = 0; i < N_NAMED_LOCALES; i++)
            if (strcasecmp(w, named_locales[i].name) == 0) return &named_locales[i];
        char *u = strrchr(w, '_');
        if (!u) return NULL;
        *u = 0;
    }
}

static const utf_collator *collator_of(const struct __s32_locdata *d) {
    static const utf_collator *found[N_NAMED_LOCALES];
    int i = (int)(d - named_locales);
    if (i < 0 || i >= N_NAMED_LOCALES) return utf_collator_root();
    if (!found[i]) {
        found[i] = utf_collator_find(d->name);
        if (!found[i]) found[i] = utf_collator_root();  /* cannot happen: the names are libutf's */
    }
    return found[i];
}

int __s32_locale_strcoll(const struct __s32_locdata *d, const char *a, const char *b) {
    return utf_collate_cmp_l((const unsigned char *)a, strlen(a),
                             (const unsigned char *)b, strlen(b), collator_of(d));
}

size_t __s32_locale_strxfrm(const struct __s32_locdata *d, char *dest, const char *src, size_t n) {
    unsigned char small[256];
    unsigned char *key = small;
    size_t cap = sizeof small, len, size = strlen(src);
    /* the key is cut at the buffer's size: grow it until the key fits */
    for (;;) {
        len = utf_collate_sortkey_l((const unsigned char *)src, size, key, cap, collator_of(d));
        if (len < cap) break;
        unsigned char *bigger = malloc(cap * 2);
        if (!bigger) {
            len = 0;                    /* no memory: an empty key, the order lost */
            break;
        }
        if (key != small) free(key);
        key = bigger;
        cap *= 2;
    }
    size_t out = 0;
    for (size_t i = 0; i < len; i++) {
        if (key[i] <= 1) {
            if (out + 1 < n) dest[out] = 1;
            out++;
            if (out + 1 < n) dest[out] = (char)(key[i] + 1);
            out++;
        } else {
            if (out + 1 < n) dest[out] = (char)key[i];
            out++;
        }
    }
    if (n > 0) dest[out < n ? out : n - 1] = 0;
    if (key != small) free(key);
    return out;
}
