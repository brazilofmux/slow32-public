/* locale_test.c -- libcob/locale.c on the host (and the same binary's
 * output on SLOW-32, by the harness): names, the current locale from the
 * environment, saving and restoring, and locale-based comparison with
 * ICU's answers as the witness (libutf's collate_locales table, ICU 72 /
 * CLDR 42; docs/plans/locale.md).  The last line is the count. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "locale_names.h"

int cob_loc_find(const char *name, int n);
const char *cob_loc_name(int loc);
const char *cob_loc_collator_name(int loc);
void cob_loc_init_env(void);
int cob_loc_current(int cat);
int cob_loc_user_default(int cat);
int cob_loc_env_missing(void);
void cob_loc_set(int cat, int loc);
void cob_loc_set_user_default(int loc);
void *cob_loc_save(int user_default);
int cob_loc_restore(int cat, const void *p);
int cob_loc_compare(const unsigned char *a, int na, int nat_a, const unsigned char *b, int nb, int nat_b, int loc, int level);

static int checks, fails;
static void ok(int cond, const char *what)
{
    checks++;
    if (!cond) { fails++; printf("FAIL: %s\n", what); }
}
static int cmp(const char *loc, const char *a, const char *b, int level)
{
    int l = cob_loc_find(loc, -1);
    return cob_loc_compare((const unsigned char *)a, (int)strlen(a), 0, (const unsigned char *)b, (int)strlen(b), 0, l, level);
}
/* a UTF-8 string as UTF-16BE */
static int to_nat(const char *s, unsigned char *out)
{
    int k = 0;
    for (const unsigned char *p = (const unsigned char *)s; *p; ) {
        unsigned cp, n;
        if (*p < 0x80) { cp = *p; n = 1; }
        else if (*p < 0xE0) { cp = ((p[0] & 0x1F) << 6) | (p[1] & 0x3F); n = 2; }
        else if (*p < 0xF0) { cp = ((p[0] & 0x0F) << 12) | ((p[1] & 0x3F) << 6) | (p[2] & 0x3F); n = 3; }
        else { cp = ((p[0] & 7) << 18) | ((p[1] & 0x3F) << 12) | ((p[2] & 0x3F) << 6) | (p[3] & 0x3F); n = 4; }
        p += n;
        if (cp >= 0x10000) { cp -= 0x10000; unsigned hi = 0xD800 + (cp >> 10), lo = 0xDC00 + (cp & 0x3FF); out[k++] = hi >> 8; out[k++] = hi; out[k++] = lo >> 8; out[k++] = lo; }
        else { out[k++] = cp >> 8; out[k++] = cp; }
    }
    return k;
}

int main(void)
{
    /* names */
    int sv = cob_loc_find("sv", -1);
    ok(sv > 0, "sv is a locale");
    ok(cob_loc_find("sv_SE", -1) == sv, "sv_SE falls back to sv");
    ok(cob_loc_find("sv-SE", -1) == sv, "sv-SE (BCP 47) is sv");
    ok(cob_loc_find("sv_SE.UTF-8", -1) == sv, "the codeset is dropped");
    ok(cob_loc_find("SV_se", -1) == sv, "case does not matter");
    ok(cob_loc_find("sv_SE.UTF-8  ", 13) == sv, "trailing spaces (a COBOL literal padded)");
    ok(cob_loc_find("de_AT@euro", -1) == cob_loc_find("de_AT", -1) && cob_loc_find("de_AT", -1) != cob_loc_find("de", -1), "de_AT is its own; the modifier is dropped");
    ok(cob_loc_find("de_CH", -1) == cob_loc_find("de", -1), "de_CH falls back to de");
    ok(cob_loc_find("sr-Latn-RS", -1) == cob_loc_find("sr_Latn", -1), "sr-Latn-RS is sr_Latn");
    ok(cob_loc_find("C", -1) == 0 && cob_loc_find("POSIX", -1) == 0 && cob_loc_find("", 0) == 0 && cob_loc_find("root", -1) == 0 && cob_loc_find("und", -1) == 0, "C, POSIX, root, und, empty are POSIX");
    ok(cob_loc_find("xx", -1) == -1 && cob_loc_find("tlh_KL", -1) == -1 && cob_loc_find("en_US_POSIX_x", -1) == cob_loc_find("en", -1), "unknown is -1; a known language with unknown subtags is the language");
    for (int i = 0; i < COB_NLOCALES; i++) {
        const char *cn = cob_loc_collator_name(i);
        char what[80]; snprintf(what, sizeof what, "locale %s has collator %s", cob_locale_names[i], cn ? cn : "(none)");
        ok(cn && !strcmp(cn, i == 0 ? "root" : cob_locale_names[i]), what);
    }
    ok(!strcmp(cob_loc_name(0), "POSIX") && !strcmp(cob_loc_name(sv), "sv"), "names read back");

    /* the current locale from the environment (the guest libc has no setenv;
     * there the COBOL tests' .env files cover this) */
#ifndef __slow32__
    unsetenv("LC_ALL"); unsetenv("LANG"); for (int c = 0; c < COB_LC_N; c++) unsetenv((const char *[]){"LC_COLLATE","LC_CTYPE","LC_MESSAGES","LC_MONETARY","LC_NUMERIC","LC_TIME"}[c]);
    cob_loc_init_env();
    ok(cob_loc_current(COB_LC_COLLATE) == 0 && cob_loc_current(COB_LC_TIME) == 0, "no environment: POSIX");
    setenv("LANG", "sv_SE.UTF-8", 1); cob_loc_init_env(); cob_loc_set(-1, -2);
    ok(cob_loc_current(COB_LC_COLLATE) == sv && cob_loc_current(COB_LC_TIME) == sv, "LANG sets every category");
    setenv("LC_COLLATE", "de_DE", 1); cob_loc_init_env(); cob_loc_set(-1, -2);
    ok(cob_loc_current(COB_LC_COLLATE) == cob_loc_find("de", -1) && cob_loc_current(COB_LC_TIME) == sv, "LC_COLLATE over LANG, for its category");
    setenv("LC_ALL", "cs_CZ", 1); cob_loc_init_env(); cob_loc_set(-1, -2);
    ok(cob_loc_current(COB_LC_COLLATE) == cob_loc_find("cs", -1) && cob_loc_current(COB_LC_TIME) == cob_loc_find("cs", -1), "LC_ALL over everything");
    ok(!cob_loc_env_missing(), "nothing missing so far");
    setenv("LC_ALL", "tlh_KL.UTF-8", 1); cob_loc_init_env(); cob_loc_set(-1, -2);
    ok(cob_loc_current(COB_LC_COLLATE) == 0 && cob_loc_env_missing() && !cob_loc_env_missing(), "an unknown environment locale: POSIX, and noted once");
    unsetenv("LC_ALL"); unsetenv("LC_COLLATE"); unsetenv("LANG"); cob_loc_init_env(); cob_loc_set(-1, -2);
#endif

    /* SET LOCALE: switch, save, restore -- from POSIX, whatever the
     * environment said (the emulator hands the guest its host's LANG) */
    cob_loc_set_user_default(-3); cob_loc_set(-1, -3);
    cob_loc_set(COB_LC_COLLATE, sv);
    ok(cob_loc_current(COB_LC_COLLATE) == sv && cob_loc_current(COB_LC_TIME) == 0, "one category switched");
    void *saved = cob_loc_save(0);
    cob_loc_set(-1, cob_loc_find("tr", -1));
    ok(cob_loc_current(COB_LC_TIME) == cob_loc_find("tr", -1), "LC_ALL switched");
    ok(cob_loc_restore(-1, saved) == 0 && cob_loc_current(COB_LC_COLLATE) == sv && cob_loc_current(COB_LC_TIME) == 0, "restored from the saved locale");
    ok(cob_loc_restore(-1, "not a saved locale") == 1 && cob_loc_restore(-1, NULL) == 1, "a pointer that is not a saved locale");
    cob_loc_set(-1, -3);
    ok(cob_loc_current(COB_LC_COLLATE) == 0, "SYSTEM-DEFAULT is POSIX");
    cob_loc_set_user_default(sv); cob_loc_set(COB_LC_TIME, -2);
    ok(cob_loc_user_default(COB_LC_TIME) == sv && cob_loc_current(COB_LC_TIME) == sv && cob_loc_current(COB_LC_COLLATE) == 0, "the user default set and taken");
    void *saved_ud = cob_loc_save(1);
    ok(cob_loc_restore(COB_LC_COLLATE, saved_ud) == 0 && cob_loc_current(COB_LC_COLLATE) == sv, "a saved user default restored into one category");
    free(saved); free(saved_ud);
    cob_loc_set_user_default(-3); cob_loc_set(-1, -3);

    /* the trimming rule (8.8.4.2.11) */
    ok(cmp("root", "ab   ", "ab", 0) == 0, "trailing spaces trimmed");
    ok(cmp("root", "     ", " ", 0) == 0, "all spaces is one space");
    ok(cmp("root", "", "", 0) == 0, "two empty operands are equal");
    ok(cmp("root", " ", "", 0) > 0, "one space after the empty operand");
    ok(cmp("root", "a b", "a  b", 0) > 0 && cmp("root", "a-b", "ab", 0) < 0, "inner spaces count: a space sorts before a letter (non-ignorable), so the extra space makes the lesser operand");

    /* orders: ICU's (libutf's collate_locales table) */
    ok(cmp("sv", "z", "\xC3\xA5", 0) < 0, "sv z < å");
    ok(cmp("sv", "\xC3\xA5", "\xC3\xA4", 0) < 0, "sv å < ä");
    ok(cmp("sv", "\xC3\xA4", "\xC3\xB6", 0) < 0, "sv ä < ö");
    ok(cmp("sv", "\xC3\xA4" "b", "ab", 0) > 0, "sv äb > ab");
    ok(cmp("root", "\xC3\xA4", "b", 0) < 0 && cmp("de", "\xC3\xA4", "b", 0) < 0, "root, de: ä < b");
    ok(cmp("cs", "ch", "h", 0) > 0 && cmp("cs", "ch", "i", 0) < 0, "cs: h < ch < i");
    ok(cmp("root", "ch", "h", 0) < 0, "root: ch < h");
    ok(cmp("es", "\xC3\xB1", "o", 0) < 0 && cmp("es", "\xC3\xB1", "nz", 0) > 0, "es: nz < ñ < o");
    ok(cmp("da", "A", "a", 0) < 0 && cmp("root", "A", "a", 0) > 0, "da upper first; root lower first");
    ok(cmp("da", "aa", "z", 0) > 0, "da: aa > z");
    ok(cmp("fr_CA", "c\xC3\xB4te", "cot\xC3\xA9", 0) < 0 && cmp("root", "c\xC3\xB4te", "cot\xC3\xA9", 0) > 0, "fr_CA backwards secondaries");
    ok(cmp("ru", "\xD1\x8F", "a", 0) < 0 && cmp("root", "\xD1\x8F", "a", 0) > 0, "ru: Cyrillic first");
    ok(cmp("tr", "\xC4\xB1", "i", 0) < 0 && cmp("tr", "\xC4\xB1", "h", 0) > 0, "tr: h < ı < i");
    ok(cmp("pl", "\xC5\x82", "m", 0) < 0 && cmp("pl", "\xC5\x82", "lz", 0) > 0, "pl: lz < ł < m");
    ok(cmp("hu", "cs", "cz", 0) > 0, "hu: cs > cz");
    ok(cmp("root", "a", "b", 0) < 0 && cmp("root", "b", "a", 0) > 0 && cmp("root", "abc", "abc", 0) == 0, "signs");

    /* levels (15.85 argument-4) */
    ok(cmp("root", "a", "A", 1) == 0 && cmp("root", "a", "A", 2) == 0 && cmp("root", "a", "A", 3) < 0 && cmp("root", "a", "A", 4) < 0, "a vs A: equal through level 2, a first at 3");
    ok(cmp("root", "a", "\xC3\xA1", 1) == 0 && cmp("root", "a", "\xC3\xA1", 2) < 0, "a vs á: equal at level 1, differs at 2");
    ok(cmp("root", "resume", "r\xC3\xA9sum\xC3\xA9", 1) == 0 && cmp("root", "resume", "r\xC3\xA9sum\xC3\xA9", 0) < 0, "resume vs résumé");
    ok(cmp("root", "ab", "abc", 1) < 0 && cmp("root", "abc", "ab", 1) > 0, "a prefix is less at level 1");
    ok(cmp("root", "\xC3\x85", "A\xCC\x8A", 4) == 0 && cmp("root", "\xC3\x85", "A\xCC\x8A", 0) == 0, "precomposed and decomposed Å are equal at every level");
    ok(cmp("root", "Hello World", "hello world", 1) == 0 && cmp("root", "Hello World", "hello world", 3) > 0, "case only at level 3");

    /* national operands */
    unsigned char na[64], nb[64];
    int la = to_nat("z", na), lb = to_nat("\xC3\xA5", nb);
    ok(cob_loc_compare(na, la, 1, nb, lb, 1, sv, 0) < 0, "national z < å in sv");
    ok(cob_loc_compare(na, la, 1, (const unsigned char *)"\xC3\xA5", 2, 0, sv, 0) < 0, "national z vs alphanumeric å");
    la = to_nat("ab   ", na);
    ok(cob_loc_compare(na, la, 1, (const unsigned char *)"ab", 2, 0, 0, 0) == 0, "national trailing spaces trimmed");
    la = to_nat("   ", na);
    ok(cob_loc_compare(na, la, 1, (const unsigned char *)" ", 1, 0, 0, 0) == 0, "national all spaces is one space");
    la = to_nat("\xF0\x9F\x98\x80", na);
    ok(cob_loc_compare(na, la, 1, (const unsigned char *)"\xF0\x9F\x98\x80", 4, 0, 0, 0) == 0, "a surrogate pair decodes to its code point");
    ok(cob_loc_compare((const unsigned char *)"a", 1, 0, (const unsigned char *)"b", 1, 0, -1, 0) < 0, "the current locale (-1)");

    /* a long operand: past the buffers, the same answer */
    static char big1[9000], big2[9000];
    memset(big1, 'x', 8999); memset(big2, 'x', 8999); big2[8998] = 'y';
    ok(cob_loc_compare((const unsigned char *)big1, 8999, 0, (const unsigned char *)big2, 8999, 0, 0, 0) < 0, "long operands");
    ok(cob_loc_compare((const unsigned char *)big1, 8999, 0, (const unsigned char *)big2, 8999, 0, 0, 1) < 0, "long operands at level 1 (the fallback)");

    printf("locale_test: %d checks, %d failed\n", checks, fails);
    return fails != 0;
}
