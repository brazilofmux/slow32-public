/* locale_names.h -- the locales this runtime has (docs/plans/locale.md).
 *
 * Index 0 is the POSIX locale, the standard's own: root collation (the
 * DUCET with no tailoring), %m/%d/%y, %H:%M:%S, no currency.  The rest are
 * libutf's 53 CLDR 46 collators, by libutf's names; the collator a record
 * uses is the one utf_collator_find gives for its name.  Shared by
 * libcob/locale.c and the compiler (a locale-name's external name is
 * resolved at compile time; one this table lacks is an error there). */
#ifndef COB_LOCALE_NAMES_H
#define COB_LOCALE_NAMES_H
#include <string.h>
#include <strings.h>
static const char *const cob_locale_names[] = {
    "POSIX",
    "az", "be", "bg", "br", "bs", "ca", "cs", "cy", "da", "de", "de_AT", "dsb", "el", "en",
    "eo", "es", "et", "fi", "fo", "fr", "fr_CA", "fy", "ga", "gl", "hr", "hsb", "hu", "is",
    "it", "kl", "lb", "lt", "lv", "mk", "mt", "nb", "nl", "nn", "no", "pl", "pt", "ro", "ru",
    "se", "sk", "sl", "smn", "sq", "sr", "sr_Latn", "sv", "tr", "uk",
};
#define COB_NLOCALES ((int)(sizeof cob_locale_names / sizeof cob_locale_names[0]))

/* The external name of a locale, resolved to its index, or -1: a POSIX
 * spelling ("sv_SE.UTF-8", "de_AT@euro") or a BCP 47 one ("sv-SE",
 * "sr-Latn"), case-insensitively; a codeset or modifier is dropped; a
 * name the table lacks falls back a subtag at a time ("de_CH" -> "de");
 * "C", "POSIX", "root", "und" and the empty name are the POSIX locale.
 * n is the name's length, or -1 for a C string.  The one rule, in the
 * compiler (the LOCALE clause) and the runtime (the environment). */
static int cob_locale_index(const char *name, int n)
{
    char w[64];
    int k = 0;
    if (n < 0) n = (int)strlen(name);
    for (int i = 0; i < n && k < (int)sizeof w - 1; i++) {
        char c = name[i];
        if (c == '.' || c == '@') break;
        if (c == '-') c = '_';
        if (c == ' ') { if (k == 0) continue; break; }
        w[k++] = c;
    }
    w[k] = 0;
    if (k == 0 || !strcasecmp(w, "C") || !strcasecmp(w, "POSIX") || !strcasecmp(w, "root") || !strcasecmp(w, "und")) return 0;
    for (;;) {
        for (int i = 1; i < COB_NLOCALES; i++) if (!strcasecmp(w, cob_locale_names[i])) return i;
        char *u = strrchr(w, '_');
        if (!u) return -1;
        *u = 0;
    }
}
/* the categories of 8.2, in the order SET LOCALE names them */
enum { COB_LC_COLLATE = 0, COB_LC_CTYPE, COB_LC_MESSAGES, COB_LC_MONETARY, COB_LC_NUMERIC, COB_LC_TIME, COB_LC_N };
#endif
