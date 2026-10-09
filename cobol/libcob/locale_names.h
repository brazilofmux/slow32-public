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
static const char *const cob_locale_names[] = {
    "POSIX",
    "az", "be", "bg", "br", "bs", "ca", "cs", "cy", "da", "de", "de_AT", "dsb", "el", "en",
    "eo", "es", "et", "fi", "fo", "fr", "fr_CA", "fy", "ga", "gl", "hr", "hsb", "hu", "is",
    "it", "kl", "lb", "lt", "lv", "mk", "mt", "nb", "nl", "nn", "no", "pl", "pt", "ro", "ru",
    "se", "sk", "sl", "smn", "sq", "sr", "sr_Latn", "sv", "tr", "uk",
};
#define COB_NLOCALES ((int)(sizeof cob_locale_names / sizeof cob_locale_names[0]))
/* the categories of 8.2, in the order SET LOCALE names them */
enum { COB_LC_COLLATE = 0, COB_LC_CTYPE, COB_LC_MESSAGES, COB_LC_MONETARY, COB_LC_NUMERIC, COB_LC_TIME, COB_LC_N };
#endif
