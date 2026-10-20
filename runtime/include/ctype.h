#ifndef _CTYPE_H
#define _CTYPE_H

#ifdef __cplusplus
extern "C" {
#endif

// Character classification functions: the C locale (ASCII; bytes 0x80-0xFF are in no class)
int isalnum(int c);   // alphanumeric character
int isalpha(int c);   // alphabetic character
int isblank(int c);   // blank character (space, tab)
int iscntrl(int c);   // control character
int isdigit(int c);   // decimal digit
int isgraph(int c);   // printing character except space
int islower(int c);   // lowercase letter
int isprint(int c);   // printing character including space
int ispunct(int c);   // punctuation character
int isspace(int c);   // white-space character
int isupper(int c);   // uppercase letter
int isxdigit(int c);  // hexadecimal digit

// Character conversion functions (ASCII letters only)
int tolower(int c);   // convert to lowercase
int toupper(int c);   // convert to uppercase

// Non-standard but common
int isascii(int c);   // ASCII character (0-127)
int toascii(int c);   // mask to 7 bits

// Classification table (for direct access)
extern const unsigned char __ctype_table[256];
extern const unsigned char __toupper_table[256];
extern const unsigned char __tolower_table[256];

#ifndef _SLOW32_LOCALE_T
#define _SLOW32_LOCALE_T
typedef struct __s32_locale *locale_t;
#endif

/* The POSIX 2008 forms that take a locale (locale.c) */
int isalnum_l(int c, locale_t l);
int isalpha_l(int c, locale_t l);
int isblank_l(int c, locale_t l);
int iscntrl_l(int c, locale_t l);
int isdigit_l(int c, locale_t l);
int isgraph_l(int c, locale_t l);
int islower_l(int c, locale_t l);
int isprint_l(int c, locale_t l);
int ispunct_l(int c, locale_t l);
int isspace_l(int c, locale_t l);
int isupper_l(int c, locale_t l);
int isxdigit_l(int c, locale_t l);
int tolower_l(int c, locale_t l);
int toupper_l(int c, locale_t l);

#ifdef __cplusplus
}
#endif
#endif
