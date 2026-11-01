/* uchar.h -- C11 7.28, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).  char16_t is UTF-16 and
 * char32_t UTF-32 (__STDC_UTF_16__, __STDC_UTF_32__); the conversions are
 * runtime/wchar.c's. */
#ifndef _SLOW32_UCHAR_H_
#define _SLOW32_UCHAR_H_

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _MBSTATE_T_DEFINED
#define _MBSTATE_T_DEFINED
typedef struct { unsigned int __count; unsigned int __value; } mbstate_t;
#endif
#ifndef __cplusplus
typedef unsigned short char16_t;    /* uint_least16_t */
typedef unsigned int char32_t;      /* uint_least32_t */
#endif

size_t mbrtoc16(char16_t *pc16, const char *s, size_t n, mbstate_t *ps);
size_t c16rtomb(char *s, char16_t c16, mbstate_t *ps);
size_t mbrtoc32(char32_t *pc32, const char *s, size_t n, mbstate_t *ps);
size_t c32rtomb(char *s, char32_t c32, mbstate_t *ps);

#ifdef __cplusplus
}
#endif

#endif
