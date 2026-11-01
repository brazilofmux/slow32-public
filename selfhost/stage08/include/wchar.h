/* wchar.h -- C11 7.29, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).
 *
 * A wchar_t is a code point: the "C" locale's characters are ASCII, the
 * UTF-8 locales' (C.UTF-8 and the named ones) are Unicode's.  The multibyte
 * functions are runtime/wchar.c, the wide strings runtime/wcs.c, wide
 * input and output runtime/wstdio.c; the scanf and printf families
 * (sscanf.c, printf_enhanced.c) do the wide forms' formatting too.
 * C++'s <cwchar> includes this header even with wide characters turned
 * off in libc++. */
#ifndef _SLOW32_WCHAR_H_
#define _SLOW32_WCHAR_H_

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WINT_T_DEFINED
#define _WINT_T_DEFINED
typedef unsigned int wint_t;   /* clang's __WINT_TYPE__; WINT_MIN/MAX in <stdint.h> */
#endif
#ifndef _MBSTATE_T_DEFINED
#define _MBSTATE_T_DEFINED
/* a conversion's state: zero is the initial one (runtime/wchar.c) */
typedef struct { unsigned int __count; unsigned int __value; } mbstate_t;
#endif

struct tm;

#define WEOF ((wint_t)-1)
#ifndef WCHAR_MIN
#if __SIZEOF_POINTER__ == 8
#define WCHAR_MIN (-2147483647 - 1)
#define WCHAR_MAX 2147483647
#else
#define WCHAR_MIN 0U
#define WCHAR_MAX 4294967295U
#endif
#endif

/* 7.29.2 formatted wide input and output */
int fwprintf(FILE *stream, const wchar_t *format, ...);
int fwscanf(FILE *stream, const wchar_t *format, ...);
int swprintf(wchar_t *s, size_t n, const wchar_t *format, ...);
int swscanf(const wchar_t *s, const wchar_t *format, ...);
int vfwprintf(FILE *stream, const wchar_t *format, va_list arg);
int vfwscanf(FILE *stream, const wchar_t *format, va_list arg);
int vswprintf(wchar_t *s, size_t n, const wchar_t *format, va_list arg);
int vswscanf(const wchar_t *s, const wchar_t *format, va_list arg);
int vwprintf(const wchar_t *format, va_list arg);
int vwscanf(const wchar_t *format, va_list arg);
int wprintf(const wchar_t *format, ...);
int wscanf(const wchar_t *format, ...);

/* 7.29.3 wide character input and output */
wint_t fgetwc(FILE *stream);
wchar_t *fgetws(wchar_t *s, int n, FILE *stream);
wint_t fputwc(wchar_t c, FILE *stream);
int fputws(const wchar_t *s, FILE *stream);
int fwide(FILE *stream, int mode);
wint_t getwc(FILE *stream);
wint_t getwchar(void);
wint_t putwc(wchar_t c, FILE *stream);
wint_t putwchar(wchar_t c);
wint_t ungetwc(wint_t c, FILE *stream);

/* 7.29.4 general wide string utilities */
double wcstod(const wchar_t *nptr, wchar_t **endptr);
float wcstof(const wchar_t *nptr, wchar_t **endptr);
long double wcstold(const wchar_t *nptr, wchar_t **endptr);
long wcstol(const wchar_t *nptr, wchar_t **endptr, int base);
long long wcstoll(const wchar_t *nptr, wchar_t **endptr, int base);
unsigned long wcstoul(const wchar_t *nptr, wchar_t **endptr, int base);
unsigned long long wcstoull(const wchar_t *nptr, wchar_t **endptr, int base);
wchar_t *wcscpy(wchar_t *s1, const wchar_t *s2);
wchar_t *wcsncpy(wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t *wmemcpy(wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t *wmemmove(wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t *wcscat(wchar_t *s1, const wchar_t *s2);
wchar_t *wcsncat(wchar_t *s1, const wchar_t *s2, size_t n);
int wcscmp(const wchar_t *s1, const wchar_t *s2);
int wcscoll(const wchar_t *s1, const wchar_t *s2);
int wcsncmp(const wchar_t *s1, const wchar_t *s2, size_t n);
size_t wcsxfrm(wchar_t *s1, const wchar_t *s2, size_t n);
int wmemcmp(const wchar_t *s1, const wchar_t *s2, size_t n);
wchar_t *wcschr(const wchar_t *s, wchar_t c);
size_t wcscspn(const wchar_t *s1, const wchar_t *s2);
wchar_t *wcspbrk(const wchar_t *s1, const wchar_t *s2);
wchar_t *wcsrchr(const wchar_t *s, wchar_t c);
size_t wcsspn(const wchar_t *s1, const wchar_t *s2);
wchar_t *wcsstr(const wchar_t *s1, const wchar_t *s2);
wchar_t *wcstok(wchar_t *s1, const wchar_t *s2, wchar_t **ptr);
wchar_t *wmemchr(const wchar_t *s, wchar_t c, size_t n);
size_t wcslen(const wchar_t *s);
wchar_t *wmemset(wchar_t *s, wchar_t c, size_t n);
size_t wcsftime(wchar_t *s, size_t maxsize, const wchar_t *format, const struct tm *timeptr);

/* POSIX */
size_t wcsnlen(const wchar_t *s, size_t maxlen);
wchar_t *wcsdup(const wchar_t *s);

/* 7.29.6 extended multibyte and wide character conversion */
wint_t btowc(int c);
int wctob(wint_t c);
int mbsinit(const mbstate_t *ps);
size_t mbrlen(const char *s, size_t n, mbstate_t *ps);
size_t mbrtowc(wchar_t *pwc, const char *s, size_t n, mbstate_t *ps);
size_t wcrtomb(char *s, wchar_t wc, mbstate_t *ps);
size_t mbsrtowcs(wchar_t *dst, const char **src, size_t len, mbstate_t *ps);
size_t wcsrtombs(char *dst, const wchar_t **src, size_t len, mbstate_t *ps);
size_t mbsnrtowcs(wchar_t *dst, const char **src, size_t nms, size_t len, mbstate_t *ps);
size_t wcsnrtombs(char *dst, const wchar_t **src, size_t nwc, size_t len, mbstate_t *ps);

#ifdef __cplusplus
}
#endif

#endif
