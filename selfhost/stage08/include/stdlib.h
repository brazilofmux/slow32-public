/* stdlib.h -- the self-hosted library's (also the cross compilers') */
#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1

void *malloc(size_t size);
void free(void *ptr);
void *calloc(size_t nmemb, size_t size);
void *realloc(void *ptr, size_t size);

void exit(int status);
void _Exit(int status);
void abort(void);
int atexit(void (*function)(void));
int system(const char *command);
char *getenv(const char *name);
int setenv(const char *name, const char *value, int overwrite);
int unsetenv(const char *name);
int putenv(char *string);
int mkstemp(char *tmpl);

int abs(int n);
long labs(long n);
long long llabs(long long n);

typedef struct { int quot; int rem; } div_t;
typedef struct { long quot; long rem; } ldiv_t;
div_t div(int numer, int denom);
ldiv_t ldiv(long numer, long denom);
typedef struct { long long quot; long long rem; } lldiv_t;
lldiv_t lldiv(long long numer, long long denom);
int at_quick_exit(void (*func)(void));
_Noreturn void quick_exit(int status);
void *aligned_alloc(size_t alignment, size_t size);
/* The longest multibyte character in LC_CTYPE's encoding: 1 in "C" and
 * "POSIX", 4 in the UTF-8 locales (runtime/wchar.c) */
size_t __s32_mb_cur_max(void);
#define MB_CUR_MAX (__s32_mb_cur_max())

/* 7.22.7, 7.22.8 */
int mblen(const char *s, size_t n);
int mbtowc(wchar_t *pwc, const char *s, size_t n);
int wctomb(char *s, wchar_t wc);
size_t mbstowcs(wchar_t *pwcs, const char *s, size_t n);
size_t wcstombs(char *s, const wchar_t *pwcs, size_t n);

int atoi(const char *nptr);
long atol(const char *nptr);
long long atoll(const char *nptr);
double atof(const char *s);
long strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
long long strtoll(const char *s, char **end, int base);
unsigned long long strtoull(const char *s, char **end, int base);
double strtod(const char *s, char **end);
float  strtof(const char *s, char **end);
long double strtold(const char *s, char **end);   /* long double is double */

/* rand's values are the top 16 bits of its state, as the clang runtime's are */
#define RAND_MAX 0xFFFF
int rand(void);
void srand(unsigned int seed);

void qsort(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *));
void qsort_r(void *base, size_t nmemb, size_t size, int (*compar)(const void *, const void *, void *), void *arg);
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *));
/* realpath is the cross targets' alone (../../cross-include/libc_linux.c):
 * the emulators they build resolve EXEC paths with it.  SLOW-32's libc
 * has none. */
#ifdef __x86_64__
char *realpath(const char *path, char *resolved);
#endif
#ifdef __aarch64__
char *realpath(const char *path, char *resolved);
#endif

#ifndef _SLOW32_LOCALE_T
#define _SLOW32_LOCALE_T
typedef struct __s32_locale *locale_t;
#endif

/* The POSIX 2008 forms that take a locale (runtime/locale.c) */
double strtod_l(const char *nptr, char **endptr, locale_t l);
float strtof_l(const char *nptr, char **endptr, locale_t l);
long double strtold_l(const char *nptr, char **endptr, locale_t l);
long strtol_l(const char *nptr, char **endptr, int base, locale_t l);
unsigned long strtoul_l(const char *nptr, char **endptr, int base, locale_t l);
long long strtoll_l(const char *nptr, char **endptr, int base, locale_t l);
unsigned long long strtoull_l(const char *nptr, char **endptr, int base, locale_t l);

#endif
