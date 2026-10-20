#ifndef _STDLIB_H
#define _STDLIB_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

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

int abs(int n);
long labs(long n);
long long llabs(long long n);

int atoi(const char *nptr);
long atol(const char *nptr);
long long atoll(const char *nptr);
long strtol(const char *nptr, char **endptr, int base);
unsigned long strtoul(const char *nptr, char **endptr, int base);
long long strtoll(const char *nptr, char **endptr, int base);
unsigned long long strtoull(const char *nptr, char **endptr, int base);
double strtod(const char *nptr, char **endptr);
long double strtold(const char *nptr, char **endptr);   /* long double is double */
float strtof(const char *nptr, char **endptr);
double atof(const char *nptr);

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *));
void qsort_r(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *, void *),
             void *arg);
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *));

typedef struct { int quot; int rem; } div_t;
typedef struct { long quot; long rem; } ldiv_t;
typedef struct { long long quot; long long rem; } lldiv_t;
div_t div(int numer, int denom);
ldiv_t ldiv(long numer, long denom);
lldiv_t lldiv(long long numer, long long denom);
/* The absolute path, with . and .. and repeated slashes resolved; the
 * path must exist.  Symbolic links are not resolved -- the machine cannot
 * read one (posix_extra.c). */
char *realpath(const char *path, char *resolved);
/* C11: ALIGNMENT a power of two; the block is freed with free() */
void *aligned_alloc(size_t alignment, size_t size);
int posix_memalign(void **memptr, size_t alignment, size_t size);

int rand(void);
void srand(unsigned int seed);
/* rand's values are the top 16 bits of its state: 0..65535.  (This said
 * 0x7FFFFFFF until 2026-10, which rand never came near: a program that
 * scaled by RAND_MAX got numbers in the bottom 1/32768 of its range.) */
#define RAND_MAX 0xFFFF

char *getenv(const char *name);

#ifndef _SLOW32_LOCALE_T
#define _SLOW32_LOCALE_T
typedef struct __s32_locale *locale_t;
#endif

/* The POSIX 2008 forms that take a locale (locale.c) */
double strtod_l(const char *nptr, char **endptr, locale_t l);
float strtof_l(const char *nptr, char **endptr, locale_t l);
long double strtold_l(const char *nptr, char **endptr, locale_t l);
long strtol_l(const char *nptr, char **endptr, int base, locale_t l);
unsigned long strtoul_l(const char *nptr, char **endptr, int base, locale_t l);
long long strtoll_l(const char *nptr, char **endptr, int base, locale_t l);
unsigned long long strtoull_l(const char *nptr, char **endptr, int base, locale_t l);

/* The longest multibyte character: the C locale has single bytes */
#define MB_CUR_MAX ((size_t)1)

#ifdef __cplusplus
}
#endif
#endif