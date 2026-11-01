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
#define MB_CUR_MAX 1    /* bytes in a character: the "C" locale is the only one */

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

#endif
