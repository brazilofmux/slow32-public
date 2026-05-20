/* stdlib.h -- s12cc-compatible stub */
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
void abort(void);

int abs(int n);
int atoi(char *nptr);
long strtol(char *nptr, char **endptr, int base);

int rand(void);
void srand(unsigned int seed);

#endif
