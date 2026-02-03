#include <stdlib.h>

int abs(int n) {
    return (n < 0) ? -n : n;
}

long labs(long n) {
    return (n < 0) ? -n : n;
}

long long llabs(long long n) {
    return (n < 0) ? -n : n;
}

int atoi(const char *nptr) {
    return (int)strtol(nptr, NULL, 10);
}

long atol(const char *nptr) {
    return strtol(nptr, NULL, 10);
}

// dtoa_strtod is provided by Gay's dtoa.c
extern double dtoa_strtod(const char *s00, char **se);

double strtod(const char *nptr, char **endptr) {
    return dtoa_strtod(nptr, endptr);
}

double atof(const char *nptr) {
    return strtod(nptr, (char **)0);
}

long long atoll(const char *nptr) {
    return (long long)strtol(nptr, (char **)0, 10);
}

static unsigned long int next = 1;

int rand(void) {
    next = next * 1103515245 + 12345;
    return (unsigned int)(next/65536) % 32768;
}

void srand(unsigned int seed) {
    next = seed;
}
