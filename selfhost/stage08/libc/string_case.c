/* Case-insensitive string functions and llabs for the stage08 libc.
 * regal (journals + majesty in C) needed all four; none were here. */
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

int strcasecmp(const char *a, const char *b) {
    unsigned char ca, cb;
    for (;;) {
        ca = (unsigned char)tolower((unsigned char)*a);
        cb = (unsigned char)tolower((unsigned char)*b);
        if (ca != cb) return (int)ca - (int)cb;
        if (ca == 0) return 0;
        a++; b++;
    }
}

int strncasecmp(const char *a, const char *b, size_t n) {
    unsigned char ca, cb;
    while (n > 0) {
        ca = (unsigned char)tolower((unsigned char)*a);
        cb = (unsigned char)tolower((unsigned char)*b);
        if (ca != cb) return (int)ca - (int)cb;
        if (ca == 0) return 0;
        a++; b++; n--;
    }
    return 0;
}

char *strcasestr(const char *hay, const char *needle) {
    size_t nl = strlen(needle);
    if (nl == 0) return (char *)hay;
    for (; *hay; hay++) {
        if (strncasecmp(hay, needle, nl) == 0) return (char *)hay;
    }
    return 0;
}

long long llabs(long long v) {
    return v < 0 ? -v : v;
}
