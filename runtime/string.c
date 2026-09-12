#include <string.h>
#include <stddef.h>

size_t strlen(const char *s) {
    size_t len = 0;
    while (*s++) len++;
    return len;
}

char *strcpy(char *dest, const char *src) {
    char *ret = dest;
    while ((*dest++ = *src++));
    return ret;
}

char *strncpy(char *dest, const char *src, size_t n) {
    /* Exactly n bytes, never n+1: the old form skipped the n-- on the
     * iteration that copied the NUL, so every padded copy overwrote
     * one byte past the field. Doom's lumpinfo name[8] found it. */
    size_t i = 0;
    for (; i < n && src[i]; i++) {
        dest[i] = src[i];
    }
    for (; i < n; i++) {
        dest[i] = '\0';
    }
    return dest;
}

int strcmp(const char *s1, const char *s2) {
    while (*s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    return *(unsigned char *)s1 - *(unsigned char *)s2;
}

int strncmp(const char *s1, const char *s2, size_t n) {
    if (n == 0) return 0;
    while (n-- && *s1 && (*s1 == *s2)) {
        s1++;
        s2++;
    }
    if (n == (size_t)-1) return 0;
    return *(unsigned char *)s1 - *(unsigned char *)s2;
}

char *strcat(char *dest, const char *src) {
    char *ret = dest;
    while (*dest) dest++;
    while ((*dest++ = *src++));
    return ret;
}

char *strncat(char *dest, const char *src, size_t n) {
    char *ret = dest;
    while (*dest) dest++;
    while (n-- && (*dest++ = *src++));
    if (n == (size_t)-1) *dest = '\0';
    return ret;
}

char *strchr(const char *s, int c) {
    while (*s) {
        if (*s == (char)c) return (char *)s;
        s++;
    }
    return (c == 0) ? (char *)s : NULL;
}

char *strrchr(const char *s, int c) {
    const char *last = NULL;
    while (*s) {
        if (*s == (char)c) last = s;
        s++;
    }
    if (c == 0) return (char *)s;
    return (char *)last;
}

char *strstr(const char *haystack, const char *needle) {
    if (!*needle) return (char *)haystack;
    
    while (*haystack) {
        const char *h = haystack;
        const char *n = needle;
        while (*h && *n && (*h == *n)) {
            h++;
            n++;
        }
        if (!*n) return (char *)haystack;
        haystack++;
    }
    return NULL;
}

/* Word-wise: slow32-dbt and qemu replace this function with a native
 * stub by name, so only the interpreters (slow32, slow32-fast) ever run
 * it -- and there the byte loop cost regal 4.4M instructions per start
 * just to zero 1.18MB of .bss from crt0.  Bytes to a word boundary,
 * four words at a time, then the tail.  Compiled with -fno-builtin so
 * clang does not turn the loop back into a call to memset. */
void *memset(void *s, int c, size_t n) {
    unsigned char *p = s;
    unsigned char b = (unsigned char)c;
    unsigned int w;
    unsigned int *wp;
    while (n != 0 && (((unsigned int)(size_t)p) & 3) != 0) { *p++ = b; n--; }
    if (n >= 4) {
        w = (unsigned int)b * 0x01010101u;
        wp = (unsigned int *)p;
        while (n >= 16) { wp[0] = w; wp[1] = w; wp[2] = w; wp[3] = w; wp += 4; n -= 16; }
        while (n >= 4) { *wp++ = w; n -= 4; }
        p = (unsigned char *)wp;
    }
    while (n != 0) { *p++ = b; n--; }
    return s;
}

int memcmp(const void *s1, const void *s2, size_t n) {
    const unsigned char *p1 = s1;
    const unsigned char *p2 = s2;
    while (n--) {
        if (*p1 != *p2) return *p1 - *p2;
        p1++;
        p2++;
    }
    return 0;
}

void *memchr(const void *s, int c, size_t n) {
    const unsigned char *p = s;
    while (n--) {
        if (*p == (unsigned char)c) return (void *)p;
        p++;
    }
    return NULL;
}

void *memmove(void *dest, const void *src, size_t n) {
    unsigned char *d = dest;
    const unsigned char *s = src;
    
    if (d < s) {
        while (n--) *d++ = *s++;
    } else if (d > s) {
        d += n;
        s += n;
        while (n--) *--d = *--s;
    }
    return dest;
}