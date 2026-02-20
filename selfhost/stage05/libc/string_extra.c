/* Selfhost bootstrap libc: extra string functions
 *
 * Provides strcmp, strncmp, strchr for the selfhost assembler.
 * These are not in mmio_minimal.s which only has strlen, memset,
 * memcpy, memmove.
 *
 * Written for cc-min subset-C compatibility.
 */

int strcmp(const char *s1, const char *s2) {
    while (*s1 && *s1 == *s2) {
        s1 = s1 + 1;
        s2 = s2 + 1;
    }
    return (int)(*(unsigned char *)s1) - (int)(*(unsigned char *)s2);
}

int strncmp(const char *s1, const char *s2, unsigned int n) {
    if (n == 0) return 0;
    while (n > 1 && *s1 && *s1 == *s2) {
        s1 = s1 + 1;
        s2 = s2 + 1;
        n = n - 1;
    }
    return (int)(*(unsigned char *)s1) - (int)(*(unsigned char *)s2);
}

char *strchr(const char *s, int c) {
    while (*s) {
        if (*s == (char)c) return (char *)s;
        s = s + 1;
    }
    if (c == 0) return (char *)s;
    return (char *)0;
}
