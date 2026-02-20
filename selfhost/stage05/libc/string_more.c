/* string_more.c -- additional string/memory functions for selfhost libc
 * Written for cc-min subset-C compatibility. */

int strlen(char *s);
char *memcpy(char *dst, char *src, int n);
char *malloc(int size);

int memcmp(char *s1, char *s2, int n) {
    int i;
    i = 0;
    while (i < n) {
        if (s1[i] != s2[i]) {
            if ((s1[i] & 255) < (s2[i] & 255)) return -1;
            return 1;
        }
        i = i + 1;
    }
    return 0;
}

char *memchr(char *s, int c, int n) {
    int i;
    i = 0;
    while (i < n) {
        if (s[i] == (char)c) return s + i;
        i = i + 1;
    }
    return (char *)0;
}

char *strdup(char *s) {
    int len;
    char *p;
    len = strlen(s);
    p = malloc(len + 1);
    if (p) memcpy(p, s, len + 1);
    return p;
}

char *strcat(char *dst, char *src) {
    int dlen;
    int i;
    dlen = strlen(dst);
    i = 0;
    while (src[i] != 0) {
        dst[dlen + i] = src[i];
        i = i + 1;
    }
    dst[dlen + i] = 0;
    return dst;
}

char *strncat(char *dst, char *src, int n) {
    int dlen;
    int i;
    dlen = strlen(dst);
    i = 0;
    while (i < n && src[i] != 0) {
        dst[dlen + i] = src[i];
        i = i + 1;
    }
    dst[dlen + i] = 0;
    return dst;
}

char *strstr(char *hay, char *needle) {
    int nlen;
    int i;
    nlen = strlen(needle);
    if (nlen == 0) return hay;
    i = 0;
    while (hay[i] != 0) {
        if (hay[i] == needle[0]) {
            if (memcmp(hay + i, needle, nlen) == 0) return hay + i;
        }
        i = i + 1;
    }
    return (char *)0;
}

char *strrchr(char *s, int c) {
    char *last;
    last = (char *)0;
    while (*s) {
        if (*s == (char)c) last = s;
        s = s + 1;
    }
    if (c == 0) return s;
    return last;
}

char *strcpy(char *dst, char *src) {
    int i;
    i = 0;
    while (src[i] != 0) {
        dst[i] = src[i];
        i = i + 1;
    }
    dst[i] = 0;
    return dst;
}

char *strncpy(char *dst, char *src, int n) {
    int i;
    i = 0;
    while (i < n && src[i] != 0) {
        dst[i] = src[i];
        i = i + 1;
    }
    while (i < n) {
        dst[i] = 0;
        i = i + 1;
    }
    return dst;
}

int atoi(char *s) {
    int neg;
    int val;
    neg = 0;
    while (*s == 32 || *s == 9 || *s == 10) s = s + 1;
    if (*s == 45) { neg = 1; s = s + 1; }
    else if (*s == 43) s = s + 1;
    val = 0;
    while (*s >= 48 && *s <= 57) {
        val = val * 10 + (*s - 48);
        s = s + 1;
    }
    if (neg) return 0 - val;
    return val;
}
