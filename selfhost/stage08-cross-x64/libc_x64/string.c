/* string.c — String and memory functions */

char *malloc(int size);

char *memcpy(char *dst, char *src, int n) {
    int i;
    i = 0;
    while (i < n) {
        dst[i] = src[i];
        i = i + 1;
    }
    return dst;
}

char *memset(char *dst, int c, int n) {
    int i;
    i = 0;
    while (i < n) {
        dst[i] = c;
        i = i + 1;
    }
    return dst;
}

int memcmp(char *a, char *b, int n) {
    int i;
    i = 0;
    while (i < n) {
        if (a[i] != b[i])
            return (a[i] & 255) - (b[i] & 255);
        i = i + 1;
    }
    return 0;
}

int strlen(char *s) {
    int n;
    n = 0;
    while (s[n]) n = n + 1;
    return n;
}

int strcmp(char *a, char *b) {
    while (*a && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return (*a & 255) - (*b & 255);
}

int strncmp(char *a, char *b, int n) {
    int i;
    i = 0;
    while (i < n && a[i] && a[i] == b[i]) i = i + 1;
    if (i == n) return 0;
    return (a[i] & 255) - (b[i] & 255);
}

char *strcpy(char *dst, char *src) {
    int i;
    i = 0;
    while (src[i]) {
        dst[i] = src[i];
        i = i + 1;
    }
    dst[i] = 0;
    return dst;
}

char *strncpy(char *dst, char *src, int n) {
    int i;
    i = 0;
    while (i < n && src[i]) {
        dst[i] = src[i];
        i = i + 1;
    }
    while (i < n) {
        dst[i] = 0;
        i = i + 1;
    }
    return dst;
}

char *strdup(char *s) {
    int len;
    char *p;
    len = strlen(s);
    p = malloc(len + 1);
    if (p) memcpy(p, s, len + 1);
    return p;
}
