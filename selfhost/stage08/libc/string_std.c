/* The rest of <string.h>: strnlen, strpbrk, strtok, strtok_r, memrchr.
 * The library had none of them (selfhost ISSUES-75).  strcoll and
 * strxfrm, which were here for the one locale "C", are
 * runtime/locale_state.c's now, LC_COLLATE's.
 * Built in phase 2 only -- nothing the compiler or the tools use. */
#include <string.h>

size_t strnlen(const char *s, size_t max) {
    size_t n = 0;
    while (n < max && s[n]) n++;
    return n;
}

char *strpbrk(const char *s, const char *accept) {
    const char *a;
    for (; *s; s++) {
        for (a = accept; *a; a++) {
            if (*a == *s) return (char *)s;
        }
    }
    return 0;
}

/* The next token of the string, or of the one a call before this began:
 * delimiters are skipped, the token runs to the next delimiter, which
 * becomes its terminator, and the scan resumes past it.  The delimiters
 * may differ from call to call. */
char *strtok_r(char *s, const char *delim, char **save) {
    char *tok;
    if (!s) s = *save;
    if (!s) return 0;
    s += strspn(s, delim);
    if (!*s) {
        *save = 0;
        return 0;
    }
    tok = s;
    s += strcspn(s, delim);
    if (*s) {
        *s = 0;
        *save = s + 1;
    } else {
        *save = 0;
    }
    return tok;
}

static char *strtok_save;

char *strtok(char *s, const char *delim) {
    return strtok_r(s, delim, &strtok_save);
}

void *memrchr(const void *s, int c, size_t n) {
    const unsigned char *p = (const unsigned char *)s + n;
    while (n > 0) {
        p--;
        n--;
        if (*p == (unsigned char)c) return (void *)p;
    }
    return 0;
}
