#include <string.h>
#include <stddef.h>
#include <stdlib.h>

// String duplication
char *strdup(const char *s) {
    if (s == NULL) return NULL;
    
    size_t len = strlen(s) + 1;
    char *dup = (char *)malloc(len);
    if (dup) {
        memcpy(dup, s, len);
    }
    return dup;
}

// String length with limit
size_t strnlen(const char *s, size_t maxlen) {
    size_t len = 0;
    while (len < maxlen && s[len]) {
        len++;
    }
    return len;
}

// strrchr is already implemented in string.c

// Find first occurrence of any character in accept
char *strpbrk(const char *s, const char *accept) {
    while (*s) {
        const char *a = accept;
        while (*a) {
            if (*s == *a) {
                return (char *)s;
            }
            a++;
        }
        s++;
    }
    return NULL;
}

// Get length of prefix consisting of characters in accept
size_t strspn(const char *s, const char *accept) {
    size_t count = 0;
    
    while (*s) {
        const char *a = accept;
        int found = 0;
        while (*a) {
            if (*s == *a) {
                found = 1;
                break;
            }
            a++;
        }
        if (!found) break;
        count++;
        s++;
    }
    
    return count;
}

// Get length of prefix not consisting of characters in reject
size_t strcspn(const char *s, const char *reject) {
    size_t count = 0;
    
    while (*s) {
        const char *r = reject;
        while (*r) {
            if (*s == *r) {
                return count;
            }
            r++;
        }
        count++;
        s++;
    }
    
    return count;
}

// String tokenization
static char *strtok_save = NULL;

char *strtok(char *str, const char *delim) {
    char *start;
    
    if (str) {
        strtok_save = str;
    } else if (!strtok_save) {
        return NULL;
    }
    
    // Skip leading delimiters
    strtok_save += strspn(strtok_save, delim);
    if (*strtok_save == '\0') {
        strtok_save = NULL;
        return NULL;
    }
    
    start = strtok_save;
    
    // Find end of token
    strtok_save = strpbrk(strtok_save, delim);
    if (strtok_save) {
        *strtok_save++ = '\0';
    }
    
    return start;
}

// Reentrant version of strtok
char *strtok_r(char *str, const char *delim, char **saveptr) {
    char *start;
    
    if (str) {
        *saveptr = str;
    } else if (!*saveptr) {
        return NULL;
    }
    
    // Skip leading delimiters
    *saveptr += strspn(*saveptr, delim);
    if (**saveptr == '\0') {
        *saveptr = NULL;
        return NULL;
    }
    
    start = *saveptr;
    
    // Find end of token
    *saveptr = strpbrk(*saveptr, delim);
    if (*saveptr) {
        *(*saveptr)++ = '\0';
    }
    
    return start;
}

// Case-insensitive string comparison: the bytes as unsigned char, as
// lower case (so 'A' and 'a' both sort after '[')
static int lower_of(unsigned char c) {
    return (c >= 'A' && c <= 'Z') ? c + 32 : c;
}

int strcasecmp(const char *s1, const char *s2) {
    for (;; s1++, s2++) {
        int c1 = lower_of((unsigned char)*s1);
        int c2 = lower_of((unsigned char)*s2);
        if (c1 != c2) return c1 - c2;
        if (c1 == 0) return 0;
    }
}

int strncasecmp(const char *s1, const char *s2, size_t n) {
    for (; n > 0; n--, s1++, s2++) {
        int c1 = lower_of((unsigned char)*s1);
        int c2 = lower_of((unsigned char)*s2);
        if (c1 != c2) return c1 - c2;
        if (c1 == 0) return 0;
    }
    return 0;
}

// Search for byte in memory (reverse)
void *memrchr(const void *s, int c, size_t n) {
    const unsigned char *p = (const unsigned char *)s + n;
    unsigned char ch = (unsigned char)c;

    while (n--) {
        if (*--p == ch) {
            return (void *)p;
        }
    }

    return NULL;
}

// Case-insensitive substring search
char *strcasestr(const char *haystack, const char *needle) {
    if (!needle[0]) return (char *)haystack;
    for (; *haystack; haystack++) {
        const char *h = haystack, *n = needle;
        while (*h && *n) {
            int ch = *h, cn = *n;
            if (ch >= 'A' && ch <= 'Z') ch += 32;
            if (cn >= 'A' && cn <= 'Z') cn += 32;
            if (ch != cn) break;
            h++; n++;
        }
        if (!*n) return (char *)haystack;
    }
    return NULL;
}

// strcoll - locale-aware string comparison (no locale support, same as strcmp)
int strcoll(const char *s1, const char *s2) {
    return strcmp(s1, s2);
}

// strxfrm - the one locale is "C": a string's transformed form is the string
size_t strxfrm(char *dest, const char *src, size_t n) {
    size_t len = strlen(src);
    if (n > 0) {
        size_t i = 0;
        for (; i < n - 1 && i < len; i++) dest[i] = src[i];
        dest[i] = '\0';
    }
    return len;
}

// strerror is strerror.c's (one table for this library and the self-hosted one)
