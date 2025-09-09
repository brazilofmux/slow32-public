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

// Case-insensitive string comparison
int strcasecmp(const char *s1, const char *s2) {
    while (*s1 && *s2) {
        int c1 = *s1;
        int c2 = *s2;
        
        // Convert to lowercase for comparison
        if (c1 >= 'A' && c1 <= 'Z') c1 += 32;
        if (c2 >= 'A' && c2 <= 'Z') c2 += 32;
        
        if (c1 != c2) {
            return c1 - c2;
        }
        
        s1++;
        s2++;
    }
    
    return *s1 - *s2;
}

int strncasecmp(const char *s1, const char *s2, size_t n) {
    while (n-- && *s1 && *s2) {
        int c1 = *s1;
        int c2 = *s2;
        
        // Convert to lowercase for comparison
        if (c1 >= 'A' && c1 <= 'Z') c1 += 32;
        if (c2 >= 'A' && c2 <= 'Z') c2 += 32;
        
        if (c1 != c2) {
            return c1 - c2;
        }
        
        s1++;
        s2++;
    }
    
    if (n == (size_t)-1) {
        return 0;
    }
    
    return *s1 - *s2;
}