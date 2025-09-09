#include <string.h>
#include <stddef.h>

// Note: memmove and memchr are already in string.c

// memrchr - search for byte in memory (reverse)
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