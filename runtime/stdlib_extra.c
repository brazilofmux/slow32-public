#include <stdlib.h>
#include <stddef.h>

// Quick sort implementation
static void swap(void *a, void *b, size_t size) {
    unsigned char *pa = (unsigned char *)a;
    unsigned char *pb = (unsigned char *)b;
    unsigned char tmp;
    
    while (size--) {
        tmp = *pa;
        *pa++ = *pb;
        *pb++ = tmp;
    }
}

static void *partition(void *base, size_t num, size_t size, 
                      int (*compar)(const void *, const void *)) {
    char *array = (char *)base;
    char *pivot = array + (num - 1) * size;
    size_t i = 0;
    
    for (size_t j = 0; j < num - 1; j++) {
        if (compar(array + j * size, pivot) <= 0) {
            swap(array + i * size, array + j * size, size);
            i++;
        }
    }
    swap(array + i * size, pivot, size);
    return array + i * size;
}

static void qsort_recursive(void *base, size_t num, size_t size,
                           int (*compar)(const void *, const void *)) {
    if (num <= 1) return;
    
    char *pivot = (char *)partition(base, num, size, compar);
    size_t left_size = (pivot - (char *)base) / size;
    size_t right_size = num - left_size - 1;
    
    qsort_recursive(base, left_size, size, compar);
    qsort_recursive(pivot + size, right_size, size, compar);
}

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *)) {
    if (base == NULL || nmemb == 0 || size == 0 || compar == NULL) {
        return;
    }
    qsort_recursive(base, nmemb, size, compar);
}

// Binary search implementation
void *bsearch(const void *key, const void *base, size_t nmemb, size_t size,
              int (*compar)(const void *, const void *)) {
    if (base == NULL || nmemb == 0 || size == 0 || compar == NULL) {
        return NULL;
    }
    
    const char *array = (const char *)base;
    size_t left = 0;
    size_t right = nmemb;
    
    while (left < right) {
        size_t mid = left + (right - left) / 2;
        const void *mid_elem = array + mid * size;
        int cmp = compar(key, mid_elem);
        
        if (cmp < 0) {
            right = mid;
        } else if (cmp > 0) {
            left = mid + 1;
        } else {
            return (void *)mid_elem;
        }
    }
    
    return NULL;
}

// String to long conversion with error checking
long strtol(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    long result = 0;
    int sign = 1;
    
    // Skip whitespace
    while (*s == ' ' || (*s >= '\t' && *s <= '\r')) s++;
    
    // Handle sign
    if (*s == '-') {
        sign = -1;
        s++;
    } else if (*s == '+') {
        s++;
    }
    
    // Handle base
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') {
                base = 16;
                s++;
            } else {
                base = 8;
            }
        } else {
            base = 10;
        }
    } else if (base == 16) {
        if (*s == '0' && (s[1] == 'x' || s[1] == 'X')) {
            s += 2;
        }
    }
    
    // Convert digits
    while (*s) {
        int digit;
        if (*s >= '0' && *s <= '9') {
            digit = *s - '0';
        } else if (*s >= 'a' && *s <= 'z') {
            digit = *s - 'a' + 10;
        } else if (*s >= 'A' && *s <= 'Z') {
            digit = *s - 'A' + 10;
        } else {
            break;
        }
        
        if (digit >= base) break;
        
        result = result * base + digit;
        s++;
    }
    
    if (endptr) {
        *endptr = (char *)s;
    }
    
    return result * sign;
}

// String to unsigned long conversion
unsigned long strtoul(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    unsigned long result = 0;
    
    // Skip whitespace
    while (*s == ' ' || (*s >= '\t' && *s <= '\r')) s++;
    
    // Skip optional plus sign
    if (*s == '+') s++;
    
    // Handle base
    if (base == 0) {
        if (*s == '0') {
            s++;
            if (*s == 'x' || *s == 'X') {
                base = 16;
                s++;
            } else {
                base = 8;
            }
        } else {
            base = 10;
        }
    } else if (base == 16) {
        if (*s == '0' && (s[1] == 'x' || s[1] == 'X')) {
            s += 2;
        }
    }
    
    // Convert digits
    while (*s) {
        int digit;
        if (*s >= '0' && *s <= '9') {
            digit = *s - '0';
        } else if (*s >= 'a' && *s <= 'z') {
            digit = *s - 'a' + 10;
        } else if (*s >= 'A' && *s <= 'Z') {
            digit = *s - 'A' + 10;
        } else {
            break;
        }
        
        if (digit >= base) break;
        
        result = result * base + digit;
        s++;
    }
    
    if (endptr) {
        *endptr = (char *)s;
    }
    
    return result;
}

// Division functions
// Types are already defined in stdlib.h

div_t div(int numer, int denom) {
    div_t result;
    result.quot = numer / denom;
    result.rem = numer % denom;
    return result;
}

ldiv_t ldiv(long numer, long denom) {
    ldiv_t result;
    result.quot = numer / denom;
    result.rem = numer % denom;
    return result;
}