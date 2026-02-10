#include <stdlib.h>
#include <stddef.h>
#include <string.h>

// Quick sort implementation
// swap uses memswap — a single call, no temp buffer, word-optimized in the
// runtime, and interceptable by the DBT for a native host swap loop.
static void swap(void *a, void *b, size_t size) {
    memswap(a, b, size);
}

static void *partition_r(void *base, size_t num, size_t size, 
                        int (*compar)(const void *, const void *, void *),
                        void *arg) {
    char *array = (char *)base;
    char *pivot = array + (num - 1) * size;
    size_t i = 0;
    
    for (size_t j = 0; j < num - 1; j++) {
        if (compar(array + j * size, pivot, arg) <= 0) {
            swap(array + i * size, array + j * size, size);
            i++;
        }
    }
    swap(array + i * size, pivot, size);
    return array + i * size;
}

static void qsort_r_recursive(void *base, size_t num, size_t size,
                             int (*compar)(const void *, const void *, void *),
                             void *arg) {
    if (num <= 1) return;
    
    char *pivot = (char *)partition_r(base, num, size, compar, arg);
    size_t left_size = (pivot - (char *)base) / size;
    size_t right_size = num - left_size - 1;
    
    qsort_r_recursive(base, left_size, size, compar, arg);
    qsort_r_recursive(pivot + size, right_size, size, compar, arg);
}

void qsort_r(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *, void *),
             void *arg) {
    if (base == NULL || nmemb == 0 || size == 0 || compar == NULL) {
        return;
    }
    qsort_r_recursive(base, nmemb, size, compar, arg);
}

// Compatibility wrapper for standard qsort
static int qsort_compat_compar(const void *a, const void *b, void *arg) {
    int (*compar)(const void *, const void *) = (int (*)(const void *, const void *))arg;
    return compar(a, b);
}

void qsort(void *base, size_t nmemb, size_t size,
           int (*compar)(const void *, const void *)) {
    qsort_r(base, nmemb, size, qsort_compat_compar, (void *)compar);
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
    const char *digits_start;
    long result = 0;
    int sign = 1;
    int found_digit = 0;

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
                // The leading '0' counts as a digit
                found_digit = 1;
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
    digits_start = s;
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

        found_digit = 1;
        result = result * base + digit;
        s++;
    }

    if (endptr) {
        // Per C standard: if no conversion performed, set endptr to nptr
        *endptr = found_digit ? (char *)s : (char *)nptr;
    }

    return result * sign;
}

// strtoul is now in convert_extra.c

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
