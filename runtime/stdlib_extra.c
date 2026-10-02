#include <stdlib.h>
#include <errno.h>
#include <stddef.h>
#include <string.h>

// Quick sort implementation
// Median-of-three pivot + recurse-on-smaller/iterate-on-larger to guarantee
// O(log n) stack depth.  Insertion sort for small partitions.

#define QSORT_INSERTION_THRESHOLD 16

static void insertion_sort_r(char *base, size_t num, size_t size,
                             int (*compar)(const void *, const void *, void *),
                             void *arg) {
    for (size_t i = 1; i < num; i++) {
        size_t j = i;
        while (j > 0 && compar(base + (j - 1) * size, base + j * size, arg) > 0) {
            memswap(base + (j - 1) * size, base + j * size, size);
            j--;
        }
    }
}

static void median_of_three(char *base, size_t num, size_t size,
                             int (*compar)(const void *, const void *, void *),
                             void *arg) {
    char *a = base;
    char *b = base + (num / 2) * size;
    char *c = base + (num - 1) * size;
    if (compar(a, b, arg) > 0) memswap(a, b, size);
    if (compar(b, c, arg) > 0) memswap(b, c, size);
    if (compar(a, b, arg) > 0) memswap(a, b, size);
    /* Median is now in b; move it to last position as pivot */
    memswap(b, c, size);
}

void qsort_r(void *base, size_t nmemb, size_t size,
             int (*compar)(const void *, const void *, void *),
             void *arg) {
    char *lo = (char *)base;
    size_t num = nmemb;

    if (base == NULL || nmemb == 0 || size == 0 || compar == NULL)
        return;

    for (;;) {
        if (num <= 1) return;
        if (num <= QSORT_INSERTION_THRESHOLD) {
            insertion_sort_r(lo, num, size, compar, arg);
            return;
        }

        /* Median-of-three pivot selection, pivot placed at end */
        median_of_three(lo, num, size, compar, arg);

        /* Lomuto partition with pivot at last position */
        {
            char *pivot = lo + (num - 1) * size;
            size_t i = 0;
            size_t j;
            size_t left_size, right_size;

            for (j = 0; j < num - 1; j++) {
                if (compar(lo + j * size, pivot, arg) <= 0) {
                    memswap(lo + i * size, lo + j * size, size);
                    i++;
                }
            }
            memswap(lo + i * size, pivot, size);

            left_size = i;
            right_size = num - i - 1;

            /* Recurse on smaller partition, iterate on larger */
            if (left_size <= right_size) {
                qsort_r(lo, left_size, size, compar, arg);
                lo = lo + (i + 1) * size;
                num = right_size;
            } else {
                qsort_r(lo + (i + 1) * size, right_size, size, compar, arg);
                num = left_size;
            }
        }
    }
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

// String to long conversion, as the standard has it: the white space
// isspace knows, a 0x that is a prefix only before a hexadecimal digit, a
// value past either end of long clamped there with ERANGE (every digit
// consumed all the same), the end pointer left at the start when nothing
// was converted.  The value is gathered below zero: a negative long has
// one more value than a positive one, so every number that fits can be
// reached that way.
long strtol(const char *nptr, char **endptr, int base) {
    const char *s = nptr;
    const char *after = 0;      // past the last digit taken
    long acc = 0;
    int neg = 0, over = 0;

    if (base != 0 && (base < 2 || base > 36)) {
        errno = EINVAL;
        if (endptr) *endptr = (char *)nptr;
        return 0;
    }

    while (*s == ' ' || (*s >= '\t' && *s <= '\r')) s++;

    if (*s == '-') {
        neg = 1;
        s++;
    } else if (*s == '+') {
        s++;
    }

    if ((base == 0 || base == 16) && s[0] == '0' && (s[1] == 'x' || s[1] == 'X')) {
        int c = s[2];
        if ((c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')) {
            s += 2;
            base = 16;
        }
    }
    if (base == 0) base = (*s == '0') ? 8 : 10;

    long limit = neg ? (-2147483647L - 1) : -2147483647L;
    long multmin = limit / base;

    for (;; s++) {
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

        if (!over) {
            if (acc < multmin) {
                over = 1;
            } else {
                acc *= base;
                if (acc < limit + digit) over = 1;
                else acc -= digit;
            }
        }
        after = s + 1;
    }

    if (endptr) *endptr = (char *)(after ? after : nptr);
    if (over) {
        errno = ERANGE;
        return neg ? limit : -limit;
    }
    return neg ? acc : -acc;
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
