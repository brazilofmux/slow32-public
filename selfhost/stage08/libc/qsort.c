/* qsort / qsort_r for the stage08 libc: the runtime's median-of-three
 * quicksort with insertion sort on small runs (runtime/stdlib_extra.c),
 * pulled in by regal (GitHub issue 73 follow-on: regal under stage08).
 * Only the sort lives here; div/ldiv/strtol stay in convert.c. */
#include <stdlib.h>
#include <stddef.h>
#include <string.h>

// Quick sort implementation
// Median-of-three pivot + recurse-on-smaller/iterate-on-larger to guarantee
// O(log n) stack depth.  Insertion sort for small partitions.

#define QSORT_INSERTION_THRESHOLD 16

/* The runtime's memswap is assembly (intrinsics.s); a byte loop is
 * enough here -- qsort swaps small elements. */
void memswap(void *a, void *b, size_t n) {
    unsigned char *p = (unsigned char *)a;
    unsigned char *q = (unsigned char *)b;
    unsigned char t;
    while (n > 0) { t = *p; *p = *q; *q = t; p++; q++; n--; }
}

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

// String to long conversion with error checking
