/* 7.15: alignas and alignof are macros for _Alignas and _Alignof; __alignas_is_defined and __alignof_is_defined are 1 */
#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#if defined(alignas) && defined(alignof) && __alignas_is_defined == 1 && __alignof_is_defined == 1
#define MACROS 1
#else
#define MACROS 0
#endif

static alignas(max_align_t) char gbuf[3];
struct s { char c; alignas(double) char d; };

int main(void) {
    alignas(long long) char lbuf[5];
    alignas(8) char eight;
    printf("macros %d\n", MACROS);
    printf("alignof %d %d %d\n", alignof(char) == 1, alignof(double) == _Alignof(double), alignof(struct s) >= alignof(double));
    printf("placed %d %d %d %d\n", (uintptr_t)gbuf % alignof(max_align_t) == 0, (uintptr_t)lbuf % alignof(long long) == 0,
           (uintptr_t)&eight % 8 == 0, offsetof(struct s, d) % alignof(double) == 0);
    return 0;
}
