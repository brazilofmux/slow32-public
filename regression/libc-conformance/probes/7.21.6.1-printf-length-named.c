/* 7.21.6.1 fprintf: the j z t and l length modifiers with each integer conversion */
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

int main(void) {
    char a[10];
    ptrdiff_t d = &a[2] - &a[9];
    printf("%jd %ju %jx %jo %ji\n", (intmax_t)-5, (uintmax_t)5000000000u, (uintmax_t)0xfeedfacecafeULL, (uintmax_t)8, (intmax_t)9);
    printf("%zu %zx %zo %zd\n", sizeof a, (size_t)255, (size_t)8, (size_t)17);
    printf("%td %ti %tu %tx\n", d, d, (ptrdiff_t)7, (ptrdiff_t)255);
    printf("%ld %li %lu %lx %lo %lX\n", -3L, -4L, 5UL, 255UL, 8UL, 255UL);
    printf("%lld %lli %llu %llx %llX %llo | %d\n", -9000000000LL, 9000000000LL, 9000000000ULL, 0xdeadbeefcafeULL, 0xdeadbeefcafeULL, 8ULL, 1);
    return 0;
}
