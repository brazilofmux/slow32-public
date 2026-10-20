/* HOSTLEG -- printf's length modifiers for the wider and the named types:
 * j (intmax_t), t (ptrdiff_t), z (size_t), L (long double), ll, h, hh,
 * each followed by an ordinary argument, so that one consumed at the
 * wrong width shows in what comes after it.  (Until 2026-10-19 j, t and
 * L were not known: %Lf printed as written and shifted every later
 * argument, which made libc++'s put_money print 0.) */
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

int main(void) {
    intmax_t big = -9000000000LL;
    uintmax_t ubig = 18000000000ULL;
    ptrdiff_t gap = -12345;
    size_t sz = 4000000000U;
    long double ld = 31415.875L;
    printf("j: %jd %ju %jx | %d\n", big, ubig, ubig, 7);
    printf("t: %td %tx | %d\n", gap, (ptrdiff_t)255, 8);
    printf("z: %zu %zx | %d\n", sz, sz, 9);
    printf("L: %.3Lf %Le %Lg %.0Lf | %d\n", ld, ld, ld, ld, 10);
    printf("ll: %lld %llu | %d\n", -5000000000LL, 5000000000ULL, 11);
    printf("h: %hd %hu %hhd %hhu | %d\n", (short)-300, (unsigned short)65000, (signed char)-5, (unsigned char)250, 12);
    char buf[80];
    int n = snprintf(buf, sizeof buf, "%jd/%.1Lf/%td", (intmax_t)42, (long double)2.5, (ptrdiff_t)-1);
    printf("snprintf: %d [%s]\n", n, buf);
    return 0;
}
