/* 7.21.6.1 fprintf: %n stores the count so far into int, signed char, short, long, long long, intmax_t, size_t, ptrdiff_t */
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>

int main(void) {
    int n1 = -1, n2 = -1;
    signed char hh = -1;
    short h = -1;
    long l = -1;
    long long ll = -1;
    intmax_t j = -1;
    size_t z = 99;
    ptrdiff_t t = -1;
    int r = printf("ab%ncde%n|\n", &n1, &n2);
    printf("%d %d %d\n", r, n1, n2);
    char b[64];
    r = snprintf(b, sizeof b, "%n12%hhn345%hn6%ln7%lln8%jn9%zn0%tn", &n1, &hh, &h, &l, &ll, &j, &z, &t);
    printf("%d %s %d %d %d %ld %lld %jd %zu %td\n", r, b, n1, hh, h, l, ll, j, z, t);
    r = snprintf(b, 4, "abcdef%n", &n1);
    printf("%d %d [%s]\n", r, n1, b);
    r = printf("%5d%n\n", 1, &n1);
    printf("%d %d\n", r, n1);
    return 0;
}
