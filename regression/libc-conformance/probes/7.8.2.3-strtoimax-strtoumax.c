/* 7.8.2.3: strtoimax and strtoumax convert like strtoll and strtoull, with ERANGE at the intmax_t limits */
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>

int main(void) {
    char *end;
    intmax_t a = strtoimax("  -123456789012x", &end, 10);
    printf("%lld %c\n", (long long)a, *end);
    uintmax_t b = strtoumax("0xFFFFFFFFFFFFFFFF", &end, 0);
    printf("%d %d\n", b == UINTMAX_MAX, *end == 0);
    errno = 0;
    a = strtoimax("99999999999999999999999", &end, 10);
    printf("%d %d\n", a == INTMAX_MAX, errno == ERANGE);
    return 0;
}
