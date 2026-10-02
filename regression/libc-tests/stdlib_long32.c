/* <stdlib.h> where the answer is this machine's: long is 32 bits, so
 * strtol and strtoul clamp there and say ERANGE; and rand's sequence,
 * which the standard leaves to the library -- a program must print the
 * same numbers whichever compiler built it. */
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <errno.h>

static void tl(const char *s, int base) {
    char *e = 0;
    long v;
    errno = 0;
    v = strtol(s, &e, base);
    printf("strtol[%s,%d] = %ld consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

static void tul(const char *s, int base) {
    char *e = 0;
    unsigned long v;
    errno = 0;
    v = strtoul(s, &e, base);
    printf("strtoul[%s,%d] = %lu consumed=%d erange=%d\n", s, base, v, (int)(e - s), errno == ERANGE);
}

int main(void) {
    int i;
    printf("sizeof long %d LONG_MAX %ld\n", (int)sizeof(long), LONG_MAX);
    tl("2147483647", 10); tl("2147483648", 10); tl("-2147483648", 10); tl("-2147483649", 10);
    tl("99999999999999999999", 10); tl("-99999999999999999999xyz", 10); tl("0x80000000", 0);
    tl("0x7fffffff", 0); tl("zzzzzzzz", 36); tl("11111111111111111111111111111111", 2);
    tl("1111111111111111111111111111111", 2);
    tul("4294967295", 10); tul("4294967296", 10); tul("-1", 10); tul("-4294967295", 10);
    tul("-4294967296", 10); tul("0x100000000", 0); tul("99999999999", 10);
    printf("atoi at the limits %d %d\n", atoi("2147483647"), atoi("-2147483648"));
    printf("system(0): %d (there is no command processor), system(\"ls\"): %d\n", system(0), system("ls"));
    printf("RAND_MAX %d\n", RAND_MAX);
    printf("rand");
    for (i = 0; i < 6; i++) printf(" %d", rand());
    printf("\n");
    srand(12345);
    printf("srand(12345)");
    for (i = 0; i < 6; i++) printf(" %d", rand());
    printf("\n");
    srand(0xFFFFFFFFu);
    printf("srand(-1)");
    for (i = 0; i < 6; i++) printf(" %d", rand());
    printf("\n");
    return 0;
}
