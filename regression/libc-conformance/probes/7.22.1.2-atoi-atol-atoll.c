/* 7.22.1.2 atoi, atol, atoll: white space, an optional sign, decimal digits, trailing junk ignored */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

int main(void) {
    printf("%d %d %d %d %d\n", atoi("  -42abc"), atoi("+7"), atoi("x9"), atoi("\n\t 0012"), atoi("- 5"));
    printf("%ld %ld\n", atol(" -2147483647"), atol("0x10"));
    printf("%lld %lld\n", atoll("9223372036854775807"), atoll("-9223372036854775807 tail"));
    return 0;
}
