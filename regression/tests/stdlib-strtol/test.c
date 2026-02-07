#include <stdio.h>
#include <stdlib.h>

int main() {
    char *end;

    /* strtol decimal */
    printf("%s: strtol decimal\n", strtol("12345", NULL, 10) == 12345 ? "PASS" : "FAIL");

    /* strtol negative */
    printf("%s: strtol negative\n", strtol("-42", NULL, 10) == -42 ? "PASS" : "FAIL");

    /* strtol hex with 0x */
    printf("%s: strtol hex 0x\n", strtol("0xFF", NULL, 16) == 255 ? "PASS" : "FAIL");

    /* strtol hex without 0x */
    printf("%s: strtol hex\n", strtol("FF", NULL, 16) == 255 ? "PASS" : "FAIL");

    /* strtol octal with 0 prefix */
    printf("%s: strtol octal\n", strtol("077", NULL, 8) == 63 ? "PASS" : "FAIL");

    /* strtol auto-detect base (0) */
    printf("%s: strtol auto dec\n", strtol("99", NULL, 0) == 99 ? "PASS" : "FAIL");
    printf("%s: strtol auto hex\n", strtol("0x1A", NULL, 0) == 26 ? "PASS" : "FAIL");
    printf("%s: strtol auto oct\n", strtol("010", NULL, 0) == 8 ? "PASS" : "FAIL");

    /* strtol endptr */
    long val = strtol("123abc", &end, 10);
    printf("%s: strtol endptr val\n", val == 123 ? "PASS" : "FAIL");
    printf("%s: strtol endptr pos\n", *end == 'a' ? "PASS" : "FAIL");

    /* strtol leading whitespace */
    printf("%s: strtol whitespace\n", strtol("   42", NULL, 10) == 42 ? "PASS" : "FAIL");

    /* strtol empty/invalid */
    val = strtol("", &end, 10);
    printf("%s: strtol empty\n", val == 0 && end[0] == '\0' ? "PASS" : "FAIL");

    val = strtol("xyz", &end, 10);
    printf("%s: strtol invalid\n", val == 0 && end[0] == 'x' ? "PASS" : "FAIL");

    /* strtoul */
    printf("%s: strtoul\n", strtoul("4294967295", NULL, 10) == 4294967295u ? "PASS" : "FAIL");
    printf("%s: strtoul hex\n", strtoul("DEADBEEF", NULL, 16) == 0xDEADBEEF ? "PASS" : "FAIL");

    /* atoi */
    printf("%s: atoi basic\n", atoi("99") == 99 ? "PASS" : "FAIL");
    printf("%s: atoi negative\n", atoi("-7") == -7 ? "PASS" : "FAIL");
    printf("%s: atoi whitespace\n", atoi("  123") == 123 ? "PASS" : "FAIL");

    /* atol */
    printf("%s: atol\n", atol("100000") == 100000 ? "PASS" : "FAIL");

    /* strtoll 64-bit */
    long long llv = strtoll("2147483648", NULL, 10);
    printf("%s: strtoll large\n", llv == 2147483648LL ? "PASS" : "FAIL");

    llv = strtoll("-2147483649", NULL, 10);
    printf("%s: strtoll neg large\n", llv == -2147483649LL ? "PASS" : "FAIL");

    /* strtoull 64-bit */
    unsigned long long ullv = strtoull("4294967296", NULL, 10);
    printf("%s: strtoull large\n", ullv == 4294967296ULL ? "PASS" : "FAIL");

    /* atoll */
    printf("%s: atoll\n", atoll("9876543210") == 9876543210LL ? "PASS" : "FAIL");

    return 0;
}
