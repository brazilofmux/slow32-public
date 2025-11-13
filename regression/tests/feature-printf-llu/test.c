#include <stdint.h>
#include <stdio.h>

static void print_value(unsigned long long v) {
    printf("value=%llu\n", v);
}

int main(void) {
    print_value(0ULL);
    print_value(42ULL);
    print_value(4294967295ULL);
    print_value(4294967296ULL);
    print_value(1234567890123456789ULL);
    print_value(18446744073709551615ULL);
    return 0;
}
