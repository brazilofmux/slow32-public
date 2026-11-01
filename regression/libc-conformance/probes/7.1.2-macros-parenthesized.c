/* 7.1.2p5: the object-like limit macros group as a single identifier would inside a larger expression */
#include <limits.h>
#include <stdint.h>
#include <stdio.h>

#define G(m) ((m) / 2 == (m / 2) && (m) % 3 == (m % 3) && (m) * 0 == m * 0)

int main(void) {
    printf("limits %d %d %d %d %d %d %d\n", G(SCHAR_MIN), G(SHRT_MIN), G(INT_MIN), G(LONG_MIN),
           G(LLONG_MIN), G(CHAR_MIN), G(INT_MAX));
    printf("stdint %d %d %d %d %d\n", G(INT8_MIN), G(INT16_MIN), G(INT32_MIN), G(INT64_MIN), G(INTMAX_MIN));
    return 0;
}
