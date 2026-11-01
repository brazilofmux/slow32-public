/* 7.21.6.1 fprintf: %e and %E give one digit before the point and an exponent of at least two digits */
#include <stdio.h>

int main(void) {
    printf("[%e][%E][%e][%e][%e]\n", 0.0, 1.0, -0.0, 123456.789, 0.000123);
    printf("[%.0e][%#.0e][%.2e][%.0e][%.0e][%.0e]\n", 3.0, 3.0, 1234.5, 15.0, 25.0, 35.0);
    printf("[%e][%e][%E][%.3e]\n", 1e100, 1e-100, 1.7976931348623157e308, 2.2250738585072014e-308);
    printf("[%.16e][%.0e][%e]\n", 4.9406564584124654e-324, 4.9406564584124654e-324, 9.9999996);
    printf("[%12.3e][%-12.3e][%012.3e][%+e][% e]\n", 1.5, 1.5, -1.5, 1.5, 1.5);
    printf("[%.1e][%.2e][%.0e]\n", 9.96, 9.995, 9.5);
    return 0;
}
