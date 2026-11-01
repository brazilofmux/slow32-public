/* 7.21.6.1 fprintf: %g chooses f or e style from the exponent and precision and drops trailing zeros unless # */
#include <stdio.h>

int main(void) {
    printf("[%g][%g][%g][%g][%g][%g]\n", 100000.0, 1000000.0, 0.0001, 0.00001, 123456789.0, 1.5);
    printf("[%g][%g][%G][%G][%g][%g]\n", 0.0, -0.0, 1e-10, 1.5e20, 1e100, 3.0);
    printf("[%.0g][%.1g][%.2g][%.3g][%.3g][%.10g]\n", 0.0, 15.0, 0.000123456, 1234.0, 0.1, 1.0 / 3.0);
    printf("[%#g][%#g][%#.3g][%#.0g][%#g][%#.2g]\n", 1.5, 0.0, 1.0, 2.0, 1e-5, 100.0);
    printf("[%g][%g][%g][%g]\n", 999999.5, 9999995.0, 0.000099999995, 99999.95);
    printf("[%10g][%-10g][%010g][%+g][% g][%.20g]\n", 1.5, 1.5, -1.5, 1.5, 1.5, 0.1);
    printf("[%.0g][%.0g][%.1g][%.1g]\n", 1e-5, 25.0, 0.05, 950.0);
    return 0;
}
