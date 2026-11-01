/* 7.21.6.1 fprintf: %a and %A hexadecimal floating output, exact by default, with precision, zero and denormals */
#include <stdio.h>

int main(void) {
    printf("[%a][%A][%a][%a][%a]\n", 1.0, 1.0, 0.5, 1.5, -2.0);
    printf("[%a][%a][%A]\n", 0.0, -0.0, 0.0);
    printf("[%.0a][%.1a][%.3a][%.2a][%#.0a]\n", 1.0, 1.0, 1.0, 1.0 + 1.0 / 1024, 1.0);
    printf("[%a][%a]\n", 0.1, 1e300);
    printf("[%12a][%-12a|][%012a][%+a]\n", 1.0, 1.0, 1.0, 1.0);
    return 0;
}
