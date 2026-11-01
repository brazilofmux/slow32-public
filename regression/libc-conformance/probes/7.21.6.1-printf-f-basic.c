/* 7.21.6.1 fprintf: %f and %F default precision 6, explicit precision, flags, and the # flag at precision 0 */
#include <stdio.h>

int main(void) {
    printf("[%f][%F][%f][%f][%f]\n", 1.5, 2.0, -0.125, 0.0, -0.0);
    printf("[%.0f][%.1f][%.2f][%.10f][%#.0f][%#.0f]\n", 3.0, 3.25, 3.0, 0.5, 3.0, 0.0);
    printf("[%10.2f][%-10.2f][%010.2f][%+.2f][% .2f][%+010.1f]\n", 3.14159, 3.14159, -3.14159, 3.14159, 3.14159, 2.5);
    printf("[%f][%.3f][%f]\n", 123456789.0, 1e-4, 1.0 / 3.0);
    return 0;
}
