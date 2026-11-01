/* 7.21.6.1 fprintf: the - + space 0 flags, width and precision with %d and %i */
#include <stdio.h>

int main(void) {
    printf("[%d][%i][%5d][%-5d][%05d][%+d][%+d][% d][% d]\n", 42, -42, 42, 42, -42, 42, -42, 42, -42);
    printf("[%+ d][% +d][%-+6d][%-06d][%+05d][% 05d]\n", 7, 7, 7, 7, 7, 7);
    printf("[%.3d][%.3d][%8.3d][%-8.3d][%08.3d][%.0d][%5.0d][%+.0d][% .0d]\n", 7, -7, 7, -7, 7, 0, 0, 0, 0);
    printf("[%.0d][%.0i][%.1d][%3.1d]\n", 1, 0, 0, 0);
    printf("[%1d][%2d][%010d][%-010d|]\n", 12345, -12345, -12345, -12345);
    return 0;
}
