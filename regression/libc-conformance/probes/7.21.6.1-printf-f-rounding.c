/* 7.21.6.1 fprintf: %f rounds the exact binary value; exact halfway cases go to even in the default rounding mode */
#include <stdio.h>

int main(void) {
    printf("[%.0f][%.0f][%.0f][%.0f][%.0f][%.0f]\n", 0.5, 1.5, 2.5, 3.5, -0.5, -2.5);
    printf("[%.1f][%.1f][%.1f][%.2f][%.2f][%.3f]\n", 0.25, 0.75, 0.35, 1.005, 1.125, 2.0625);
    printf("[%.1f][%.2f][%.0f][%.0f]\n", 0.05, 0.015, 0.49999999999999994, 4503599627370497.0);
    printf("[%.1f][%.3f][%.0f]\n", 9.96, 9.9995, 999999.5);
    printf("[%.20f][%.30f]\n", 0.1, 1.0 / 1024);
    return 0;
}
