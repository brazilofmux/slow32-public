/*
 * Bisect part 6: printf varargs alignment
 */
#include <stdio.h>
#include <math.h>

int main() {
    double v1 = 1.0;
    double v2 = 2.0;
    double v3 = 3.0;
    double v4 = 4.0;

    /* Works: 4 doubles */
    printf("A: %f %f %f %f\n", v1, v2, v3, v4);

    /* Test: 1 string + 4 doubles (double starts at odd register) */
    printf("B: %s %f %f %f %f\n", "X", v1, v2, v3, v4);

    /* Test: 1 int + 4 doubles */
    printf("C: %d %f %f %f %f\n", 42, v1, v2, v3, v4);

    /* Test: 2 strings + 3 doubles */
    printf("D: %s %s %f %f %f\n", "X", "Y", v1, v2, v3);

    /* Test: 1 string + 1 double (minimum broken case?) */
    printf("E: %s %f\n", "X", v1);

    /* Test: 1 string + 2 doubles */
    printf("F: %s %f %f\n", "X", v1, v2);

    /* Test: 1 string + 3 doubles */
    printf("G: %s %f %f %f\n", "X", v1, v2, v3);

    return 0;
}
