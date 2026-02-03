#include <stdio.h>
#include <stdlib.h>

int main() {
    char *end;

    // Basic decimal
    double d1 = strtod("3.14", &end);
    printf("d1: %f end='%s'\n", d1, end);

    // Negative
    double d2 = strtod("-0.001", &end);
    printf("d2: %f end='%s'\n", d2, end);

    // Scientific notation
    double d3 = strtod("1.5e10", &end);
    printf("d3: %e end='%s'\n", d3, end);

    // Leading whitespace
    double d4 = strtod("  42.0", &end);
    printf("d4: %f end='%s'\n", d4, end);

    // Trailing text (endptr should point to it)
    double d5 = strtod("123.456abc", &end);
    printf("d5: %f end='%s'\n", d5, end);

    // Integer-like
    double d6 = strtod("1000", &end);
    printf("d6: %f end='%s'\n", d6, end);

    // Zero
    double d7 = strtod("0.0", &end);
    printf("d7: %f end='%s'\n", d7, end);

    // atof
    double d8 = atof("2.718281828");
    printf("d8: %f\n", d8);

    // Negative exponent
    double d9 = strtod("6.022e-4", &end);
    printf("d9: %f end='%s'\n", d9, end);

    // Large number
    double d10 = strtod("1e20", &end);
    printf("d10: %e end='%s'\n", d10, end);

    return 0;
}
