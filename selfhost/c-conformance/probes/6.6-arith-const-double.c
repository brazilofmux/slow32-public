// 6.6p8  arithmetic constant expressions with double operands in static initializers: 1.0/4 + 2.0, 2 + 0.25
#include <stdio.h>
static double b = 1.0 / 4 + 2.0;
static double c = 2 + 0.25;
int main(void) { printf("%.2f %.2f\n", b, c); return 0; }
