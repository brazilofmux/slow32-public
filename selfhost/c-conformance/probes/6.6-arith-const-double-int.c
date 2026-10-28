// 6.6p8  a double constant plus an int constant (double on the left) is an arithmetic constant initializer
#include <stdio.h>
static double a = 0.25 + 2;
int main(void) { printf("%.2f\n", a); return 0; }
