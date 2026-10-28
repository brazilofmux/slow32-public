// 6.6p8  float constants in a static initializer: 3 / 2.0f
#include <stdio.h>
static float f = 3 / 2.0f;
static float g = 1.5f;
int main(void) { printf("%.2f %.2f\n", f, g); return 0; }
