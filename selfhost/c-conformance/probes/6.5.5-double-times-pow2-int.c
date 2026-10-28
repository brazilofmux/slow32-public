// 6.5.5p4  a double times an integer constant that is a power of two
#include <stdio.h>
double twice(double f) { return f * 2; }
double eight(double f) { return f * 8; }
float ftwice(float f) { return f * 2; }
int main(void) {
    volatile double v = 1.5;
    volatile float w = 1.5f;
    printf("%d %d %d\n", (int)(twice(v) * 10), (int)(eight(v) * 10), (int)(ftwice(w) * 10));
    return 0;
}
