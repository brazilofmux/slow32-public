// Test f32 comparisons: feq.s, flt.s, fle.s
// Uses volatile to force runtime FP instructions
#include <stdio.h>

volatile float va = 3.14f;
volatile float vb = 2.0f;
volatile float vc = 3.14f;
volatile float vd = -1.0f;
volatile float ve = 0.0f;

int main() {
    float a = va;   // 3.14
    float b = vb;   // 2.0
    float c = vc;   // 3.14 (same as a)
    float d = vd;   // -1.0
    float e = ve;   // 0.0

    // Equal
    printf("a==c: %d\n", a == c);  // 1
    printf("a==b: %d\n", a == b);  // 0

    // Less than
    printf("b<a: %d\n", b < a);    // 1
    printf("a<b: %d\n", a < b);    // 0
    printf("a<c: %d\n", a < c);    // 0 (equal)

    // Less than or equal
    printf("b<=a: %d\n", b <= a);  // 1
    printf("a<=c: %d\n", a <= c);  // 1 (equal)
    printf("a<=b: %d\n", a <= b);  // 0

    // Greater than
    printf("a>b: %d\n", a > b);    // 1
    printf("b>a: %d\n", b > a);    // 0

    // Negative comparisons
    printf("d<e: %d\n", d < e);    // 1 (-1 < 0)
    printf("d<a: %d\n", d < a);    // 1 (-1 < 3.14)
    printf("e==e: %d\n", e == e);  // 1 (0 == 0)

    return 0;
}
