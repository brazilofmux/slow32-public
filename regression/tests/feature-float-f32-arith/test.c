// Test f32 arithmetic: fadd.s, fsub.s, fmul.s, fdiv.s, fneg.s, fabs.s
// Uses volatile to force runtime FP instructions (prevent constant folding)
#include <stdio.h>

volatile float va = 3.14f;
volatile float vb = 2.0f;
volatile float vc = -7.5f;
volatile float vd = 0.0f;

int main() {
    float a = va;
    float b = vb;
    float c = vc;
    float d = vd;

    // Basic arithmetic
    int sum = (int)(a + b);       // 3.14 + 2.0 = 5.14 -> 5
    int diff = (int)(a - b);      // 3.14 - 2.0 = 1.14 -> 1
    int prod = (int)(a * b);      // 3.14 * 2.0 = 6.28 -> 6
    int quot = (int)(a / b);      // 3.14 / 2.0 = 1.57 -> 1

    printf("add: %d\n", sum);
    printf("sub: %d\n", diff);
    printf("mul: %d\n", prod);
    printf("div: %d\n", quot);

    // Negative operands
    int negsum = (int)(a + c);    // 3.14 + (-7.5) = -4.36 -> -4
    int negprod = (int)(b * c);   // 2.0 * (-7.5) = -15.0 -> -15
    printf("neg add: %d\n", negsum);
    printf("neg mul: %d\n", negprod);

    // Negation: -(3.14) -> -3
    float neg_a = -a;
    printf("neg: %d\n", (int)neg_a);

    // Absolute value via float comparison (exercises flt.s + fneg.s)
    float abs_c = (c < 0.0f) ? -c : c;
    printf("abs: %d\n", (int)abs_c);

    // Zero arithmetic
    int zadd = (int)(a + d);     // 3.14 + 0.0 = 3.14 -> 3
    int zmul = (int)(a * d);     // 3.14 * 0.0 = 0.0 -> 0
    printf("zero add: %d\n", zadd);
    printf("zero mul: %d\n", zmul);

    return 0;
}
