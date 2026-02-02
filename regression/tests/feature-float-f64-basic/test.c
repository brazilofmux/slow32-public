// Test f64 (double) arithmetic, comparison, and conversion
// Uses volatile to force runtime FP instructions
#include <stdio.h>

volatile double da = 3.14;
volatile double db = 2.0;
volatile double dc = -7.5;
volatile int vi = 42;

int main() {
    double a = da;
    double b = db;
    double c = dc;

    // Basic arithmetic
    int sum = (int)(a + b);       // 5.14 -> 5
    int diff = (int)(a - b);      // 1.14 -> 1
    int prod = (int)(a * b);      // 6.28 -> 6
    int quot = (int)(a / b);      // 1.57 -> 1
    printf("d add: %d\n", sum);
    printf("d sub: %d\n", diff);
    printf("d mul: %d\n", prod);
    printf("d div: %d\n", quot);

    // Negative operands
    int negsum = (int)(a + c);    // -4.36 -> -4
    printf("d neg add: %d\n", negsum);

    // Comparisons including equality
    printf("a>b: %d\n", a > b);    // 1
    printf("b>a: %d\n", b > a);    // 0
    printf("a==a: %d\n", a == a);  // 1
    printf("c<a: %d\n", c < a);    // 1

    // int -> double -> int roundtrip
    int x = vi;
    double dx = (double)x;
    dx = dx + b;                   // 42.0 + 2.0 = 44.0
    int result = (int)dx;
    printf("d 42+2: %d\n", result);

    // f32 -> f64 promotion
    volatile float vf = 3.14f;
    float f = vf;
    double promoted = (double)f;
    int promo_result = (int)(promoted + b);  // 3.14 + 2.0 = 5.14 -> 5
    printf("f2d: %d\n", promo_result);

    return 0;
}
