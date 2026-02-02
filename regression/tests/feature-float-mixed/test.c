// Test mixed f32/f64 conversions and arithmetic
// Uses volatile to force runtime instructions
#include <stdio.h>

volatile int vi = 100;
volatile float vf = 3.14f;
volatile double vd = 2.71828;

int main() {
    int i = vi;
    float f = vf;
    double d = vd;

    // int -> float arithmetic
    float sum_f = (float)i + f;     // 100.0 + 3.14 = 103.14
    printf("i+f: %d\n", (int)sum_f);

    // int -> double arithmetic
    double sum_d = (double)i + d;   // 100.0 + 2.71828 = 102.71828
    printf("i+d: %d\n", (int)sum_d);

    // float -> double promotion + arithmetic
    double promoted = (double)f;    // 3.14 as double
    double result_d = promoted + d; // 3.14 + 2.71828 = 5.858..
    printf("f2d+d: %d\n", (int)result_d);

    // double -> float demotion + arithmetic
    float demoted = (float)d;       // 2.71828 -> 2.71828f
    float result_f = demoted + f;   // 2.718 + 3.14 = 5.858..
    printf("d2f+f: %d\n", (int)result_f);

    // int -> float -> double chain
    float fi = (float)i;           // 100.0f
    double di = (double)fi;        // 100.0
    di = di + d;                   // 100.0 + 2.71828 = 102.71828
    printf("i2f2d: %d\n", (int)di);

    // int -> double -> float chain
    double di2 = (double)i;        // 100.0
    float fi2 = (float)di2;        // 100.0f
    fi2 = fi2 + f;                 // 100.0 + 3.14 = 103.14
    printf("i2d2f: %d\n", (int)fi2);

    return 0;
}
