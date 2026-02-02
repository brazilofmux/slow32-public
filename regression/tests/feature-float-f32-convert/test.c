// Test f32 conversions: fcvt.w.s, fcvt.s.w
// Uses volatile to force runtime conversion instructions
#include <stdio.h>

volatile int vi_pos = 42;
volatile int vi_neg = -17;
volatile int vi_zero = 0;
volatile int vi_large = 100000;

volatile float vf_pos = 7.9f;
volatile float vf_neg = -3.7f;
volatile float vf_zero = 0.0f;
volatile float vf_half = 0.5f;

int main() {
    // int -> float -> int roundtrip
    int ip = vi_pos;
    float fp = (float)ip;
    int back = (int)fp;
    printf("i2f2i 42: %d\n", back);

    int in = vi_neg;
    float fn = (float)in;
    int backn = (int)fn;
    printf("i2f2i -17: %d\n", backn);

    int iz = vi_zero;
    float fz = (float)iz;
    int backz = (int)fz;
    printf("i2f2i 0: %d\n", backz);

    // Large int roundtrip
    int il = vi_large;
    float fl = (float)il;
    int backl = (int)fl;
    printf("i2f2i 100000: %d\n", backl);

    // float -> int truncation (toward zero)
    float fpos = vf_pos;
    printf("f2i 7.9: %d\n", (int)fpos);     // 7 (truncate)

    float fneg = vf_neg;
    printf("f2i -3.7: %d\n", (int)fneg);    // -3 (truncate toward zero)

    float fzero = vf_zero;
    printf("f2i 0.0: %d\n", (int)fzero);    // 0

    float fhalf = vf_half;
    printf("f2i 0.5: %d\n", (int)fhalf);    // 0 (truncate)

    // Arithmetic after conversion with constant pool literal
    int x = vi_pos;                           // 42
    float fx = (float)x + 8.0f;              // 50.0 (8.0f from constant pool)
    printf("42+8.0: %d\n", (int)fx);

    return 0;
}
