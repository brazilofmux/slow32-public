#include <stdio.h>
#include <stdint.h>
#include <string.h>

// Declare built-in copysign functions if not available in headers
double copysign(double x, double y);
float copysignf(float x, float y);

void check_d(const char* name, double got, double expected) {
    // Exact equality check is fine here since we're just moving bits
    if (got == expected) {
        // Also check if the sign bit matches, since +0.0 == -0.0
        uint64_t got_bits = 0;
        uint64_t exp_bits = 0;
        memcpy(&got_bits, &got, sizeof(got_bits));
        memcpy(&exp_bits, &expected, sizeof(exp_bits));

        // Mask for sign bit of double (bit 63)
        uint64_t sign_got = (got_bits >> 63);
        uint64_t sign_exp = (exp_bits >> 63);
        
        if (sign_got == sign_exp) {
            printf("%s: PASS\n", name);
        } else {
            printf("%s: FAIL (wrong sign bit)\n", name);
        }
    } else {
        printf("%s: FAIL (value mismatch)\n", name);
    }
}

void check_f(const char* name, float got, float expected) {
    if (got == expected) {
        uint32_t got_bits = 0;
        uint32_t exp_bits = 0;
        memcpy(&got_bits, &got, sizeof(got_bits));
        memcpy(&exp_bits, &expected, sizeof(exp_bits));

        uint32_t sign_got = (got_bits >> 31);
        uint32_t sign_exp = (exp_bits >> 31);
        
        if (sign_got == sign_exp) {
            printf("%s: PASS\n", name);
        } else {
            printf("%s: FAIL (wrong sign bit)\n", name);
        }
    } else {
        printf("%s: FAIL (value mismatch)\n", name);
    }
}

int main() {
    volatile double d_pos = 10.0;
    volatile double d_neg = -20.0;
    volatile float f_pos = 5.0f;
    volatile float f_neg = -15.0f;

    // 1. copysign(f64, f64)
    check_d("copysign(10.0, 10.0)", copysign(d_pos, d_pos), 10.0);
    check_d("copysign(10.0, -20.0)", copysign(d_pos, d_neg), -10.0);
    check_d("copysign(-20.0, 10.0)", copysign(d_neg, d_pos), 20.0);
    check_d("copysign(-20.0, -20.0)", copysign(d_neg, d_neg), -20.0);

    // 2. copysignf(f32, f32)
    check_f("copysignf(5.0f, 5.0f)", copysignf(f_pos, f_pos), 5.0f);
    check_f("copysignf(5.0f, -15.0f)", copysignf(f_pos, f_neg), -5.0f);
    check_f("copysignf(-15.0f, 5.0f)", copysignf(f_neg, f_pos), 15.0f);
    check_f("copysignf(-15.0f, -15.0f)", copysignf(f_neg, f_neg), -15.0f);

    // 3. Mixed types
    check_d("copysign(10.0, -15.0f)", copysign(d_pos, f_neg), -10.0);
    
    return 0;
}
