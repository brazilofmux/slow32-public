/* fpu_ops.c — Float/double operations for the cross-compiled emulator.
 *
 * Compiled by the HOST compiler (GCC/Clang), not cc-x64, because cc-x64
 * has no SSE support. The resulting .o links with our ld-x64 linker.
 *
 * All functions use unsigned int for f32 bit patterns and
 * unsigned int lo/hi pairs for f64 bit patterns, avoiding float/double
 * in the cross-compiled code's type system entirely.
 */

#include <string.h>
#include <math.h>
#include <stdint.h>

/* ============================================================
 * f32 helpers — argument/return is the IEEE 754 bit pattern
 * ============================================================ */

static float to_f32(unsigned int bits) {
    float f; memcpy(&f, &bits, 4); return f;
}

static unsigned int from_f32(float f) {
    unsigned int bits; memcpy(&bits, &f, 4); return bits;
}

unsigned int fpu_add_s(unsigned int a, unsigned int b) {
    return from_f32(to_f32(a) + to_f32(b));
}

unsigned int fpu_sub_s(unsigned int a, unsigned int b) {
    return from_f32(to_f32(a) - to_f32(b));
}

unsigned int fpu_mul_s(unsigned int a, unsigned int b) {
    return from_f32(to_f32(a) * to_f32(b));
}

unsigned int fpu_div_s(unsigned int a, unsigned int b) {
    return from_f32(to_f32(a) / to_f32(b));
}

unsigned int fpu_sqrt_s(unsigned int a) {
    return from_f32(sqrtf(to_f32(a)));
}

int fpu_eq_s(unsigned int a, unsigned int b) {
    return to_f32(a) == to_f32(b) ? 1 : 0;
}

int fpu_lt_s(unsigned int a, unsigned int b) {
    return to_f32(a) < to_f32(b) ? 1 : 0;
}

int fpu_le_s(unsigned int a, unsigned int b) {
    return to_f32(a) <= to_f32(b) ? 1 : 0;
}

unsigned int fpu_neg_s(unsigned int a) {
    return a ^ 0x80000000u;
}

unsigned int fpu_abs_s(unsigned int a) {
    return a & 0x7FFFFFFFu;
}

/* f32 ↔ int conversions */
int fpu_cvt_w_s(unsigned int a) {
    return (int)to_f32(a);
}

unsigned int fpu_cvt_wu_s(unsigned int a) {
    return (unsigned int)to_f32(a);
}

unsigned int fpu_cvt_s_w(int a) {
    return from_f32((float)a);
}

unsigned int fpu_cvt_s_wu(unsigned int a) {
    return from_f32((float)a);
}

/* ============================================================
 * f64 helpers — stored as lo/hi uint32 pair
 * Return via pointer args for the two halves.
 * ============================================================ */

static double to_f64(unsigned int lo, unsigned int hi) {
    double d;
    uint64_t bits = (uint64_t)lo | ((uint64_t)hi << 32);
    memcpy(&d, &bits, 8);
    return d;
}

static void from_f64(double d, unsigned int *lo, unsigned int *hi) {
    uint64_t bits;
    memcpy(&bits, &d, 8);
    *lo = (unsigned int)bits;
    *hi = (unsigned int)(bits >> 32);
}

void fpu_add_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int b_lo, unsigned int b_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    from_f64(to_f64(a_lo, a_hi) + to_f64(b_lo, b_hi), r_lo, r_hi);
}

void fpu_sub_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int b_lo, unsigned int b_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    from_f64(to_f64(a_lo, a_hi) - to_f64(b_lo, b_hi), r_lo, r_hi);
}

void fpu_mul_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int b_lo, unsigned int b_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    from_f64(to_f64(a_lo, a_hi) * to_f64(b_lo, b_hi), r_lo, r_hi);
}

void fpu_div_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int b_lo, unsigned int b_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    from_f64(to_f64(a_lo, a_hi) / to_f64(b_lo, b_hi), r_lo, r_hi);
}

void fpu_sqrt_d(unsigned int a_lo, unsigned int a_hi,
                unsigned int *r_lo, unsigned int *r_hi) {
    from_f64(sqrt(to_f64(a_lo, a_hi)), r_lo, r_hi);
}

int fpu_eq_d(unsigned int a_lo, unsigned int a_hi,
             unsigned int b_lo, unsigned int b_hi) {
    return to_f64(a_lo, a_hi) == to_f64(b_lo, b_hi) ? 1 : 0;
}

int fpu_lt_d(unsigned int a_lo, unsigned int a_hi,
             unsigned int b_lo, unsigned int b_hi) {
    return to_f64(a_lo, a_hi) < to_f64(b_lo, b_hi) ? 1 : 0;
}

int fpu_le_d(unsigned int a_lo, unsigned int a_hi,
             unsigned int b_lo, unsigned int b_hi) {
    return to_f64(a_lo, a_hi) <= to_f64(b_lo, b_hi) ? 1 : 0;
}

void fpu_neg_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    *r_lo = a_lo;
    *r_hi = a_hi ^ 0x80000000u;
}

void fpu_abs_d(unsigned int a_lo, unsigned int a_hi,
               unsigned int *r_lo, unsigned int *r_hi) {
    *r_lo = a_lo;
    *r_hi = a_hi & 0x7FFFFFFFu;
}

/* f64 ↔ int32 conversions */
int fpu_cvt_w_d(unsigned int a_lo, unsigned int a_hi) {
    return (int)to_f64(a_lo, a_hi);
}

unsigned int fpu_cvt_wu_d(unsigned int a_lo, unsigned int a_hi) {
    return (unsigned int)to_f64(a_lo, a_hi);
}

void fpu_cvt_d_w(int a, unsigned int *r_lo, unsigned int *r_hi) {
    from_f64((double)a, r_lo, r_hi);
}

void fpu_cvt_d_wu(unsigned int a, unsigned int *r_lo, unsigned int *r_hi) {
    from_f64((double)a, r_lo, r_hi);
}

/* f32 ↔ f64 conversions */
void fpu_cvt_d_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi) {
    from_f64((double)to_f32(a), r_lo, r_hi);
}

unsigned int fpu_cvt_s_d(unsigned int a_lo, unsigned int a_hi) {
    return from_f32((float)to_f64(a_lo, a_hi));
}

/* f32/f64 ↔ int64 conversions */
void fpu_cvt_l_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi) {
    int64_t v = (int64_t)to_f32(a);
    *r_lo = (unsigned int)v;
    *r_hi = (unsigned int)((uint64_t)v >> 32);
}

void fpu_cvt_lu_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi) {
    uint64_t v = (uint64_t)to_f32(a);
    *r_lo = (unsigned int)v;
    *r_hi = (unsigned int)(v >> 32);
}

unsigned int fpu_cvt_s_l(unsigned int a_lo, unsigned int a_hi) {
    int64_t v = (int64_t)((uint64_t)a_lo | ((uint64_t)a_hi << 32));
    return from_f32((float)v);
}

unsigned int fpu_cvt_s_lu(unsigned int a_lo, unsigned int a_hi) {
    uint64_t v = (uint64_t)a_lo | ((uint64_t)a_hi << 32);
    return from_f32((float)v);
}

void fpu_cvt_l_d(unsigned int a_lo, unsigned int a_hi,
                 unsigned int *r_lo, unsigned int *r_hi) {
    int64_t v = (int64_t)to_f64(a_lo, a_hi);
    *r_lo = (unsigned int)v;
    *r_hi = (unsigned int)((uint64_t)v >> 32);
}

void fpu_cvt_lu_d(unsigned int a_lo, unsigned int a_hi,
                  unsigned int *r_lo, unsigned int *r_hi) {
    uint64_t v = (uint64_t)to_f64(a_lo, a_hi);
    *r_lo = (unsigned int)v;
    *r_hi = (unsigned int)(v >> 32);
}

void fpu_cvt_d_l(unsigned int a_lo, unsigned int a_hi,
                 unsigned int *r_lo, unsigned int *r_hi) {
    int64_t v = (int64_t)((uint64_t)a_lo | ((uint64_t)a_hi << 32));
    from_f64((double)v, r_lo, r_hi);
}

void fpu_cvt_d_lu(unsigned int a_lo, unsigned int a_hi,
                  unsigned int *r_lo, unsigned int *r_hi) {
    uint64_t v = (uint64_t)a_lo | ((uint64_t)a_hi << 32);
    from_f64((double)v, r_lo, r_hi);
}
