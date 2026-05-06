/* fpu_ops_d_a64.c — Self-compiled f64 portion of fpu_ops.
 *
 * Compiled by cc-a64 --hir.  Native AArch64 fadd_d / fsub_d / fmul_d /
 * fdiv_d / fcmp_d / scvtf-D / fcvtzs-D end up here.  Companion to the
 * f32 file fpu_ops_a64.c.
 *
 * Differs from libc_x64/fpu_ops.c (host-built x64 sibling) only in:
 *   - No #include <math.h> (stage07 has no math.h); sqrt declared extern.
 *   - f32 functions live in fpu_ops_a64.c to avoid duplicate symbols.
 *
 * Native f64 codegen is gated by S12CC_NATIVE_F64 in cc-a64.c — see
 * the lower's #ifdef branches in stage07/hir_lower.h. */

#include <string.h>
#include <stdint.h>

extern double sqrt(double x);

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

static float to_f32(unsigned int bits) {
    float f; memcpy(&f, &bits, 4); return f;
}

static unsigned int from_f32(float f) {
    unsigned int bits; memcpy(&bits, &f, 4); return bits;
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

void fpu_cvt_d_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi) {
    from_f64((double)to_f32(a), r_lo, r_hi);
}

unsigned int fpu_cvt_s_d(unsigned int a_lo, unsigned int a_hi) {
    return from_f32((float)to_f64(a_lo, a_hi));
}

/* int64 ↔ f32/f64 paths.  These exercise sf=1 (X-form) on
 * scvtf/fcvtzs/ucvtf/fcvtzu via the codegen's TY_LLONG type sniff. */
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
