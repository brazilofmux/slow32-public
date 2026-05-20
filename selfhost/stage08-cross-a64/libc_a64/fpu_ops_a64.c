/* fpu_ops_a64.c — Self-compiled f32 portion of fpu_ops.
 *
 * Compiled by cc-a64 --hir.  Native AArch64 FP arithmetic ends up here:
 * fadd/fsub/fmul/fdiv/fneg/fcmp/scvtf/fcvtzs etc.  This is the
 * Session 7 milestone deliverable for the float-codegen plan.
 *
 * Limitations:
 *   - f32 only.  The f64 functions (fpu_*_d, fpu_cvt_*_d/_l_*) live in
 *     ../stage08-cross-x64/libc_x64/fpu_ops.c, still host-gcc-built —
 *     blocked by the lower's __fp64_* libcall path which doesn't go
 *     native on a64 yet.
 *   - sqrtf goes via an extern decl (provided by libc_a64/math_stubs.c,
 *     host-gcc-built since stage07 has no math.h).  The encoders for
 *     fsqrt s/d exist in a64_encode.h; wiring them as a HI op would let
 *     us drop math_stubs entirely, follow-up.
 *
 * All public functions match the f32 signatures in fpu_ops.c. */

#include <string.h>

extern float sqrtf(float x);

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
