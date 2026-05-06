/* Host-gcc-built runner for cc_fp_arith.c.
 *
 * cc-a64 --hir doesn't yet support float literals (HI_FCONST is Session 3
 * of the float-codegen plan), so the test driver lives in a separate
 * compilation unit built with host gcc — gcc materialises 3.0f / 4.0f /
 * etc., calls into cc-a64-built `t_*` via the standard AAPCS64 V-arg ABI,
 * and checks the float result. */

extern float t_fadd(float, float);
extern float t_fsub(float, float);
extern float t_fmul(float, float);
extern float t_fdiv(float, float);
extern float t_fneg(float);
extern float t_fcalc(float, float, float);
extern float t_six(void);
extern float t_add_one(float);
extern float t_halfneg(float);
extern float t_unencodable(float);
extern float t_pi_times(float);
extern float t_combo(float);
extern int t_feq_eq(float, float);
extern int t_flt_eq(float, float);
extern int t_fle_eq(float, float);
extern int t_fgt_eq(float, float);
extern int t_fge_eq(float, float);
extern int t_branch_lt(float, float);
extern int t_branch_eq(float, float);
extern int t_branch_le(float, float);
extern float t_i2f(int);
extern float t_u2f(unsigned int);
extern int   t_f2i(float);
extern unsigned int t_f2u(float);
extern float t_round_via_int(float);
extern float t_call_simple(float);
extern float t_call_swap(float, float);
extern float t_call_pass_through(float, float, float, float, float, float);
extern float t_call_mixed(int, float, int, float);
extern float t_recurse(float, int);
extern float t_local_load(float);
extern float t_array_load(float, float, float, float);

/* Session 7 helper: do nothing visible; just ensure the called function
 * spills the FP local to a stack slot (via taking its address). */
void h_take_addr(float *p) {
    (void)p;
}

/* Helpers called by t_call_* — provided by host gcc, so they use the
 * AArch64 AAPCS64 calling convention natively (V-args in V0..V7).  If
 * cc-a64's CALL-side marshalling is correct, these get the expected
 * float values. */
float h_one(float x)                  { return x * 10.0f; }
float h_two(float a, float b)         { return a - b; }
float h_six(float a, float b, float c,
            float d, float e, float f) { return a + b + c + d + e + f; }
float h_mixed(int n, float a, int m, float b) {
    return (float)(n * m) + a + b;
}

/* Compare floats by bit pattern (exact equality).  Avoids surprises
 * from compiler folding `==` against literal in different ways. */
static int feq(float a, float b) { return a == b ? 1 : 0; }

int main(void) {
    int code = 0;
    /* Session 2 ops */
    if (!feq(t_fadd(3.0f, 4.0f),         7.0f))  code |= 1;
    if (!feq(t_fsub(10.0f, 3.0f),        7.0f))  code |= 2;
    if (!feq(t_fmul(2.5f, 4.0f),         10.0f)) code |= 4;
    if (!feq(t_fdiv(20.0f, 4.0f),        5.0f))  code |= 8;
    if (!feq(t_fneg(7.0f),               -7.0f)) code |= 16;
    if (!feq(t_fcalc(2.0f, 3.0f, 4.0f),  18.0f)) code |= 32;
    if (!feq(t_fadd(-1.5f, 2.5f),        1.0f))  code |= 64;
    /* Session 3 (FCONST) ops */
    if (!feq(t_six(),                    6.0f))  code |= 128;
    if (!feq(t_add_one(5.0f),            6.0f))  code |= 256;
    if (!feq(t_halfneg(8.0f),            -4.0f)) code |= 512;
    if (!feq(t_unencodable(0.9f),        1.0f))  code |= 1024;
    if (!feq(t_pi_times(2.0f),           6.28f)) code |= 2048;
    if (!feq(t_combo(3.0f),              7.0f))  code |= 4096;
    /* Session 4 (FCMP) ops — straight CSET path */
    if (t_feq_eq(1.0f, 1.0f)        != 1)        code |= 8192;
    if (t_feq_eq(1.0f, 2.0f)        != 0)        code |= 8192;
    if (t_flt_eq(1.0f, 2.0f)        != 1)        code |= 16384;
    if (t_flt_eq(2.0f, 1.0f)        != 0)        code |= 16384;
    if (t_flt_eq(1.0f, 1.0f)        != 0)        code |= 16384;
    if (t_fle_eq(1.0f, 2.0f)        != 1)        code |= 32768;
    if (t_fle_eq(1.0f, 1.0f)        != 1)        code |= 32768;
    if (t_fle_eq(2.0f, 1.0f)        != 0)        code |= 32768;
    if (t_fgt_eq(2.0f, 1.0f)        != 1)        code |= 65536;
    if (t_fgt_eq(1.0f, 2.0f)        != 0)        code |= 65536;
    if (t_fge_eq(2.0f, 1.0f)        != 1)        code |= 131072;
    if (t_fge_eq(1.0f, 1.0f)        != 1)        code |= 131072;
    if (t_fge_eq(1.0f, 2.0f)        != 0)        code |= 131072;
    /* Session 4 — fused BRC */
    if (t_branch_lt(1.0f, 2.0f)     != 1)        code |= 262144;
    if (t_branch_lt(2.0f, 1.0f)     != 0)        code |= 262144;
    if (t_branch_eq(1.5f, 1.5f)     != 42)       code |= 524288;
    if (t_branch_eq(1.5f, 2.5f)     != 7)        code |= 524288;
    if (t_branch_le(1.0f, 2.0f)     != 99)       code |= 1048576;
    if (t_branch_le(2.0f, 1.0f)     != 0)        code |= 1048576;
    /* Session 5 — int ↔ float conversions */
    if (!feq(t_i2f(7),                7.0f))     code |= 2097152;
    if (!feq(t_i2f(-3),              -3.0f))     code |= 2097152;
    if (!feq(t_u2f(0xFFFFFFFFu),     4294967296.0f))  code |= 4194304;  /* 2^32 */
    if (t_f2i(7.5f)                  != 7)       code |= 8388608;       /* truncate */
    if (t_f2i(-7.5f)                 != -7)      code |= 8388608;
    if (t_f2u(7.5f)                  != 7u)      code |= 16777216;
    if (t_f2u(2147483904.0f)         != 2147483904u) code |= 16777216;  /* fits unsigned, not signed */
    if (!feq(t_round_via_int(3.9f),  3.0f))      code |= 33554432;
    if (!feq(t_round_via_int(-3.9f), -3.0f))     code |= 33554432;
    /* Session 6 — CALL-side FP arg marshalling */
    if (!feq(t_call_simple(2.0f),    30.0f))     code |= 67108864;       /* (2+1)*10 */
    if (!feq(t_call_swap(3.0f, 7.0f), 4.0f))     code |= 134217728;      /* h_two(b,a)=b-a=4 */
    if (!feq(t_call_pass_through(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f), 21.0f)) code |= 268435456;
    if (!feq(t_call_mixed(2, 3.0f, 5, 4.0f), 17.0f)) code |= 536870912;  /* 2*5 + 3 + 4 */
    if (!feq(t_recurse(1.0f, 5),     6.0f))      code |= 1073741824;
    /* Session 7 — V-class LOAD/STORE.  Skipping array_load for now
     * since arrays of floats use HI_ADDI(alloca, k) which the
     * V-class path handles via offset-disp fold (Pattern A). */
    if (!feq(t_local_load(2.0f),     6.0f))      code |= 0x80000000;  /* (2+1)*2=6, after roundtrip */
    if (!feq(t_array_load(1.0f, 2.0f, 3.0f, 4.0f), 10.0f)) code |= 0x80000000;
    return code;
}
