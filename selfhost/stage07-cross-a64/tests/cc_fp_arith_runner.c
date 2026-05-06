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
    return code;
}
