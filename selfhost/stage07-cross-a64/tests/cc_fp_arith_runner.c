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

int main(void) {
    int code = 0;
    if (t_fadd(3.0f, 4.0f)         != 7.0f)  code |= 1;
    if (t_fsub(10.0f, 3.0f)        != 7.0f)  code |= 2;
    if (t_fmul(2.5f, 4.0f)         != 10.0f) code |= 4;
    if (t_fdiv(20.0f, 4.0f)        != 5.0f)  code |= 8;
    if (t_fneg(7.0f)               != -7.0f) code |= 16;
    if (t_fcalc(2.0f, 3.0f, 4.0f)  != 18.0f) code |= 32;  /* (2+3)*4 - 2 = 18 */
    if (t_fadd(-1.5f, 2.5f)        != 1.0f)  code |= 64;
    return code;
}
