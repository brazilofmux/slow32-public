/* Float arithmetic functions exercised by tests/cc_fp_arith_runner.c.
 * The runner is host-gcc-built and uses the AArch64 AAPCS64 V-reg
 * float-arg convention to call into these.  Each function tests one
 * HIR FP op (HI_FADD / FSUB / FMUL / FDIV / FNEG). */

float t_fadd(float a, float b) { return a + b; }
float t_fsub(float a, float b) { return a - b; }
float t_fmul(float a, float b) { return a * b; }
float t_fdiv(float a, float b) { return a / b; }
float t_fneg(float a)          { return -a; }
float t_fcalc(float a, float b, float c) { return (a + b) * c - a; }
