/* Float arithmetic functions exercised by tests/cc_fp_arith_runner.c.
 * The runner is host-gcc-built and uses the AArch64 AAPCS64 V-reg
 * float-arg convention to call into these.  Each function tests one
 * HIR FP op (HI_FADD / FSUB / FMUL / FDIV / FNEG / HI_ICONST+TY_FLOAT). */

float t_fadd(float a, float b) { return a + b; }
float t_fsub(float a, float b) { return a - b; }
float t_fmul(float a, float b) { return a * b; }
float t_fdiv(float a, float b) { return a / b; }
float t_fneg(float a)          { return -a; }
float t_fcalc(float a, float b, float c) { return (a + b) * c - a; }

/* Float constants — exercises HI_ICONST(TY_FLOAT) remat.  Mix of
 * fmov-imm-encodable (1.0, 2.0, 0.5, -1.0, 8.0 ...) and unencodable
 * (0.1, 3.14, 0.123456) values to cover both codegen paths. */
float t_six(void)                  { return 2.0f * 3.0f; }
float t_add_one(float x)           { return x + 1.0f; }
float t_halfneg(float x)           { return -0.5f * x; }
float t_unencodable(float x)       { return x + 0.1f; }
float t_pi_times(float x)          { return x * 3.14f; }
float t_combo(float x)             { return x * 2.0f + 1.0f; }

/* Float comparisons — exercise FCMP+CSET (HI_FEQ/FLT/FLE) and
 * FCMP+B.cond (BRC fusion via `if (a OP b)`). */
int t_feq_eq(float a, float b)     { return a == b; }
int t_flt_eq(float a, float b)     { return a < b; }
int t_fle_eq(float a, float b)     { return a <= b; }
int t_fgt_eq(float a, float b)     { return a > b; }
int t_fge_eq(float a, float b)     { return a >= b; }

/* Branch-fusion variants. */
int t_branch_lt(float a, float b)  { if (a < b)  return 1; return 0; }
int t_branch_eq(float a, float b)  { if (a == b) return 42; return 7; }
int t_branch_le(float a, float b)  { if (a <= b) return 99; return 0; }

/* Float ↔ int conversions — exercises HI_FCVT_ItoF / FtoI. */
float t_i2f(int x)              { return (float)x; }
float t_u2f(unsigned int x)     { return (float)x; }
int   t_f2i(float x)            { return (int)x; }
unsigned int t_f2u(float x)     { return (unsigned int)x; }
float t_round_via_int(float x)  { return (float)((int)x); }   /* ItoF ∘ FtoI */
