/* math_stubs.c — minimal libm replacements (sqrt/sqrtf) used by fpu_ops.c.
 *
 * Built with host gcc (which targets AArch64 directly on this host), so
 * __builtin_sqrt[f] expands to the hardware FSQRT instruction without
 * needing libm.  Only sqrt/sqrtf are referenced by libc_x64/fpu_ops.c. */

double sqrt(double x)  { return __builtin_sqrt(x); }
float  sqrtf(float x)  { return __builtin_sqrtf(x); }
