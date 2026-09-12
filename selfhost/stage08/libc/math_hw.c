/* Hardware square root for the stage08 libc.  __fp64_sqrt / __fp32_sqrt
 * are the libcall names the stage08 backend lowers to FSQRT.D / FSQRT.S
 * inline, so each wrapper is one instruction.  Built in phase 2 only:
 * the stage07 bootstrap compiler does not know these names.  regal's
 * pow (runtime/math_soft.c) reaches sqrt through asin. */
double __fp64_sqrt(double x);
float __fp32_sqrt(float x);
double sqrt(double x) { return __fp64_sqrt(x); }
float sqrtf(float x) { return __fp32_sqrt(x); }
