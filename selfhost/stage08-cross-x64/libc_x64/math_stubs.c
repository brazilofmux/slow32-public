/* math_stubs.c — minimal libm replacements (sqrt/sqrtf) used by fpu_ops.c.
 *
 * Built with host gcc.  __builtin_sqrt[f] lowers directly to the SSE2
 * SQRTSD/SQRTSS instructions on x86-64, so this avoids dragging in libm
 * for outputs linked with -nostdlib. */

double sqrt(double x)  { return __builtin_sqrt(x); }
float  sqrtf(float x)  { return __builtin_sqrtf(x); }
