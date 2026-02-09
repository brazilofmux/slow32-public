/*
 * SLOW-32 Math Hardware Helpers
 *
 * Small wrapper functions that should lower to native SLOW-32
 * floating-point instructions. These are compiled with -fno-builtin
 * -fno-math-errno to avoid recursive lowering and math-errno fences.
 */

#include <math.h>

double fabs(double x) {
    return __builtin_fabs(x);  /* emits FABS.D */
}

double sqrt(double x) {
    return __builtin_sqrt(x);  /* emits FSQRT.D */
}

float fabsf(float x) {
    return __builtin_fabsf(x);  /* emits FABS.S */
}

float sqrtf(float x) {
    return __builtin_sqrtf(x);  /* emits FSQRT.S */
}
