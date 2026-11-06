// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/* Adapted from compiler-rt's lib/builtins/powisf2.c and powidf2.c, part of the LLVM
 * Project, under its licence (https://llvm.org/LICENSE.txt; see NOTICE).
 *
 * What llvm.powi becomes, x**n for an int n: the names and arguments compiler-rt and
 * libgcc use.  Multiplication, not pow(): a float can't hold an odd exponent past 2^24,
 * and x**n of a real is a product in every Fortran.  Square and multiply, low bit first,
 * dividing at the end for a negative n, as compiler-rt does, so programs agree with it
 * to the bit. */
float __powisf2(float x, int n) {
    int negative = n < 0;
    float r = 1;
    for (;;) {
        if (n & 1)
            r *= x;
        n /= 2;
        if (n == 0)
            break;
        x *= x;
    }
    return negative ? 1 / r : r;
}

double __powidf2(double x, int n) {
    int negative = n < 0;
    double r = 1;
    for (;;) {
        if (n & 1)
            r *= x;
        n /= 2;
        if (n == 0)
            break;
        x *= x;
    }
    return negative ? 1 / r : r;
}
