/* posix_math.c -- fabs.  Own translation unit so a program that only
 * calls getenv (posix_proc.c) does not link the math intercept
 * (GitHub issue 65).  Plain C: stage07 compiles this libc too. */

double fabs(double x) {
    if (x < 0) return -x;
    if (x == 0) return 0.0;    /* -0.0 comes back positive */
    return x;
}
