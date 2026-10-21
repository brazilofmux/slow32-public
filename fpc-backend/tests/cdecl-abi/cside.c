/* The C half of the cdecl ABI test (GitHub issue 98).  Each function
 * prints what it received; the Pascal half calls them with known values,
 * and calls c_callbacks, which calls Pascal cdecl functions from C and
 * prints what they return.  All output is from C, so ordering is simple. */
#include <stdio.h>
#include <stdarg.h>

void c_int64(int a, long long b) { printf("c_int64 %d %lld\n", a, b); }
void c_double(int a, double b) { printf("c_double %d %.4f\n", a, b); }
void c_hole(int a, double b, int c, int d) { printf("c_hole %d %.4f %d %d\n", a, b, c, d); }
void c_mix(int a, double b, long long c, float d, double e) {
    printf("c_mix %d %.4f %lld %.4f %.4f\n", a, b, c, (double)d, e);
}
void c_spill(int a, int b, int c, int d, int e, int f, int g, double h, int i) {
    printf("c_spill %d %d %d %d %d %d %d %.4f %d\n", a, b, c, d, e, f, g, h, i);
}
void c_straddle(int a, int b, int c, int d, int e, int f, int g, long long h) {
    printf("c_straddle %d %d %d %d %d %d %d %lld\n", a, b, c, d, e, f, g, h);
}
void c_holestraddle(int a, int b, int c, int d, int e, int f, int g, double h, long long i) {
    printf("c_holestraddle %d %d %d %d %d %d %d %.4f %lld\n", a, b, c, d, e, f, g, h, i);
}
void c_fixedvar(int a, double b, ...) {
    va_list ap;
    va_start(ap, b);
    printf("c_fixedvar %d %.4f %d %.4f %lld\n", a, b, va_arg(ap, int), va_arg(ap, double), va_arg(ap, long long));
    va_end(ap);
}
void c_printf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vprintf(fmt, ap);
    va_end(ap);
}

/* Pascal cdecl functions, called from here */
extern double p_mix(int a, double b, long long c, float d, double e);
extern long long p_int64(int a, long long b, int c);
extern double p_spill(int a, int b, int c, int d, int e, int f, int g, double h, int i);

void c_callbacks(void) {
    printf("p_mix %.4f\n", p_mix(1, 2.5, 3000000000LL, 4.25f, 5.125));
    printf("p_int64 %lld\n", p_int64(7, 0x0000000200000003LL, 11));
    printf("p_spill %.4f\n", p_spill(1, 2, 3, 4, 5, 6, 7, 8.5, 9));
}
