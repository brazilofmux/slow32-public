/* GitHub #13: a value narrower than the function's return type must be
 * widened on the way out -- a 32-bit unsigned returned as unsigned long
 * long left the pair's high register stale.  Every return in these
 * functions is 32-bit (the trigger); one goes through a call. */
typedef unsigned long long u64;
typedef long long i64;
static u64 ret_u32(unsigned x, unsigned d) { unsigned q = x / d; return q; }
static i64 ret_i32(int x) { int y = x - 1; return y; }
static u64 ret_u32_two(unsigned x) { if (x > 5) { unsigned a = x + 1; return a; } { unsigned b = x * 3; return b; } }
static i64 ret_c(char c) { return c; }
static u64 ret_call(unsigned x, unsigned d) { return x / d; }
static double ret_int_as_double(int x) { return x; }
int main(void) {
    volatile unsigned x = 10000, d = 4; volatile int m = -6; volatile char c = -3;
    u64 a = ret_u32(x, d); i64 b = ret_i32(m); u64 e = ret_u32_two(x); i64 f = ret_c(c); u64 g = ret_call(x, d);
    double h = ret_int_as_double(m);
    int ok = 1;
    if ((int)(a >> 32) != 0 || (unsigned)a != 2500) ok = 0;
    if ((int)(b >> 32) != -1 || (int)b != -7) ok = 0;
    if ((int)(e >> 32) != 0 || (unsigned)e != 10001) ok = 0;
    if ((int)(f >> 32) != -1 || (int)f != -3) ok = 0;
    if ((int)(g >> 32) != 0 || (unsigned)g != 2500) ok = 0;
    if (h != -6.0) ok = 0;
    return ok ? 0 : 1;              /* the runner judges the exit code */
}
