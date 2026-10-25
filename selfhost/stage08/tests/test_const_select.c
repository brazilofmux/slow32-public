/* c ? K1 : K2 with both arms integer constants is a mask, not a
 * branch.  Conditions of every shape -- a comparison, a bare int, a
 * byte, an unsigned, a pointer (which keeps the diamond) -- arms that
 * are zero, negative or too wide for an immediate, a result narrowed
 * on assignment, a nested select, and a condition with a side effect
 * evaluated exactly once.  run-tests.sh checks that dbl has no
 * branch. */
static int calls;

unsigned char dbl(unsigned char x) {
    return (unsigned char)((x << 1) ^ ((x & 0x80) ? 0x1b : 0));
}

int pick(int c) { return c ? -7 : 12; }
int wide(int c) { return c > 3 ? 0x12345678 : -0x7654321; }
int zero_then(unsigned u) { return u ? 0 : 99; }
int bytec(signed char b) { return b ? 5 : 6; }
int ptrc(int *p) { return p ? 40 : 41; }
int nest(int a, int b) { return a < b ? (a == 0 ? 1 : 2) : (b == 0 ? 3 : 4); }
unsigned char narrow(int c) { unsigned char v; v = c ? 300 : 2; return v; }
short shorty(int c) { return c ? 40000 : -3; }
int tick(int v) { calls = calls + 1; return v; }
int once(int v) { return tick(v) ? 8 : 9; }

int main(void) {
    int z;
    z = 0;
    if (dbl(0x80) != 0x1b || dbl(0x81) != 0x19 || dbl(0x40) != 0x80 || dbl(0) != 0) return 1;
    if (dbl(0xff) != 0xe5) return 2;
    if (pick(0) != 12 || pick(5) != -7 || pick(-1) != -7) return 3;
    if (wide(4) != 0x12345678 || wide(3) != -0x7654321) return 4;
    if (zero_then(0) != 99 || zero_then(0x80000000u) != 0) return 5;
    if (bytec(0) != 6 || bytec(-128) != 5) return 6;
    if (ptrc(&z) != 40 || ptrc((int *)0) != 41) return 7;
    if (nest(0, 1) != 1 || nest(2, 3) != 2 || nest(5, 0) != 3 || nest(5, 2) != 4) return 8;
    if (narrow(1) != 44 || narrow(0) != 2) return 9;
    if (shorty(1) != -25536 || shorty(0) != -3) return 10;
    calls = 0;
    if (once(0) != 9 || once(3) != 8 || calls != 2) return 11;
    return 0;
}
