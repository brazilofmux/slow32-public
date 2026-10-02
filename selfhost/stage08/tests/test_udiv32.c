/* selfhost ISSUES-77: 32-bit unsigned division and remainder.
 *
 * SLOW-32's div and rem are signed.  This compiler used them for
 * unsigned operands too, so any value of 2^31 or more divided as a
 * negative number: 4000000000u / 10 was -29496729, and a decimal
 * conversion written the obvious way -- v % 10, v / 10 -- printed
 * 4000000000 as "UNSNQPUNQ" (characters below '0').  Unsigned operands
 * go through __udivsi3 / __umodsi3 now, as they do in stage08.
 *
 * Returns 0, or the number of the first check that fails. */

unsigned int gdiv(unsigned int a, unsigned int b) { return a / b; }
unsigned int grem(unsigned int a, unsigned int b) { return a % b; }
int sdiv(int a, int b) { return a / b; }
int srem(int a, int b) { return a % b; }

int digits(unsigned int v, char *out) {
    char tmp[12];
    int n;
    int i;
    n = 0;
    if (v == 0) { tmp[0] = '0'; n = 1; }
    while (v > 0) {
        tmp[n] = '0' + (int)(v % 10);
        v = v / 10;
        n = n + 1;
    }
    i = 0;
    while (i < n) {
        out[i] = tmp[n - 1 - i];
        i = i + 1;
    }
    out[n] = 0;
    return n;
}

int same(char *a, char *b) {
    while (*a && *a == *b) { a = a + 1; b = b + 1; }
    return *a == *b;
}

int main(void) {
    unsigned int u;
    unsigned int d;
    unsigned short us;
    unsigned char uc;
    char buf[16];

    if (gdiv(4000000000u, 10) != 400000000u) return 1;
    if (grem(4000000000u, 10) != 0) return 2;
    if (gdiv(4294967295u, 2) != 2147483647u) return 3;
    if (grem(4294967295u, 2) != 1) return 4;
    if (gdiv(4294967295u, 4294967295u) != 1) return 5;
    if (gdiv(2147483648u, 3) != 715827882u) return 6;
    if (grem(2147483648u, 3) != 2) return 7;
    if (gdiv(5, 4000000000u) != 0) return 8;            /* a divisor above 2^31 */
    if (grem(5, 4000000000u) != 5) return 9;
    if (gdiv(4000000001u, 4000000000u) != 1) return 10;
    if (grem(4000000001u, 4000000000u) != 1) return 11;
    if (gdiv(100, 7) != 14 || grem(100, 7) != 2) return 12;   /* and the ordinary ones still */

    /* signed stays signed */
    if (sdiv(-7, 2) != -3 || srem(-7, 2) != -1) return 13;
    if (sdiv(-2147483647 - 1, 10) != -214748364) return 14;

    /* compound assignment */
    u = 3000000000u;
    u /= 7;
    if (u != 428571428u) return 15;
    u = 3000000000u;
    u %= 7;
    if (u != 4) return 16;
    u = 4294967290u;
    d = 4294967280u;
    u %= d;
    if (u != 10) return 17;

    /* narrower unsigned types promote to int: signed division, and right */
    us = 65535;
    if (us / 2 != 32767 || us % 7 != 1) return 18;
    uc = 255;
    if (uc / 16 != 15 || uc % 16 != 15) return 19;

    /* what it was found by: a number written out a digit at a time */
    if (digits(4000000000u, buf) != 10 || !same(buf, "4000000000")) return 20;
    if (digits(4294967295u, buf) != 10 || !same(buf, "4294967295")) return 21;
    if (digits(2147483648u, buf) != 10 || !same(buf, "2147483648")) return 22;
    if (digits(0, buf) != 1 || !same(buf, "0")) return 23;
    return 0;
}
