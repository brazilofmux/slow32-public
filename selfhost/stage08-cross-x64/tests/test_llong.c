/* test_llong.c — 64-bit long long arithmetic, bitwise, shift, compare tests
 *
 * All values built via arithmetic to avoid 64-bit literal parsing limits.
 * Expected exit code: 0 (all pass)
 */

int main(int argc, char **argv) {
    long long a;
    long long b;
    long long c;
    int fails;
    int count;

    fails = 0;

    /* Basic add/sub */
    a = 100;
    b = 200;
    c = a + b;
    if (c != 300) fails = fails + 1;
    c = b - a;
    if (c != 100) fails = fails + 2;

    /* Multiply exceeding 32 bits: 1e6 * 1e6 = 1e12 */
    a = 1000000;
    b = 1000000;
    c = a * b;
    if ((int)(c >> 32) != 232) fails = fails + 4;

    /* Division: 1e12 / 1e6 = 1e6 */
    c = a * b;
    c = c / b;
    if (c != 1000000) fails = fails + 8;

    /* Modulus: (1e12 + 7) % 1e6 = 7 */
    c = a * b + 7;
    c = c % b;
    if (c != 7) fails = fails + 16;

    /* Bitwise AND */
    a = 0xFF00FF00;
    b = 0x0F0F0F0F;
    c = a & b;
    if ((int)c != 0x0F000F00) fails = fails + 32;

    /* Left shift into upper 32 bits */
    a = 1;
    c = a << 40;
    if ((int)(c >> 32) != 256) fails = fails + 64;

    /* Right shift */
    c = c >> 40;
    if (c != 1) fails = fails + 128;

    /* Negation */
    a = 42;
    b = -a;
    if (b != -42) fails = fails + 256;

    /* Bitwise NOT */
    a = 0;
    b = ~a;
    if (b != -1) fails = fails + 512;

    /* Comparison: large positive */
    a = 1000000;
    b = a * a;
    if (b <= 0) fails = fails + 1024;

    /* XOR / OR */
    a = 0xFF;
    b = 0xF0;
    c = a ^ b;
    if (c != 0x0F) fails = fails + 2048;
    c = a | b;
    if (c != 0xFF) fails = fails + 4096;

    /* Compound assignment operators */
    a = 1000000;
    a *= 1000000;
    if ((int)(a >> 32) != 232) fails = fails + 8192;

    a = 100;
    a += 200;
    if (a != 300) fails = fails + 16384;

    a = 1;
    a <<= 40;
    if ((int)(a >> 32) != 256) fails = fails + 32768;

    /* Condition: if (long_long_var) */
    a = 1000000;
    b = a * a;
    if (b) { /* good */ } else { fails = fails + 65536; }

    /* While loop with long long counter */
    a = 5;
    count = 0;
    while (a) { count = count + 1; a = a - 1; }
    if (count != 5) fails = fails + 131072;

    /* Return low byte only (exit code is 8-bit) */
    return fails;
}
