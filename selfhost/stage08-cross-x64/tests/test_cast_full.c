/* test_cast_full.c — comprehensive type cast tests
 *
 * Tests sign-extend, zero-extend, narrowing, and widening casts.
 * Expected exit code: 0 (all pass)
 */

int main(int argc, char **argv) {
    int x;
    int r;
    int fails;
    long long ll;

    fails = 0;

    /* (char) narrowing + sign-extend: 0x1FF → 0xFF → -1 */
    x = 511;
    r = (char)x;
    if (r != -1) fails = fails + 1;

    /* (unsigned char) narrowing + zero-extend: -1 → 0xFF → 255 */
    x = -1;
    r = (unsigned char)x;
    if (r != 255) fails = fails + 2;

    /* (short) narrowing + sign-extend: 0x18000 → 0x8000 → -32768 */
    x = 98304;
    r = (short)x;
    if (r != -32768) fails = fails + 4;

    /* (unsigned short) narrowing + zero-extend: -1 → 0xFFFF → 65535 */
    x = -1;
    r = (unsigned short)x;
    if (r != 65535) fails = fails + 8;

    /* (char) positive value that fits: 120 stays 120 */
    x = 120;
    r = (char)x;
    if (r != 120) fails = fails + 16;

    /* (long long) widening from signed int */
    x = -42;
    ll = (long long)x;
    if (ll != -42) fails = fails + 32;

    /* (long long) widening from unsigned int */
    x = -1;
    ll = (unsigned long long)(unsigned int)x;
    /* (unsigned int)-1 = 0xFFFFFFFF = 4294967295, which fits in 64 bits */
    /* Check: should NOT be -1 (which would be 0xFFFFFFFFFFFFFFFF) */
    if (ll == -1) fails = fails + 64;

    /* (int) narrowing from long long — just takes low 32 bits */
    ll = 100;
    r = (int)ll;
    if (r != 100) fails = fails + 128;

    return fails;
}
