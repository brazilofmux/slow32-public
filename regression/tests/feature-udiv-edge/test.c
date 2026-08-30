/* Unsigned 32-bit divide and modulo at the edges the hardware signed
 * divider cannot take directly: operands with bit 31 set, in every
 * combination, plus the ordinary case and INT_MIN / -1. */
void debug_char(char c);

static void put(const char *s) { while (*s) debug_char(*s++); }

static volatile unsigned int U[] = {
    0xFFFFFFFFu, 0x80000000u, 0x80000001u, 0xFFFFFFFEu, 0x12345678u, 7u, 1u, 0u,
    0x7FFFFFFFu, 0xC0000000u, 0x80000000u, 3u
};

int main(void) {
    int fails = 0;
    unsigned int a, b;
    /* both large */
    a = U[0]; b = U[1]; if (a / b != 1u || a % b != 0x7FFFFFFFu) fails |= 1;
    a = U[1]; b = U[0]; if (a / b != 0u || a % b != 0x80000000u) fails |= 2;
    a = U[3]; b = U[2]; if (a / b != 1u || a % b != 0x7FFFFFFDu) fails |= 4;
    /* large dividend, small divisor */
    a = U[0]; b = U[5]; if (a / b != 613566756u || a % b != 3u) fails |= 8;
    a = U[1]; b = U[6]; if (a / b != 0x80000000u || a % b != 0u) fails |= 16;
    a = U[1]; b = U[11]; if (a / b != 715827882u || a % b != 2u) fails |= 32;
    a = U[9]; b = U[8]; if (a / b != 1u || a % b != 0x40000001u) fails |= 64;
    a = U[2]; b = U[4]; if (a / b != 7u || a % b != 0x0091A2B9u) fails |= 128;
    /* small dividend, large divisor */
    a = U[4]; b = U[1]; if (a / b != 0u || a % b != 0x12345678u) fails |= 256;
    /* both small */
    a = U[4]; b = U[5]; if (a / b != 43631413u || a % b != 5u) fails |= 512;
    a = U[7]; b = U[5]; if (a / b != 0u || a % b != 0u) fails |= 1024;
    /* signed: INT_MIN / -1 stays INT_MIN, -7 / 2 truncates toward zero */
    volatile int sa = (int)0x80000000, sb = -1, sc = -7, sd = 2;
    if (sa / sb != (int)0x80000000) fails |= 2048;
    if (sc / sd != -3 || sc % sd != -1) fails |= 4096;
    if (fails) {
        put("FAIL ");
        for (int i = 12; i >= 0; i--) debug_char((fails >> i) & 1 ? '1' : '0');
        debug_char('\n');
        return 1;
    }
    put("PASS\n");
    return 0;
}
