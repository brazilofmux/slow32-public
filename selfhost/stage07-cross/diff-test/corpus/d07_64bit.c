/* 64-bit integer arithmetic — exercises the libcall paths for mul/div
 * and the UMUL_LOHI / SMUL_LOHI custom lowerings. */
static long long mix64(long long a, long long b) {
    long long x = a * b;
    long long y = a + b;
    long long z = (b != 0) ? (a / b) : 0;
    long long w = (b != 0) ? (a % b) : 0;
    return x ^ y ^ z ^ w ^ (x >> 17) ^ (y << 13);
}

int main(void) {
    long long acc = 0;
    int i;
    for (i = -10; i <= 10; i++) {
        long long a = (long long)i * 0x123456789ABCDEFLL;
        long long b = (long long)(i + 7) * 0xFEDCBA9876543210LL;
        acc ^= mix64(a, b);
        acc = (acc << 1) ^ (acc >> 63);
    }
    return (int)(acc & 0xff);
}
