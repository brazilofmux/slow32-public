/* 64-bit global and static initializers keep both words (the high word
 * was dropped: -2 read 0x00000000fffffffe, 1ULL << 32 went to BSS as 0). */
static unsigned long long st = 0x9E3779B97F4A7C15ULL;
static long long sn = -2;
unsigned long long hi_only = 1ULL << 32;
long long big = 0x123456789ABCDEFLL;
int after = 7;                      /* the next global is not disturbed */
int main(void) {
    static long long loc = -5000000000LL;
    if (st != 0x9E3779B97F4A7C15ULL) return 2;
    if (sn != -2) return 3;
    if ((sn >> 32) != -1) return 4;
    if (hi_only != (1ULL << 32)) return 5;
    if ((int)(hi_only >> 32) != 1) return 6;
    if (big != 0x123456789ABCDEFLL) return 7;
    if (loc != -5000000000LL) return 8;
    if (after != 7) return 9;
    return 1;
}
