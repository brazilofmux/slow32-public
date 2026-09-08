/* GitHub issue 49: file-scope constant shifts must fold on the
 * pc_hi:lo pair.  1LL << 40 as a 32-bit host shift is 1<<8 on x86-64
 * (amount masked to 5 bits) or 0 on slow32, so !(1LL << 40) was 0
 * for the wrong reason.  1LL << 32 is bit 32, which a 32-bit shift
 * cannot represent.  Own file because t_const_unary returns 512. */
static long long shl32 = 1LL << 32;
static long long shl40 = 1LL << 40;
static long long shr32 = 0x100000000LL >> 32;
static long long shl0 = 1LL << 0;
static int not_shl32 = !(1LL << 32);
static int not_wide = !(0x100000000LL);
static int shl4 = 1 << 4;

int main(void) {
    if (shl32 != 0x100000000LL) return 1;
    if (shl40 != 0x10000000000LL) return 2;
    if (shr32 != 1) return 3;
    if (shl0 != 1) return 4;
    if (not_shl32 != 0) return 5;
    if (not_wide != 0) return 6;
    if (shl4 != 16) return 7;
    return 0;
}
