/* selfhost ISSUES-70: file-scope constants are folded on the pc_hi:lo
 * pair through + - & | ^, not just shifts.  INT64_MIN's usual spelling,
 * -9223372036854775807LL - 1, was 0: the 32-bit sum of the low words is
 * 0 and the high word was dropped.  Comparisons must not leave a stale
 * high word under their 0/1, and ?: keeps the chosen arm's. */
static long long min64 = -9223372036854775807LL - 1;
static long long tbl[] = { -0x7FFFFFFFFFFFFFFFLL - 1, 0x100000000LL + 1, (1LL << 40) - 1, -1LL - 0x100000000LL };
static long long band = 0x0F0F0F0F0F0F0F0FLL & 0xFF00000000000000LL;
static long long bor = 0x1LL | (1LL << 33);
static long long bxor = 0x100000000LL ^ 1;
static long long eqv = (0LL - 1) == -1;
static long long cnd = 1 ? -5LL - 0x100000000LL : 3;
static long long cnd2 = 0 ? 3 : 0x100000000LL - 1;

int main(void) {
    if (min64 != -9223372036854775807LL - 1 || min64 >= 0) return 1;
    if (tbl[0] != min64) return 2;
    if (tbl[1] != 4294967297LL) return 3;
    if (tbl[2] != 1099511627775LL) return 4;
    if (tbl[3] != -4294967297LL) return 5;
    if (band != 0x0F00000000000000LL) return 6;
    if (bor != 8589934593LL) return 7;
    if (bxor != 4294967297LL) return 8;
    if (eqv != 1) return 9;
    if (cnd != -4294967301LL) return 10;
    if (cnd2 != 4294967295LL) return 11;
    return 0;
}
