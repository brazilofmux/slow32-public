/* Loop with multiple loop-carried values.  Each is a PHI in HIR.
 * Stresses the regalloc's PHI handling — the values must be
 * coordinated across the back edge.  Each value updates differently
 * to avoid trivial coalescing, so the regalloc has to keep them
 * distinct. */
int main(void) {
    int i;
    int sum = 0;
    int product = 1;
    int max_seen = (int)0x80000000;
    int min_seen = 0x7FFFFFFF;
    int xor_acc = 0xCAFE;
    int count_pos = 0;
    int count_neg = 0;

    for (i = -100; i < 101; i++) {
        int v = i * i - 17 * i + 23;
        sum = sum + v;
        if (v != 0) product = product * (v % 13 + 1);
        if (v > max_seen) max_seen = v;
        if (v < min_seen) min_seen = v;
        xor_acc = xor_acc ^ v;
        if (v > 0) count_pos = count_pos + 1;
        if (v < 0) count_neg = count_neg + 1;
    }

    int acc = sum;
    acc = acc * 7 + product;
    acc = acc * 7 + max_seen;
    acc = acc * 7 + min_seen;
    acc = acc * 7 + xor_acc;
    acc = acc * 7 + count_pos;
    acc = acc * 7 + count_neg;
    return acc & 0xff;
}
