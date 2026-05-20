/* All 12 comparison ops (signed × {eq, ne, lt, gt, le, ge} +
 * unsigned × {lt, gt, le, ge}) on a range that spans negative,
 * zero, positive, and the unsigned-wrap boundary.  Catches
 * misorderings between SLT/SLTU and signed/unsigned codegen paths. */
static unsigned int fold_comparisons(int a, int b) {
    unsigned int ua = (unsigned int)a;
    unsigned int ub = (unsigned int)b;
    unsigned int acc = 0;
    acc = (acc << 1) | ((a == b) ? 1u : 0u);
    acc = (acc << 1) | ((a != b) ? 1u : 0u);
    acc = (acc << 1) | ((a <  b) ? 1u : 0u);
    acc = (acc << 1) | ((a >  b) ? 1u : 0u);
    acc = (acc << 1) | ((a <= b) ? 1u : 0u);
    acc = (acc << 1) | ((a >= b) ? 1u : 0u);
    acc = (acc << 1) | ((ua <  ub) ? 1u : 0u);
    acc = (acc << 1) | ((ua >  ub) ? 1u : 0u);
    acc = (acc << 1) | ((ua <= ub) ? 1u : 0u);
    acc = (acc << 1) | ((ua >= ub) ? 1u : 0u);
    return acc;
}

int main(void) {
    int seeds[8];
    seeds[0] = -1000000;
    seeds[1] = -1;
    seeds[2] = 0;
    seeds[3] = 1;
    seeds[4] = 1000000;
    seeds[5] = 0x7FFFFFFF;
    seeds[6] = (int)0x80000000;
    seeds[7] = (int)0x80000001;

    unsigned int acc = 0xCAFEBABEu;
    int i;
    for (i = 0; i < 8; i++) {
        int j;
        for (j = 0; j < 8; j++) {
            acc = acc * 31u + fold_comparisons(seeds[i], seeds[j]);
        }
    }
    return acc & 0xff;
}
