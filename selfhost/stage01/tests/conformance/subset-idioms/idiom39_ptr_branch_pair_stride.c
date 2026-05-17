static int count_hi_lo_pairs(unsigned int *pairs, int npairs) {
    int i;
    int found;
    found = 0;
    for (i = 0; i + 1 < npairs; i = i + 1) {
        unsigned int *a = pairs + (i * 2);
        unsigned int *b = a + 2;
        if (((unsigned char *)b - (unsigned char *)a) == 8
            && ((a[0] & 0xFFU) == 2U)
            && ((b[0] & 0xFFU) == 3U)
            && (a[1] == b[1])) {
            found = found + 1;
        }
    }
    return found;
}

int main(void) {
    unsigned int pairs[10];
    int got;
    pairs[0] = 2U; pairs[1] = 7U;
    pairs[2] = 3U; pairs[3] = 7U;
    pairs[4] = 2U; pairs[5] = 9U;
    pairs[6] = 3U; pairs[7] = 8U;
    pairs[8] = 3U; pairs[9] = 9U;
    got = count_hi_lo_pairs(pairs, 5);
    return (got == 1) ? 0 : 1;
}
