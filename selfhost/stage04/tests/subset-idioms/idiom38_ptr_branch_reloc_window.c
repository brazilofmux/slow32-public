static int scan_window(unsigned int *rel, int n) {
    int i;
    int hits;
    hits = 0;
    for (i = 0; i + 1 < n; i = i + 1) {
        unsigned int *cur = rel + i;
        unsigned int *nxt = cur + 1;
        if (((unsigned char *)nxt - (unsigned char *)cur) == 4
            && ((*cur & 0xF0U) == 0x20U)
            && ((*nxt & 0x0FU) == 0x03U)) {
            hits = hits + 1;
        }
    }
    return hits;
}

int main(void) {
    unsigned int rel[8];
    int got;
    rel[0] = 0x21U;
    rel[1] = 0x13U;
    rel[2] = 0x20U;
    rel[3] = 0x12U;
    rel[4] = 0x2AU;
    rel[5] = 0x03U;
    rel[6] = 0x10U;
    rel[7] = 0x03U;
    got = scan_window(rel, 8);
    return (got == 2) ? 0 : 1;
}
