static const unsigned char *step(const unsigned char *p, int n) {
    return p + n;
}

static const unsigned char *row2(const unsigned char *base, int idx) {
    return step(step(base, idx), idx);
}

static int good_kind(const unsigned char *r) {
    int k = (int)(unsigned char)r[0];
    return k == 0x20 || k == 0x30;
}

int main(void) {
    unsigned char tbl[10];
    const unsigned char *base;
    int i;
    int hits;

    tbl[0] = 0x20; tbl[1] = 3;
    tbl[2] = 0x10; tbl[3] = 3;
    tbl[4] = 0x30; tbl[5] = 7;
    tbl[6] = 0x30; tbl[7] = 9;
    tbl[8] = 0x20; tbl[9] = 3;

    base = tbl;
    hits = 0;
    for (i = 0; i < 5; i = i + 1) {
        const unsigned char *r = row2(base, i);
        if (good_kind(r) &&
            (((signed char)r[1] > 0 && (unsigned char)r[1] == 3) ||
             ((signed char)r[1] > 0 && (unsigned char)r[1] == 7))) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
