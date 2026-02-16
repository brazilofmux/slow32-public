#define CALL1(fn, a) fn((a))
#define CALL2(fn, a, b) fn((a), (b))
#define ADV(p, n) CALL2(advance, (p), (n))
#define ROW(base, i) ADV((base), ((i) * 2))

static const unsigned char *advance(const unsigned char *p, int n) {
    return p + n;
}

static int kind_ok(int k) {
    return k == 0x20 || k == 0x30;
}

int main(void) {
    unsigned char tbl[12];
    const unsigned char *base;
    int i;
    int hits;

    tbl[0] = 0x20; tbl[1] = 3;
    tbl[2] = 0x10; tbl[3] = 3;
    tbl[4] = 0x30; tbl[5] = 7;
    tbl[6] = 0x30; tbl[7] = 9;
    tbl[8] = 0x20; tbl[9] = 3;
    tbl[10] = 0; tbl[11] = 0;

    base = tbl;
    hits = 0;
    for (i = 0; i < 6; i = i + 1) {
        const unsigned char *r = ROW(base, i);
        const unsigned char *q = ADV(r, 1);
        if ((int)(q - r) == 1 &&
            r[0] != 0 &&
            CALL1(kind_ok, (int)r[0]) &&
            (((int)q[0] == 3 || (int)q[0] == 7) || ((int)r[0] == 0x20 && (int)q[0] == 3))) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
