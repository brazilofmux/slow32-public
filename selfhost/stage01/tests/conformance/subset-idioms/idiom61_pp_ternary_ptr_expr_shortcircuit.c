#define CALL2(fn, a, b) fn((a), (b))
#define ROW(base, i) CALL2(row_at, (base), (i))
#define ADV(p, n) CALL2(advance, (p), (n))
#define PICK_PTR(c, a, b) ((c) ? (a) : (b))

static const unsigned char *row_at(const unsigned char *p, int i) {
    return p + (i * 2);
}

static const unsigned char *advance(const unsigned char *p, int n) {
    return p + n;
}

int main(void) {
    unsigned char tbl[12];
    int i;
    int hits;

    tbl[0] = 0x20; tbl[1] = 3;
    tbl[2] = 0x10; tbl[3] = 3;
    tbl[4] = 0x30; tbl[5] = 7;
    tbl[6] = 0x30; tbl[7] = 9;
    tbl[8] = 0x20; tbl[9] = 3;
    tbl[10] = 0; tbl[11] = 0;

    hits = 0;
    for (i = 0; i < 6; i = i + 1) {
        const unsigned char *r = ROW(tbl, i);
        const unsigned char *p = PICK_PTR((i & 1), r, ADV(r, 0));
        const unsigned char *q = ADV(p, 1);
        if ((int)(q - p) == 1 &&
            p[0] != 0 &&
            (((int)p[0] == 0x20 || (int)p[0] == 0x30) &&
             ((int)q[0] == 3 || (int)q[0] == 7))) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
