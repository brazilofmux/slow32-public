#define WRAP(x) ((x))
#define CALL2(fn, a, b) fn((a), (b))
#define ROW(base, i) WRAP(CALL2(row_at, WRAP(base), WRAP(i)))
#define CELL(base, i, j) (ROW((base), (i))[WRAP(j)])

static const unsigned char *row_at(const unsigned char *p, int i) {
    return p + (i * 2);
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
        int k = CELL(tbl, WRAP(i), WRAP((0)));
        int s = CELL(tbl, WRAP(i), WRAP((1)));
        if (k != 0 && ((k == 0x20 || k == 0x30) && (s == 3 || s == 7))) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
