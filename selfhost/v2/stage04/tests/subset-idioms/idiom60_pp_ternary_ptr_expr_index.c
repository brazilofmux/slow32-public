#define CALL2(fn, a, b) fn((a), (b))
#define ROW(base, i) CALL2(row_at, (base), (i))
#define ADV(p, n) CALL2(advance, (p), (n))
#define SEL_PTR(c, a, b) ((c) ? (a) : (b))

static unsigned char *row_at(unsigned char *p, int i) {
    return p + (i * 2);
}

static unsigned char *advance(unsigned char *p, int n) {
    return p + n;
}

int main(void) {
    unsigned char rec[10];
    int i;
    int hits;

    rec[0] = 2; rec[1] = 9;
    rec[2] = 3; rec[3] = 8;
    rec[4] = 3; rec[5] = 9;
    rec[6] = 2; rec[7] = 7;
    rec[8] = 2; rec[9] = 9;

    hits = 0;
    for (i = 0; i < 5; i = i + 1) {
        unsigned char *p = SEL_PTR(i & 1, ROW(rec, i), ADV(ROW(rec, i), 0));
        if (((int)p[0] == 2 || (int)p[0] == 3) &&
            ((int)p[1] == 9 || ((int)p[0] == 2 && (int)p[1] == 7))) {
            if ((int)p[1] == 9) hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
