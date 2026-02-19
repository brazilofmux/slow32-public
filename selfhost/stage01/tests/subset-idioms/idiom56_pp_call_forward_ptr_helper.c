#define CALL1(fn, a) fn((a))
#define CALL2(fn, a, b) fn((a), (b))
#define ROW(base, i) CALL2(row_at, (base), (i))

static unsigned char *row_at(unsigned char *p, int i) {
    return p + (i * 2);
}

static int good(unsigned char *r) {
    return ((int)r[0] == 2 || (int)r[0] == 3) && (int)r[1] == 9;
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
        unsigned char *r = ROW(rec, i);
        if (CALL1(good, r) || ((int)r[0] == 2 && (int)r[1] == 9)) {
            if ((int)r[1] == 9) hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
