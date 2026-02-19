#define ID(x) (x)
#define PASS1(x) ID(x)
#define PASS2(x) PASS1(x)
#define ROW_AT(base, i) row_at((base), (i))
#define GET(base, i, off) (PASS2(ROW_AT((base), (i)))[(off)])

static unsigned char *row_at(unsigned char *p, int i) {
    return p + (i * 2);
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
        int k = GET(rec, i, ID((0, 0)));
        int s = GET(rec, i, ID((0, 1)));
        if ((k == 2 || k == 3) && (s == 9 || (k == 2 && s == 7))) {
            if (s == 9) hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
