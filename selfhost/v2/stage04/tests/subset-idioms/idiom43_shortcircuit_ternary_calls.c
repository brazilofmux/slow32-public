static int is_type(unsigned char v, unsigned char t) {
    return (int)v == (int)t;
}

static int sym_eq(const unsigned char *p, int i, int want) {
    return (int)p[i + 1] == want;
}

int main(void) {
    unsigned char rec[8];
    int i;
    int hits;
    rec[0] = 2; rec[1] = 7;
    rec[2] = 3; rec[3] = 7;
    rec[4] = 2; rec[5] = 9;
    rec[6] = 3; rec[7] = 9;

    hits = 0;
    for (i = 0; i + 1 < 8; i = i + 2) {
        if ((is_type(rec[i], 2) || is_type(rec[i], 3)) &&
            (sym_eq(rec, i, rec[i] == 2 ? 7 : 9))) {
            hits = hits + 1;
        }
    }
    return (hits == 2) ? 0 : 1;
}
