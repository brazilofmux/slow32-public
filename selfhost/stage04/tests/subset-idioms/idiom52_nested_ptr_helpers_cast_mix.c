static unsigned char *advance(unsigned char *p, int n) {
    return p + n;
}

static unsigned char *at_row(unsigned char *base, int idx) {
    return advance(base, idx * 2);
}

static int sel_col(unsigned char *row) {
    return ((signed char)row[0] < 0) ? 0 : 1;
}

int main(void) {
    unsigned char rec[12];
    int i;
    int hits;

    rec[0] = 0x80; rec[1] = 2;
    rec[2] = 0x20; rec[3] = 9;
    rec[4] = 0xFF; rec[5] = 3;
    rec[6] = 0x30; rec[7] = 7;
    rec[8] = 0x81; rec[9] = 4;
    rec[10] = 0x20; rec[11] = 3;

    hits = 0;
    for (i = 0; i < 6; i = i + 1) {
        unsigned char *r = at_row(rec, i);
        int c = sel_col(r);
        if ((((unsigned char)r[0] == 0x20 || (unsigned char)r[0] == 0x30) &&
             ((int)r[c] == 9 || (int)r[c] == 3 || (int)r[c] == 7))) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
