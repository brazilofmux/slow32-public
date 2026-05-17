static unsigned char *row_at(unsigned char *p, int idx) {
    return p + (idx * 2);
}

static int pick_col(int prefer_second) {
    return prefer_second ? 1 : 0;
}

int main(void) {
    unsigned char rec[10];
    int i;
    int hits;

    rec[0] = 2; rec[1] = 9;
    rec[2] = 3; rec[3] = 8;
    rec[4] = 2; rec[5] = 7;
    rec[6] = 3; rec[7] = 9;
    rec[8] = 2; rec[9] = 9;

    hits = 0;
    for (i = 0; i < 5; i = i + 1) {
        unsigned char *r = row_at(rec, i);
        int c0 = pick_col(0);
        int c1 = pick_col(1);
        if (((int)r[c0] == 2 || (int)r[c0] == 3) &&
            ((int)r[c1] == ((int)r[c0] == 3 ? 9 : (int)r[c1]))) {
            if ((int)r[c1] == 9) hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
