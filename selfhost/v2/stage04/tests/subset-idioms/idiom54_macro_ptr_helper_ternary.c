#define ROW_AT(base, i) (row_at((base), (i)))
#define PICK_COL(k) ((k) == 3 ? 1 : 0)

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
        unsigned char *r = ROW_AT(rec, i);
        int c = PICK_COL((int)r[0]);
        if (((int)r[0] == 2 || (int)r[0] == 3) &&
            ((int)r[c] == 9 || (int)r[c] == 2)) {
            if ((int)r[1] == 9) hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
