static int pick_idx(int i, int choose_next) {
    return choose_next ? (i + 1) : i;
}

int main(void) {
    unsigned char rec[10];
    unsigned char *base;
    int i;
    int hits;

    rec[0] = 2; rec[1] = 7;
    rec[2] = 3; rec[3] = 9;
    rec[4] = 2; rec[5] = 9;
    rec[6] = 3; rec[7] = 8;
    rec[8] = 2; rec[9] = 9;

    base = rec;
    hits = 0;
    for (i = 0; i < 10; i = i + 2) {
        int t = pick_idx(i, 0);
        int s = pick_idx(i, 1);
        if ((int)((base + s) - (base + t)) == 1 &&
            (((int)base[t] == 2 || (int)base[t] == 3) && (int)base[s] == 9)) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
