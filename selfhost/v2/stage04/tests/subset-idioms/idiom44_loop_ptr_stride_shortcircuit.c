static int good_pair(const unsigned char *p) {
    return ((int)p[0] == 2 || (int)p[0] == 3) && (int)p[1] == 9;
}

int main(void) {
    unsigned char rec[10];
    unsigned char *p;
    int hits;
    int i;

    rec[0] = 2; rec[1] = 9;
    rec[2] = 3; rec[3] = 8;
    rec[4] = 3; rec[5] = 9;
    rec[6] = 2; rec[7] = 7;
    rec[8] = 3; rec[9] = 9;

    p = rec;
    hits = 0;
    for (i = 0; i < 5; i = i + 1) {
        if (good_pair(p) && ((int)p[0] == 2 || (int)p[1] == 9)) {
            hits = hits + 1;
        }
        p = p + 2;
    }

    return (hits == 3) ? 0 : 1;
}
