static int in_range(int v, int lo, int hi) {
    return v >= lo && v <= hi;
}

static int is_pair(const unsigned char *p, int i) {
    int a = (int)p[i];
    int b = (int)p[i + 1];
    return in_range(a, 32, 47) && in_range(b, 3, 3);
}

int main(void) {
    unsigned char rel[8];
    int i = 0;
    int hits = 0;
    rel[0] = 0x21;
    rel[1] = 0x03;
    rel[2] = 0x10;
    rel[3] = 0x03;
    rel[4] = 0x2A;
    rel[5] = 0x03;
    rel[6] = 0x2F;
    rel[7] = 0x02;

    while (i + 1 < 8) {
        if (is_pair(rel, i) && ((int)rel[i + 1] == 3 || (int)rel[i] == 0x2A)) {
            hits = hits + 1;
        }
        i = i + 2;
    }
    return (hits == 2) ? 0 : 1;
}
