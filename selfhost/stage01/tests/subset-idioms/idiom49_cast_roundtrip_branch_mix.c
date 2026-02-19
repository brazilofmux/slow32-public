static int match_kind(unsigned char k) {
    return (int)k == 0x20 || (int)k == 0x30;
}

int main(void) {
    unsigned char buf[12];
    void *base;
    int i;
    int hits;

    buf[0] = 0x20; buf[1] = 3;
    buf[2] = 0x10; buf[3] = 3;
    buf[4] = 0x30; buf[5] = 7;
    buf[6] = 0x30; buf[7] = 9;
    buf[8] = 0x20; buf[9] = 3;
    buf[10] = 0x00; buf[11] = 0x00;

    base = (void *)buf;
    hits = 0;
    for (i = 0; i < 6; i = i + 1) {
        unsigned char *p = (unsigned char *)base + (i * 2);
        void *row = (void *)p;
        unsigned char *q = (unsigned char *)row;
        if ((int)(q - (unsigned char *)base) == (i * 2) &&
            q[0] != 0 &&
            (q[1] == 3 || q[1] == 7) &&
            match_kind(q[0])) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
