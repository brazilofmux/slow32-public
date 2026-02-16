static int accept(const char *p) {
    return (((unsigned char)p[0] == 0x20 && (unsigned char)p[1] == 3) ||
            ((unsigned char)p[0] == 0x30 && (unsigned char)p[1] == 7));
}

int main(void) {
    char tbl[12];
    char *base;
    char *p;
    int i;
    int hits;

    tbl[0] = 0x20; tbl[1] = 3;
    tbl[2] = 0x10; tbl[3] = 3;
    tbl[4] = 0x30; tbl[5] = 7;
    tbl[6] = 0x30; tbl[7] = 9;
    tbl[8] = 0x20; tbl[9] = 3;
    tbl[10] = 0x00; tbl[11] = 0x00;

    base = tbl;
    hits = 0;
    for (i = 0; i < 6; i = i + 1) {
        p = base + (i * 2);
        if ((unsigned char)p[0] != 0 &&
            ((unsigned char)p[1] == 3 || (unsigned char)p[1] == 7) &&
            accept(p)) {
            hits = hits + 1;
        }
    }
    return (hits == 3) ? 0 : 1;
}
