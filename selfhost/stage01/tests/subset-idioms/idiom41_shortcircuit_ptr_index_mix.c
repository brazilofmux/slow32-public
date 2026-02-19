static int nonzero(int v) {
    return v != 0;
}

static int tagged_match(const char *p, int i) {
    int tag = (unsigned char)p[i];
    int sym = (unsigned char)p[i + 1];
    return (tag == 2 || tag == 3) && sym == 9;
}

int main(void) {
    char tbl[10];
    int i;
    int ok;

    tbl[0] = 2; tbl[1] = 9;
    tbl[2] = 3; tbl[3] = 8;
    tbl[4] = 3; tbl[5] = 9;
    tbl[6] = 0; tbl[7] = 0;
    tbl[8] = 2; tbl[9] = 9;

    ok = 0;
    for (i = 0; i + 1 < 10; i = i + 2) {
        if (nonzero((unsigned char)tbl[i]) &&
            tagged_match(tbl, i) &&
            (((unsigned char)tbl[i] == 2 && (unsigned char)tbl[i + 1] == 9) ||
             ((unsigned char)tbl[i] == 3 && (unsigned char)tbl[i + 1] == 9))) {
            ok = ok + 1;
        }
    }

    return (ok == 3) ? 0 : 1;
}
