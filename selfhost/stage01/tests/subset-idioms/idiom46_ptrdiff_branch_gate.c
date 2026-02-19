static int is_stride2(const unsigned char *a, const unsigned char *b) {
    return ((int)(b - a)) == 2;
}

int main(void) {
    unsigned char rel[8];
    unsigned char *p;
    int hits;

    rel[0] = 2; rel[1] = 9;
    rel[2] = 3; rel[3] = 9;
    rel[4] = 2; rel[5] = 8;
    rel[6] = 3; rel[7] = 9;

    p = rel;
    hits = 0;
    while (p < rel + 6) {
        if (is_stride2(p, p + 2) &&
            (((int)p[0] == 2 && (int)p[1] == 9) ||
             ((int)(p + 2)[0] == 3 && (int)(p + 2)[1] == 9))) {
            hits = hits + 1;
        }
        p = p + 2;
    }
    return (hits == 2) ? 0 : 1;
}
