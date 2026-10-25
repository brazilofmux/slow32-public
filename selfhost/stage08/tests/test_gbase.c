/* A global table indexed by a run-time value inside a loop gets its
 * address once, in the preheader, not before every lookup
 * (run-tests.sh: one %hi(squares) in mix).  More tables than the cap,
 * a table also read directly, a table written through the hoisted
 * base, and a loop that is not a leaf must all still read and write
 * the right bytes. */
unsigned char squares[16];
unsigned char cubes[16];
unsigned char t0[8];
unsigned char t1[8];
unsigned char t2[8];
unsigned char t3[8];
unsigned char t4[8];
int hits;

int mix(unsigned char *idx, int n) {
    int i;
    int s;
    s = 0;
    for (i = 0; i < n; i++) s = s + squares[idx[i]];
    return s;
}

int many(int n) {
    int i;
    int s;
    s = 0;
    for (i = 0; i < n; i++)
        s = s + t0[i & 7] + t1[(i + 1) & 7] * 2 + t2[(i + 2) & 7] * 3
              + t3[(i + 3) & 7] * 4 + t4[(i + 4) & 7] * 5;
    return s;
}

int both(int n) {
    int i;
    int s;
    s = 0;
    for (i = 0; i < n; i++) {
        cubes[i & 15] = (unsigned char)(cubes[i & 15] + squares[(i * 3) & 15]);
        s = s + cubes[i & 15];
    }
    return s;
}

int outer(int n) {
    int i;
    int j;
    int s;
    s = 0;
    for (i = 0; i < n; i++)
        for (j = 0; j < i; j++) s = s + squares[(i + j) & 15];
    return s;
}

int main(void) {
    unsigned char idx[6];
    int i;
    for (i = 0; i < 16; i++) { squares[i] = (unsigned char)(i * i); cubes[i] = (unsigned char)i; }
    for (i = 0; i < 8; i++) {
        t0[i] = (unsigned char)i; t1[i] = (unsigned char)(i + 10);
        t2[i] = (unsigned char)(i * 2); t3[i] = (unsigned char)(7 - i); t4[i] = (unsigned char)(i ^ 5);
    }
    idx[0] = 3; idx[1] = 15; idx[2] = 0; idx[3] = 7; idx[4] = 7; idx[5] = 12;
    if (mix(idx, 6) != 476) return 1;
    if (many(8) != 664) return 2;
    if (both(20) != 1618) return 3;
    if (outer(7) != 896) return 4;
    return 0;
}
