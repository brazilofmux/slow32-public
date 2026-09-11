/* GitHub issue 77: spilled values share frame slots by live range.
 * Each phase keeps 30 values live across a call, more than the 19
 * callee-saved colors, so each phase spills; the phases do not overlap,
 * so their spills share slots.  Without sharing the frame held ~44
 * private slots (448-byte frame); with it, 15 (284 bytes).  run-tests.sh
 * checks the frame stays under 320. */
int sink(int x) { return x + 1; }

#define PHASE(a) \
    v0 = a * 3; v1 = a * 5; v2 = a * 7; v3 = a * 11; v4 = a * 13; \
    v5 = a * 17; v6 = a * 19; v7 = a * 23; v8 = a * 29; v9 = a * 31; \
    w0 = a ^ 1; w1 = a ^ 2; w2 = a ^ 3; w3 = a ^ 4; w4 = a ^ 5; \
    w5 = a ^ 6; w6 = a ^ 7; w7 = a ^ 8; w8 = a ^ 9; w9 = a ^ 10; \
    x0 = a + 1; x1 = a + 2; x2 = a + 3; x3 = a + 4; x4 = a + 5; \
    x5 = a + 6; x6 = a + 7; x7 = a + 8; x8 = a + 9; x9 = a + 10; \
    r += sink(a); \
    r += v0 + v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9; \
    r += w0 + w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9; \
    r += x0 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9;

int phases(int a, int b, int c, int d) {
    int v0, v1, v2, v3, v4, v5, v6, v7, v8, v9;
    int w0, w1, w2, w3, w4, w5, w6, w7, w8, w9;
    int x0, x1, x2, x3, x4, x5, x6, x7, x8, x9;
    int r = 0;
    PHASE(a)
    PHASE(b)
    PHASE(c)
    PHASE(d)
    return r;
}

static int ref(int a) {
    int r = a + 1;
    r += a * (3 + 5 + 7 + 11 + 13 + 17 + 19 + 23 + 29 + 31);
    r += (a ^ 1) + (a ^ 2) + (a ^ 3) + (a ^ 4) + (a ^ 5) + (a ^ 6) + (a ^ 7) + (a ^ 8) + (a ^ 9) + (a ^ 10);
    r += 10 * a + 55;
    return r;
}

int main(void) {
    if (phases(1, 2, 3, 4) != ref(1) + ref(2) + ref(3) + ref(4)) return 1;
    if (phases(100, -7, 0, 55) != ref(100) + ref(-7) + ref(0) + ref(55)) return 2;
    return 0;
}
