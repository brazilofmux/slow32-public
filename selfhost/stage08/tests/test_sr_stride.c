/* Strength reduction turns a[i] into a pointer that steps by the
 * element size.  The step and the starting offset were built as
 * `1 << 2` and `0 << 2` after the last optimizer pass, so nothing
 * folded them: the stride sat in a register for the whole loop and
 * the latch added it.  Folded, the latch is `addi rP, rP, 4` and
 * run-tests.sh checks that sumw has no slli left.  The program checks
 * the sums for an int and a short array and a stride of 12 bytes. */
struct trip { int a; int b; int c; };

static int iw[9] = { 14, 3, 27, 8, 41, 6, 19, 2, 33 };
static short hw[7] = { 300, -12, 77, 5, -260, 91, 18 };
static struct trip tw[5] = {
    { 1, 70, 2 }, { 3, 51, 4 }, { 5, 66, 6 }, { 7, 29, 8 }, { 9, 13, 10 }
};

int sumw(int *v, int n) {
    int s;
    int i;
    s = 0;
    for (i = 0; i < n; i++) s = s + v[i];
    return s;
}

int sumh(short *v, int n) {
    int s;
    int i;
    s = 0;
    for (i = 0; i < n; i++) s = s + v[i];
    return s;
}

int sumb(struct trip *v, int n) {
    int s;
    int i;
    s = 0;
    for (i = 0; i < n; i++) s = s + v[i].b;
    return s;
}

int main(void) {
    if (sumw(iw, 9) != 153) return 1;
    if (sumw(iw + 4, 3) != 66) return 2;
    if (sumh(hw, 7) != 219) return 3;
    if (sumb(tw, 5) != 229) return 4;
    if (sumb(tw + 2, 2) != 95) return 5;
    return 0;
}
