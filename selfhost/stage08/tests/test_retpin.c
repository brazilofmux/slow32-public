/* GitHub issue 72: cross-block r1 pinning.
 * afterloop's accumulator should live in r1 (no copy at the return).
 * diamond's join phi should be r1 (each arm writes r1). */
int afterloop(int n) {
    int i, s;
    s = 0;
    for (i = 0; i < n; i++) s = s + i;
    return s;
}
int diamond(int x, int y) {
    int t;
    if (x > 0) t = x + 1;
    else t = y + 2;
    return t;
}
int early(int x) { if (x > 3) return 100; return x; }

int main(void) {
    if (afterloop(5) != 10) return 1;
    if (diamond(3, 4) != 4) return 2;
    if (diamond(-1, 4) != 6) return 3;
    if (early(2) != 2) return 4;
    if (early(9) != 100) return 5;
    return 0;
}
