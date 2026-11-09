/* Return-value pinning against a symbol-form store -- wrong code
 * (selfhost ISSUES-117).
 *
 * A value whose only use is the RET is computed straight into r1 (GitHub
 * issue 72) when nothing between it and the RET touches r1.  The check
 * missed a load or store selected in the symbol form, `lui r1, %hi(g+12)`,
 * when the address had a register of its own -- here the member pointers,
 * which the optimizer keeps in registers from entry.  r = 0 was set in r1
 * and the store of g.i overwrote it: f returned the address's upper half.
 * Found through fortran/'s f77 (LAPACK's DLCTSX, its COMMON members), in
 * the same backend.  Returns 1 if a selection is wrong, 0 if right. */
struct S { int m, n, mplusn, i, fs; } g;
int f(void) {
    int r;
    int *pi = &g.i, *pm = &g.m, *pn = &g.n, *pp = &g.mplusn, *pf = &g.fs;
    if (*pf) {
        *pi = *pi + 1;
        if (*pi <= *pm) r = 0; else r = 1;
        if (*pi == *pp) { *pf = 0; *pi = 0; }
    } else {
        *pi = *pi + 1;
        if (*pi <= *pn) r = 1; else r = 0;
        if (*pi == *pp) { *pf = 1; *pi = 0; }
    }
    return r;
}
int main(void) {
    static const int want[4] = { 0, 1, 1, 0 };
    int k;
    g.m = 1; g.n = 1; g.mplusn = 2; g.fs = 1; g.i = 0;
    for (k = 0; k < 4; k++)
        if (f() != want[k]) return 1;
    return 0;
}
