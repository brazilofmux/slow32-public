/* Two wrong-code defects found together (2026-10-27).
 *
 * The emitter folds an ADDI chain: addi(addi(x, 5), 1000) is emitted as
 * x + 1005, reading x, while the inner ADDI is still emitted.  Return
 * pinning (GitHub issue 72) put x, the inner ADDI and the result all in
 * r1, so the inner write clobbered x before the outer read it: chain()
 * returned x * 2 + 1010.
 *
 * A BRC whose condition folded to a constant became a BR, and the arm
 * it no longer reached kept a phi argument for it.  After full
 * unrolling every copy's `if (i != 7)` folds; the stale arguments drew
 * values into the pinned web that the emitter's ADDI folds read past:
 * unrolled() returned 598. */
int chain(int x) {
    x = x * 2 + 5;
    return x + 1000;
}

int chain3(int x) {
    x = x * 4 + 3;
    x = x + 9;
    return x - 100;
}

int unrolled(void) {
    int i;
    int t;
    t = 0;
    for (i = 0; i < 8; i++) {
        if (i != 3) t = t + i;
        else t = t * 2;
        if (i != 7) t = t + 1;
    }
    return t * 10 + i;
}

int main(void) {
    if (chain(1) != 1007) return 1;
    if (chain(-3) != 999) return 2;
    if (chain3(2) != -80) return 3;
    if (unrolled() != 388) return 4;
    return 0;
}
