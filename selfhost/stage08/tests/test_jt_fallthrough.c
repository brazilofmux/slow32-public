/* GitHub issue 64: top-level case fall-through still gets a jump table.
 * The table points at a trampoline that BRs to the case block, so the
 * JMPTAB edge stays single-predecessor.  Duff's device (nested labels)
 * stays on the comparison path — see test_duff_jt.c. */
static int fall(int x) {
    int r;
    r = 0;
    switch (x) {
    case 0:
        r = r + 1;
    case 1:
        r = r + 2;
        break;
    case 2:
        r = r + 4;
        break;
    case 3:
        r = r + 8;
        break;
    case 4:
        r = r + 16;
        break;
    }
    return r;
}

/* A fall-through landing on a COALESCED group: case 1 and case 2 share a
 * block, so both table entries must route through the trampoline.  Marking
 * only the slot the fall-through lands on left table[2] pointing straight
 * at the shared block and coal(2) returned 4 instead of 2. */
static int coal(int x) {
    int r;
    r = 0;
    switch (x) {
    case 0:
        r = r + 1;
    case 1:
    case 2:
        r = r + 2;
        break;
    case 3:
        r = r + 8;
        break;
    case 4:
        r = r + 16;
        break;
    case 5:
        r = r + 32;
        break;
    }
    return r;
}

static int dense(int x) {
    switch (x) {
    case 0: return 10;
    case 1: return 11;
    case 2: return 12;
    case 3: return 13;
    case 4: return 14;
    }
    return -1;
}

int main(void) {
    if (fall(0) != 3) return 1;
    if (fall(1) != 2) return 2;
    if (fall(2) != 4) return 3;
    if (fall(3) != 8) return 4;
    if (fall(4) != 16) return 5;
    if (fall(5) != 0) return 6;
    if (coal(0) != 3) return 10;
    if (coal(1) != 2) return 11;
    if (coal(2) != 2) return 12;
    if (coal(3) != 8) return 13;
    if (coal(4) != 16) return 14;
    if (coal(5) != 32) return 15;
    if (coal(6) != 0) return 16;
    if (dense(0) != 10) return 7;
    if (dense(4) != 14) return 8;
    if (dense(9) != -1) return 9;
    return 0;
}
