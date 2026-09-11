/* GitHub issue 75: a goto into a jump-table case is a second predecessor.
 * Issue 64 trampolines C fall-through; it does not see this edge.  Without
 * a post-CFG split, the JMPTAB lands on a multi-pred block that cannot
 * carry the phi copy for `ts = p`, and `g(15, 7)` returns the old ts (0). */
static int g(int cs, int p) {
    int ts;
    ts = 0;
    switch (cs) {
    case 10:
        p = p + 1;
        goto L15;
    case 11:
        return 11;
    case 12:
        return 12;
    case 13:
        return 13;
    case 14:
        return 14;
    case 15:
    L15:
        ts = p;
        return ts;
    }
    return -1;
}

int main(void) {
    if (g(15, 7) != 7) return 1;
    if (g(10, 7) != 8) return 2;
    if (g(11, 0) != 11) return 3;
    if (g(14, 0) != 14) return 4;
    if (g(9, 0) != -1) return 5;
    return 0;
}
