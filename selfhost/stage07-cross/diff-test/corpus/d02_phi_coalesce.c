/* PHI coalesce hazard: two PARAMs flow into a downstream PHI via
 * different branches.  Tests that PHI-arg coalescing doesn't merge
 * values whose constraints differ.  Each branch also does enough
 * arithmetic to provoke register pressure. */
static int twist(int a, int b, int sel) {
    int x;
    if (sel & 1) {
        x = a * 3 - b;
    } else {
        x = b * 5 - a;
    }
    return x ^ (x << 7) ^ (x >> 3);
}

int main(void) {
    int acc = 0;
    int i;
    for (i = 0; i < 100; i++) {
        acc = acc + twist(i, i * 7, i);
        acc = acc - twist(i + 1, i * 3, i ^ 5);
    }
    return acc & 0xff;
}
