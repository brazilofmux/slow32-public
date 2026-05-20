/* Explicit goto-driven control flow with multi-predecessor merges.
 * The HIR will lower this into a CFG with PHIs at each merge point.
 * Tests that the codegen handles labels reached from multiple sources
 * — exactly the multi-predecessor PHI shape that the gc_combine
 * edge-transfer bug latched onto. */
static int labyrinth(int seed) {
    int x = seed;
    int trips = 0;

start:
    if (trips >= 100) goto done;
    trips = trips + 1;
    x = x * 1103515245 + 12345;

    if ((x & 7) == 0) goto bump_low;
    if ((x & 7) == 1) goto bump_mid;
    if ((x & 7) == 2) goto bump_high;
    goto start;

bump_low:
    x = x ^ 0x000000FF;
    goto merge;

bump_mid:
    x = x ^ 0x0000FF00;
    goto merge;

bump_high:
    x = x ^ 0x00FF0000;
    goto merge;

merge:
    if (x < 0) x = -x;
    goto start;

done:
    return x ^ trips;
}

int main(void) {
    int acc = 0;
    int s;
    for (s = 1; s < 32; s++) {
        acc = acc * 13 + labyrinth(s * 0xDEAD);
    }
    return acc & 0xff;
}
