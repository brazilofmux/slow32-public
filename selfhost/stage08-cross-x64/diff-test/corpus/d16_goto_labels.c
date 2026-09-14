/* Explicit goto-driven control flow with multi-predecessor merges.
 * The HIR will lower this into a CFG with PHIs at each merge point.
 * Tests that the codegen handles labels reached from multiple sources
 * — exactly the multi-predecessor PHI shape that the gc_combine
 * edge-transfer bug latched onto. */
/* x is unsigned: the LCG step overflows, and as signed int that is
 * undefined -- gcc -O2 on aarch64 folded the later `x < 0` test on it
 * and answered 93 where -O0, -fwrapv, clang, cc-a64 and cc.s32x all
 * answer 27 (GitHub issue 79's a64 run).  The merge shape is unchanged. */
static int labyrinth(int seed) {
    unsigned int x = (unsigned int)seed;
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
    if (x & 0x80000000u) x = 0u - x;
    goto start;

done:
    return (int)(x ^ (unsigned int)trips);
}

int main(void) {
    int acc = 0;
    int s;
    for (s = 1; s < 32; s++) {
        acc = acc * 13 + labyrinth(s * 0xDEAD);
    }
    return acc & 0xff;
}
