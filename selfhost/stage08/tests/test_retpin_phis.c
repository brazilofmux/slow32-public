/* Return values may be computed straight into r1 (GitHub issue 72's
 * retpin), one return at a time.  Two returns whose values are both
 * phis of one join block -- here each phi merges parameters only, so
 * the value's whole web is that phi -- were both pinned to r1: the
 * conflict scan walked the block's instructions and never its phis.
 * swapsel(..., c = 0) returned a.  A rotated loop's exit (strrchr's `s`
 * and `last`) has the same shape. */
static char text[8] = "quokka";

char *swapsel(char *s, char *t, int k, int c) {
    char *a;
    char *b;
    if (k) { a = s; b = t; } else { a = t; b = s; }
    if (c) return a;
    return b;
}

int pairsel(int x, int y, int k, int c) {
    int a;
    int b;
    if (k > 0) { a = x; b = y; } else { a = y; b = x; }
    if (c == 0) return b;
    return a;
}

int main(void) {
    if (swapsel(text, text + 5, 1, 1) != text) return 1;
    if (swapsel(text, text + 5, 1, 0) != text + 5) return 2;
    if (swapsel(text, text + 5, 0, 1) != text + 5) return 3;
    if (swapsel(text, text + 5, 0, 0) != text) return 4;
    if (pairsel(61, 29, 3, 0) != 29) return 5;
    if (pairsel(61, 29, 3, 7) != 61) return 6;
    if (pairsel(61, 29, -1, 0) != 61) return 7;
    if (pairsel(61, 29, -1, 7) != 29) return 8;
    return 0;
}
