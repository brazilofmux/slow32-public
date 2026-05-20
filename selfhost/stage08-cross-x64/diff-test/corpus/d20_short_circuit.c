/* && / || / ?: with side-effecting calls in the operand positions.
 * The C standard requires lazy evaluation — RHS of && evaluated only
 * when LHS is true; RHS of || only when LHS is false; ?: branches
 * pick exactly one arm.  A miscompile that evaluates both sides
 * would corrupt the side effect counter. */

static int g_calls = 0;

static int side(int v) {
    g_calls = g_calls + 1;
    return v;
}

static int evaluate(int a, int b, int c) {
    int r = 0;
    if (side(a) && side(b)) r = r + 1;
    if (side(a) || side(b)) r = r + 2;
    if (side(a) && (side(b) || side(c))) r = r + 4;
    if ((side(a) || side(b)) && side(c)) r = r + 8;
    r = r * 16 + (side(a) ? side(b) : side(c));
    r = r * 16 + (side(a) ? (side(b) ? 1 : 2) : (side(c) ? 3 : 4));
    return r;
}

int main(void) {
    int i;
    int acc = 0;
    for (i = 0; i < 8; i++) {
        int j;
        for (j = 0; j < 8; j++) {
            int k;
            for (k = 0; k < 8; k++) {
                int a = (i & 1);
                int b = (j & 1);
                int c = (k & 1);
                acc = acc * 7 + evaluate(a, b, c);
            }
        }
    }
    /* Mix in g_calls so any miscount of side effects shows up. */
    return (acc + g_calls) & 0xff;
}
