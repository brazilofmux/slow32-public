/* Array compound literals -- with and without explicit size, with
 * designated initializers, indexed via a saved pointer.  Tests that
 * the literal's per-call hidden alloca holds its initialized values
 * for the duration of the access. */

static int read_at(int *a, int idx) {
    return a[idx];
}

int main(void) {
    int acc = 0;
    int i;
    for (i = 0; i < 10; i++) {
        int *a = (int[]){ [2] = 22, 33, [5] = 55 };
        int *b = (int[5]){ 1, [3] = 4 };
        char *letters = (char[4]){ [2] = 'z' };

        acc = acc * 3 + a[0] + a[2] + a[3] + a[5];
        acc = acc * 3 + b[0] + b[1] + b[3] + b[4];
        acc = acc * 3 + (int)letters[0] + (int)letters[2] + (int)letters[3];
        acc = acc * 3 + read_at((int[]){ i, i + 1, i + 2, i + 3 }, i & 3);
    }
    return acc & 0xff;
}
