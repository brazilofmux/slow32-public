/* Test: a variable undefined on entry to a loop and set only on some
 * iterations.  Phi simplification took -1 -- the undefined value, a
 * phi argument like any other -- as its "none yet" sentinel, so
 * phi(undefined, x) became x, which need not dominate the phi: best
 * became the loop index itself, its value when the loop ended (6, not
 * the index of the maximum).  Found by fort370 (SYMQR sorting every
 * eigenvalue to the same one); stage05 through stage08 had it. */

int write(int fd, char *buf, int len);

int my_strlen(char *s) { int n = 0; while (*(s+n)) n++; return n; }
void my_puts(char *s) { write(1, s, my_strlen(s)); }
void print_ok(char *name) { my_puts("  OK: "); my_puts(name); my_puts("\n"); }
void print_fail(char *name) { my_puts("  FAIL: "); my_puts(name); my_puts("\n"); }

__attribute__((noinline))
int argmax(int *d, int n) {
    int j;
    int best;
    int vmax;
    vmax = -1;
    for (j = 0; j < n; j++) {
        if (d[j] <= vmax) continue;
        vmax = d[j];
        best = j;
    }
    return best;
}

int test_phi_undef(void) {
    int d[6];
    d[0] = 3; d[1] = 1; d[2] = 4; d[3] = 1; d[4] = 5; d[5] = 2;
    if (argmax(d, 6) != 4) return 1;
    d[0] = 9;
    if (argmax(d, 6) != 0) return 2;
    return 0;
}

int main(void) {
    int fail;
    fail = 0;
    if (test_phi_undef() == 0) print_ok("phi of an undefined value");
    else { print_fail("phi of an undefined value"); fail = 1; }
    return fail;
}
