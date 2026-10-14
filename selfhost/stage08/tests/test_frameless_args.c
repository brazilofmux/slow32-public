/* Test: a leaf with stack-passed arguments and nothing to keep in a
 * frame.  A frameless leaf never sets r30 -- it is still the caller's
 * frame pointer -- and read its 9th and later arguments through it, so
 * put9 stored through the caller's slot and get10 returned the caller's
 * locals.  The leaves in test_many_args.c all keep a frame (callee-saved
 * registers), which is why they never saw it.  Found by fort370 on a
 * 15-argument Fortran leaf. */

int write(int fd, char *buf, int len);

int my_strlen(char *s) { int n = 0; while (*(s+n)) n++; return n; }
void my_puts(char *s) { write(1, s, my_strlen(s)); }
void print_ok(char *name) { my_puts("  OK: "); my_puts(name); my_puts("\n"); }
void print_fail(char *name) { my_puts("  FAIL: "); my_puts(name); my_puts("\n"); }

__attribute__((noinline))
void put9(int a, int b, int c, int d, int e, int f, int g, int h, int *p) {
    *p = a + h;
}

__attribute__((noinline))
int get10(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    return i * 10 + j;
}

/* The caller has a frame of its own whose r30-relative words are not
 * the arguments: the bug read these instead. */
int test_frameless(void) {
    int r;
    int decoy[4];
    decoy[0] = 1111; decoy[1] = 2222; decoy[2] = 3333; decoy[3] = 4444;
    r = -1;
    put9(1, 2, 3, 4, 5, 6, 7, 8, &r);
    if (r != 9) return 1;
    if (get10(1, 2, 3, 4, 5, 6, 7, 8, 9, 4) != 94) return 2;
    if (get10(0, 0, 0, 0, 0, 0, 0, 0, decoy[0], decoy[3]) != 15554) return 3;
    return 0;
}

int main(void) {
    int fail;
    fail = 0;
    if (test_frameless() == 0) print_ok("frameless leaf, stack args");
    else { print_fail("frameless leaf, stack args"); fail = 1; }
    return fail;
}
