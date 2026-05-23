/* Test: functions with > 8 args (stack-passed slots).
 * SLOW-32 ABI puts args 1-8 in r3..r10; args 9+ go on the stack and
 * the callee must load them from fp+0, fp+4, etc. */

int write(int fd, char *buf, int len);

int my_strlen(char *s) { int n = 0; while (*(s+n)) n++; return n; }
void my_puts(char *s) { write(1, s, my_strlen(s)); }
void print_ok(char *name) { my_puts("  OK: "); my_puts(name); my_puts("\n"); }
void print_fail(char *name) { my_puts("  FAIL: "); my_puts(name); my_puts("\n"); }

/* Each arg gets a distinct power-of-two so a wrong sum is easy to diagnose. */
int sum9(int a, int b, int c, int d, int e, int f, int g, int h, int i) {
    return a + b + c + d + e + f + g + h + i;
}

int test_sum9(void) {
    if (sum9(1, 2, 4, 8, 16, 32, 64, 128, 256) != 511) return 1;
    return 0;
}

int sum12(int a, int b, int c, int d, int e, int f, int g, int h,
          int i, int j, int k, int l) {
    return a + b + c + d + e + f + g + h + i + j + k + l;
}

int test_sum12(void) {
    if (sum12(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024, 2048) != 4095) return 1;
    return 0;
}

/* Pointer out-params past the 8th: mirrors the bf_alloc shape that
 * surfaced this bug originally. */
void compute(int x, int y, int z, int *a, int *b, int *c, int *d, int *e, int *f) {
    *a = x + 1;
    *b = y + 2;
    *c = z + 3;
    *d = x + y;
    *e = y + z;
    *f = x + y + z;
}

int test_ptr_outparams(void) {
    int a, b, c, d, e, f;
    a = b = c = d = e = f = -1;
    compute(10, 20, 30, &a, &b, &c, &d, &e, &f);
    if (a != 11) return 1;
    if (b != 22) return 2;
    if (c != 33) return 3;
    if (d != 30) return 4;
    if (e != 50) return 5;
    if (f != 60) return 6;
    return 0;
}

/* Mixed: stack arg must not collide with caller-saved register usage
 * for in-flight computations.  Adding `g+h+i+j` after using `i` and
 * `j` (stack args) confirms they aren't clobbered mid-function. */
int mixed10(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j) {
    int s1, s2;
    s1 = a + b + c + d;
    s2 = e + f + g + h;
    return s1 + s2 + i + j;
}

int test_mixed10(void) {
    if (mixed10(1, 2, 4, 8, 16, 32, 64, 128, 256, 512) != 1023) return 1;
    return 0;
}

int main(void) {
    int fail;
    fail = 0;
    if (test_sum9() == 0) print_ok("sum9 (9 args)");
    else { print_fail("sum9 (9 args)"); fail = 1; }
    if (test_sum12() == 0) print_ok("sum12 (12 args)");
    else { print_fail("sum12 (12 args)"); fail = 1; }
    if (test_ptr_outparams() == 0) print_ok("9-arg ptr out-params");
    else { print_fail("9-arg ptr out-params"); fail = 1; }
    if (test_mixed10() == 0) print_ok("mixed10 (10 args)");
    else { print_fail("mixed10 (10 args)"); fail = 1; }
    return fail;
}
