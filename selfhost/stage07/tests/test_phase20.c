/* Phase 20 test: static local variables */

int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

int my_puts(char *s) {
    int len;
    len = my_strlen(s);
    write(1, s, len);
    return 0;
}

/* Test 1: basic counter */
int counter(void) {
    static int count = 0;
    count = count + 1;
    return count;
}

/* Test 2: non-zero initializer */
int get_val(void) {
    static int x = 42;
    return x;
}

/* Test 3: zero-init (BSS) */
int get_zero(void) {
    static int z;
    return z;
}

/* Test 4: multiple static locals in same function */
int multi(void) {
    static int a = 10;
    static int b = 20;
    a = a + 1;
    b = b + 2;
    return a + b;
}

/* Test 5: static locals in different functions (independent) */
int inc_a(void) {
    static int v = 0;
    v = v + 1;
    return v;
}

int inc_b(void) {
    static int v = 0;
    v = v + 10;
    return v;
}

/* Test 6: static local inside block scope */
int block_static(void) {
    int r;
    r = 0;
    {
        static int inner = 5;
        inner = inner + 1;
        r = inner;
    }
    return r;
}

/* Test 7: mix of static and non-static locals */
int mixed(int x) {
    static int total = 0;
    int temp;
    temp = x * 2;
    total = total + temp;
    return total;
}

int main(void) {
    int r;
    int fail;
    fail = 0;

    /* Test 1: basic counter - call twice, verify 1 then 2 */
    r = counter();
    if (r != 1) { my_puts("FAIL: counter() first call\n"); fail = fail + 1; }
    r = counter();
    if (r != 2) { my_puts("FAIL: counter() second call\n"); fail = fail + 1; }
    r = counter();
    if (r != 3) { my_puts("FAIL: counter() third call\n"); fail = fail + 1; }

    /* Test 2: non-zero initializer */
    r = get_val();
    if (r != 42) { my_puts("FAIL: get_val() != 42\n"); fail = fail + 1; }

    /* Test 3: zero-init */
    r = get_zero();
    if (r != 0) { my_puts("FAIL: get_zero() != 0\n"); fail = fail + 1; }

    /* Test 4: multiple static locals */
    r = multi();
    if (r != 33) { my_puts("FAIL: multi() first call\n"); fail = fail + 1; }
    r = multi();
    if (r != 36) { my_puts("FAIL: multi() second call\n"); fail = fail + 1; }

    /* Test 5: independent static locals in different functions */
    r = inc_a();
    if (r != 1) { my_puts("FAIL: inc_a() first\n"); fail = fail + 1; }
    r = inc_b();
    if (r != 10) { my_puts("FAIL: inc_b() first\n"); fail = fail + 1; }
    r = inc_a();
    if (r != 2) { my_puts("FAIL: inc_a() second\n"); fail = fail + 1; }
    r = inc_b();
    if (r != 20) { my_puts("FAIL: inc_b() second\n"); fail = fail + 1; }

    /* Test 6: static local inside block scope */
    r = block_static();
    if (r != 6) { my_puts("FAIL: block_static() first\n"); fail = fail + 1; }
    r = block_static();
    if (r != 7) { my_puts("FAIL: block_static() second\n"); fail = fail + 1; }

    /* Test 7: mix of static and non-static */
    r = mixed(3);
    if (r != 6) { my_puts("FAIL: mixed(3) first\n"); fail = fail + 1; }
    r = mixed(5);
    if (r != 16) { my_puts("FAIL: mixed(5) second\n"); fail = fail + 1; }
    r = mixed(1);
    if (r != 18) { my_puts("FAIL: mixed(1) third\n"); fail = fail + 1; }

    if (fail == 0) {
        my_puts("All static local tests passed!\n");
    }

    return fail;
}
