/* Test: short (16-bit) type support */

int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) n = n + 1;
    return n;
}

void my_puts(char *s) {
    write(1, s, my_strlen(s));
}

void print_ok(char *name) {
    my_puts("  OK: ");
    my_puts(name);
    my_puts("\n");
}

void print_fail(char *name, int got, int expect) {
    my_puts("  FAIL: ");
    my_puts(name);
    my_puts("\n");
}

/* Test 1: sizeof(short) == 2 */
int test_sizeof(void) {
    if (sizeof(short) != 2) return 1;
    if (sizeof(unsigned short) != 2) return 2;
    return 0;
}

/* Test 2: short variable assignment and retrieval */
int test_basic(void) {
    short x;
    x = 42;
    if (x != 42) return 1;
    x = 0;
    if (x != 0) return 2;
    x = 1000;
    if (x != 1000) return 3;
    return 0;
}

/* Test 3: truncation — assign value > 16 bits, read back lower 16 */
int test_truncation(void) {
    short x;
    x = 0x2345;
    if (x != 0x2345) return 1;
    return 0;
}

/* Test 4: sign extension — assign -1 to short, compare to -1 as int */
int test_sign_ext(void) {
    short x;
    int y;
    x = -1;
    y = x;
    if (y != -1) return 1;
    x = -100;
    y = x;
    if (y != -100) return 2;
    return 0;
}

/* Test 5: unsigned short — 0xFFFF should be 65535, not -1 */
int test_unsigned(void) {
    unsigned short x;
    int y;
    x = 65535;
    y = x;
    if (y != 65535) return 1;
    x = 0;
    y = x;
    if (y != 0) return 2;
    return 0;
}

/* Test 6: short array — pointer arithmetic steps by 2 */
int test_array(void) {
    short arr[4];
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    if (arr[0] != 10) return 1;
    if (arr[1] != 20) return 2;
    if (arr[2] != 30) return 3;
    if (arr[3] != 40) return 4;
    return 0;
}

/* Test 7: struct with short member */
struct ShortStruct {
    short a;
    short b;
};

int test_struct(void) {
    struct ShortStruct s;
    s.a = 100;
    s.b = 200;
    if (s.a != 100) return 1;
    if (s.b != 200) return 2;
    if (sizeof(struct ShortStruct) != 4) return 3;
    return 0;
}

/* Test 8: struct with mixed char/short/int — verify padding */
struct MixedStruct {
    char c;
    short s;
    int i;
};

int test_mixed_struct(void) {
    struct MixedStruct m;
    m.c = 65;
    m.s = 1234;
    m.i = 56789;
    if (m.c != 65) return 1;
    if (m.s != 1234) return 2;
    if (m.i != 56789) return 3;
    /* char at 0, short at 2 (aligned), int at 4 (aligned), total = 8 */
    if (sizeof(struct MixedStruct) != 8) return 4;
    return 0;
}

/* Test 9: short pointer dereference */
int test_ptr(void) {
    short x;
    short *p;
    x = 777;
    p = &x;
    if (*p != 777) return 1;
    *p = 888;
    if (x != 888) return 2;
    return 0;
}

int main(void) {
    int fail;
    fail = 0;

    if (test_sizeof() == 0) { print_ok("sizeof(short)"); }
    else { print_fail("sizeof(short)", 0, 0); fail = 1; }

    if (test_basic() == 0) { print_ok("basic assign"); }
    else { print_fail("basic assign", 0, 0); fail = 1; }

    if (test_truncation() == 0) { print_ok("truncation"); }
    else { print_fail("truncation", 0, 0); fail = 1; }

    if (test_sign_ext() == 0) { print_ok("sign extension"); }
    else { print_fail("sign extension", 0, 0); fail = 1; }

    if (test_unsigned() == 0) { print_ok("unsigned short"); }
    else { print_fail("unsigned short", 0, 0); fail = 1; }

    if (test_array() == 0) { print_ok("short array"); }
    else { print_fail("short array", 0, 0); fail = 1; }

    if (test_struct() == 0) { print_ok("short struct"); }
    else { print_fail("short struct", 0, 0); fail = 1; }

    if (test_mixed_struct() == 0) { print_ok("mixed struct"); }
    else { print_fail("mixed struct", 0, 0); fail = 1; }

    if (test_ptr() == 0) { print_ok("short pointer"); }
    else { print_fail("short pointer", 0, 0); fail = 1; }

    return fail;
}
