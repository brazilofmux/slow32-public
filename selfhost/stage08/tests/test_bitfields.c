/* Test: C99 bit-fields — packing, load/store, RMW, sign-extension */

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

void print_fail(char *name) {
    my_puts("  FAIL: ");
    my_puts(name);
    my_puts("\n");
}

/* --- Test 1: unsigned int bit-fields pack into one 4-byte unit --- */
struct Packed32 {
    unsigned int a : 1;
    unsigned int b : 7;
    unsigned int c : 24;
};

int test_packed32(void) {
    struct Packed32 s;
    s.a = 1;
    s.b = 0x55;
    s.c = 0xABCDEF;
    if (s.a != 1) return 1;
    if (s.b != 0x55) return 2;
    if (s.c != 0xABCDEF) return 3;
    if (sizeof(struct Packed32) != 4) return 4;
    /* RMW: write a, b and c must be unchanged */
    s.a = 0;
    if (s.a != 0) return 5;
    if (s.b != 0x55) return 6;
    if (s.c != 0xABCDEF) return 7;
    return 0;
}

/* --- Test 2: unsigned char bit-fields pack into one byte --- */
struct PackedByte {
    unsigned char x : 3;
    unsigned char y : 5;
};

int test_packed_byte(void) {
    struct PackedByte s;
    s.x = 5;
    s.y = 17;
    if (s.x != 5) return 1;
    if (s.y != 17) return 2;
    /* RMW preserves neighbor */
    s.x = 2;
    if (s.x != 2) return 3;
    if (s.y != 17) return 4;
    return 0;
}

/* --- Test 3: signed bit-fields sign-extend --- */
struct Signed {
    int a : 4;     /* -8..7 */
    int b : 12;    /* -2048..2047 */
};

int test_signed(void) {
    struct Signed s;
    s.a = -1;
    s.b = -1000;
    if (s.a != -1) return 1;
    if (s.b != -1000) return 2;
    s.a = 7;
    if (s.a != 7) return 3;
    s.a = -8;
    if (s.a != -8) return 4;
    /* 0xF stored into 4-bit signed field reads back as -1 */
    s.a = 0xF;
    if (s.a != -1) return 5;
    return 0;
}

/* --- Test 4: overflow into next storage unit --- */
struct Cross {
    unsigned int a : 20;
    unsigned int b : 20;   /* won't fit with a; new unit */
};

int test_cross_unit(void) {
    struct Cross s;
    s.a = 0xAAAAA;
    s.b = 0x55555;
    if (s.a != 0xAAAAA) return 1;
    if (s.b != 0x55555) return 2;
    /* two units of 4 bytes each */
    if (sizeof(struct Cross) != 8) return 3;
    return 0;
}

/* --- Test 5: :0 forces a new storage unit --- */
struct Zero {
    unsigned int a : 3;
    unsigned int   : 0;   /* flush */
    unsigned int b : 5;
};

int test_zero_width(void) {
    struct Zero s;
    s.a = 5;
    s.b = 17;
    if (s.a != 5) return 1;
    if (s.b != 17) return 2;
    if (sizeof(struct Zero) != 8) return 3;
    return 0;
}

/* --- Test 6: anonymous padding bit-field --- */
struct Anon {
    unsigned int a : 4;
    unsigned int   : 4;   /* padding */
    unsigned int b : 4;
};

int test_anon_padding(void) {
    struct Anon s;
    s.a = 5;
    s.b = 9;
    if (s.a != 5) return 1;
    if (s.b != 9) return 2;
    /* All fits in one int unit; verify b is at bit 8, not bit 4. */
    /* By writing 0xFF into the underlying word indirectly via b=15,
     * then reading a, we'd test layout, but without &s.bit we just
     * check that a and b don't trample each other. */
    s.a = 0xF;
    if (s.b != 9) return 3;
    s.b = 0xF;
    if (s.a != 0xF) return 4;
    return 0;
}

/* --- Test 7: bit-field after non-bit-field (mixed) --- */
struct Mixed {
    int x;
    unsigned int a : 3;
    unsigned int b : 5;
    int y;
};

int test_mixed(void) {
    struct Mixed s;
    s.x = 100;
    s.a = 6;
    s.b = 30;
    s.y = 200;
    if (s.x != 100) return 1;
    if (s.a != 6) return 2;
    if (s.b != 30) return 3;
    if (s.y != 200) return 4;
    return 0;
}

/* --- Test 8: comma-separated bit-field declarators --- */
struct Comma {
    unsigned int p : 1, q : 7, r : 8;
};

int test_comma(void) {
    struct Comma s;
    s.p = 1;
    s.q = 100;
    s.r = 200;
    if (s.p != 1) return 1;
    if (s.q != 100) return 2;
    if (s.r != 200) return 3;
    return 0;
}

/* --- Test 9: compound assignment and ++ on bit-fields --- */
struct Counter {
    unsigned int n : 4;
};

int test_compound(void) {
    struct Counter c;
    c.n = 0;
    c.n = c.n + 1;
    if (c.n != 1) return 1;
    c.n += 3;
    if (c.n != 4) return 2;
    c.n++;
    if (c.n != 5) return 3;
    c.n -= 2;
    if (c.n != 3) return 4;
    /* wrap on 4 bits */
    c.n = 15;
    c.n++;
    if (c.n != 0) return 5;
    return 0;
}

/* --- Test 10: array of struct with bit-fields --- */
struct Slot {
    unsigned int valid : 1;
    unsigned int tag   : 7;
};

int test_array_of_struct(void) {
    struct Slot arr[3];
    int i;
    i = 0;
    while (i < 3) {
        arr[i].valid = 1;
        arr[i].tag = i + 1;
        i = i + 1;
    }
    if (arr[0].valid != 1) return 1;
    if (arr[1].valid != 1) return 2;
    if (arr[2].valid != 1) return 3;
    if (arr[0].tag != 1) return 4;
    if (arr[1].tag != 2) return 5;
    if (arr[2].tag != 3) return 6;
    return 0;
}

/* --- Test 11: union with bit-fields --- */
union UB {
    unsigned int as_int;
    struct {
        unsigned int lo : 16;
        unsigned int hi : 16;
    } parts;
};

int test_union(void) {
    union UB u;
    u.as_int = 0x12345678;
    if (u.parts.lo != 0x5678) return 1;
    if (u.parts.hi != 0x1234) return 2;
    u.parts.lo = 0xBEEF;
    if (u.parts.lo != 0xBEEF) return 3;
    if (u.parts.hi != 0x1234) return 4;
    return 0;
}

int main(void) {
    int fail;
    fail = 0;

    if (test_packed32() == 0) print_ok("packed uint:1+7+24");
    else { print_fail("packed uint:1+7+24"); fail = 1; }

    if (test_packed_byte() == 0) print_ok("packed uchar:3+5");
    else { print_fail("packed uchar:3+5"); fail = 1; }

    if (test_signed() == 0) print_ok("signed bit-fields");
    else { print_fail("signed bit-fields"); fail = 1; }

    if (test_cross_unit() == 0) print_ok("cross-unit overflow");
    else { print_fail("cross-unit overflow"); fail = 1; }

    if (test_zero_width() == 0) print_ok(":0 flush");
    else { print_fail(":0 flush"); fail = 1; }

    if (test_anon_padding() == 0) print_ok("anonymous padding");
    else { print_fail("anonymous padding"); fail = 1; }

    if (test_mixed() == 0) print_ok("mixed regular + bit-field");
    else { print_fail("mixed regular + bit-field"); fail = 1; }

    if (test_comma() == 0) print_ok("comma declarators");
    else { print_fail("comma declarators"); fail = 1; }

    if (test_compound() == 0) print_ok("compound assign / ++");
    else { print_fail("compound assign / ++"); fail = 1; }

    if (test_array_of_struct() == 0) print_ok("array of struct");
    else { print_fail("array of struct"); fail = 1; }

    if (test_union() == 0) print_ok("union with bit-fields");
    else { print_fail("union with bit-fields"); fail = 1; }

    return fail;
}
