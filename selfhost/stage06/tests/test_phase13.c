/* Phase 13 test: unsigned type tracking + semantic analysis */

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

/* --- Test 1: unsigned comparison (> 0x7FFFFFFF) --- */

int test_unsigned_cmp(void) {
    unsigned int a;
    unsigned int b;

    a = 0 - 1;  /* 0xFFFFFFFF = 4294967295 */
    b = 0;

    /* (unsigned)(0-1) > 0 must be true */
    if (!(a > b)) return 1;

    /* (unsigned)(0-1) < 1 must be false */
    if (a < 1) return 2;

    /* unsigned 5 > unsigned 3 */
    a = 5;
    b = 3;
    if (!(a > b)) return 3;

    /* unsigned 3 < unsigned 5 */
    if (!(b < a)) return 4;

    /* unsigned 5 >= unsigned 5 */
    a = 5;
    b = 5;
    if (!(a >= b)) return 5;

    /* unsigned 5 <= unsigned 5 */
    if (!(a <= b)) return 6;

    return 0;
}

/* --- Test 2: unsigned right shift (logical, not arithmetic) --- */

int test_unsigned_rshift(void) {
    unsigned int x;
    int y;

    /* unsigned: (0-1) >> 1 should be 0x7FFFFFFF (logical shift) */
    x = 0 - 1;  /* 0xFFFFFFFF */
    x = x >> 1;
    if (x != 0x7FFFFFFF) return 1;

    /* signed: -1 >> 1 should stay -1 (arithmetic shift) */
    y = 0 - 1;
    y = y >> 1;
    if (y != -1) return 2;

    /* unsigned shift by more */
    x = 0 - 1;
    x = x >> 4;
    /* 0xFFFFFFFF >> 4 = 0x0FFFFFFF */
    if (x != 0x0FFFFFFF) return 3;

    return 0;
}

/* --- Test 3: unsigned char load (ldbu vs ldb) --- */

int test_unsigned_char(void) {
    unsigned char c;

    c = 200;
    /* unsigned char 200 should stay 200, not become -56 */
    if (c != 200) return 1;

    c = 255;
    if (c != 255) return 2;

    c = 128;
    if (c != 128) return 3;

    /* signed char 200 wraps to -56 */
    /* (we just test unsigned works, signed is existing behavior) */

    return 0;
}

/* --- Test 4: mixed unsigned/signed promotion --- */

int test_mixed_promotion(void) {
    unsigned int u;
    int s;
    int result;

    u = 10;
    s = 5;

    /* When mixing unsigned and signed in arithmetic, result is unsigned */
    /* u + s: both become unsigned, result should be 15 */
    result = u + s;
    if (result != 15) return 1;

    /* u - s = 5 (unsigned) */
    result = u - s;
    if (result != 5) return 2;

    /* u * s */
    result = u * s;
    if (result != 50) return 3;

    return 0;
}

/* --- Test 5: unsigned compound assignment --- */

int test_unsigned_compound(void) {
    unsigned int x;

    x = 10;
    x += 5;
    if (x != 15) return 1;

    x -= 3;
    if (x != 12) return 2;

    x >>= 1;
    if (x != 6) return 3;

    x <<= 2;
    if (x != 24) return 4;

    return 0;
}

/* --- Test 6: unsigned postfix increment --- */

int test_unsigned_postfix(void) {
    unsigned int x;
    unsigned int y;

    x = 10;
    y = x++;
    if (y != 10) return 1;
    if (x != 11) return 2;

    y = x--;
    if (y != 11) return 3;
    if (x != 10) return 4;

    return 0;
}

/* --- Test 7: unsigned function return --- */

unsigned int get_max_uint(void) {
    return 0 - 1;
}

int test_unsigned_return(void) {
    unsigned int v;
    v = get_max_uint();
    if (v == 0) return 1;
    if (!(v > 100)) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_unsigned_cmp();
    if (rc) return rc;

    rc = test_unsigned_rshift();
    if (rc) return rc + 10;

    rc = test_unsigned_char();
    if (rc) return rc + 20;

    rc = test_mixed_promotion();
    if (rc) return rc + 30;

    rc = test_unsigned_compound();
    if (rc) return rc + 40;

    rc = test_unsigned_postfix();
    if (rc) return rc + 50;

    rc = test_unsigned_return();
    if (rc) return rc + 60;

    my_puts("Phase 13: all tests passed\n");
    return 0;
}
