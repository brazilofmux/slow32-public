/* Phase 3 test: operators and control flow */

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

/* --- Test: shift operators --- */
int test_shift(void) {
    int x;
    x = 1 << 3;
    if (x != 8) return 1;
    x = 256 >> 4;
    if (x != 16) return 2;
    /* Shift binds tighter than relational, looser than additive */
    x = 1 + (1 << 2);
    if (x != 5) return 3;
    return 0;
}

/* --- Test: do/while --- */
int test_do_while(void) {
    int x;
    int count;
    x = 0;
    count = 0;
    do {
        x = x + 1;
        count = count + 1;
    } while (x < 5);
    if (x != 5) return 1;
    if (count != 5) return 2;
    /* do/while always executes at least once */
    x = 100;
    do {
        x = x + 1;
    } while (x < 5);
    if (x != 101) return 3;
    return 0;
}

/* --- Test: break --- */
int test_break(void) {
    int i;
    i = 0;
    while (1) {
        if (i >= 10) break;
        i = i + 1;
    }
    if (i != 10) return 1;
    return 0;
}

/* --- Test: continue --- */
int test_continue(void) {
    int i;
    int sum;
    sum = 0;
    i = 0;
    while (i < 10) {
        i = i + 1;
        if (i % 2 == 0) continue;
        sum = sum + i;
    }
    /* sum of odd numbers 1-9: 1+3+5+7+9 = 25 */
    if (sum != 25) return 1;
    return 0;
}

/* --- Test: for loop with continue (broken with desugaring) --- */
int test_for_continue(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < 10; i = i + 1) {
        if (i % 2 == 0) continue;
        sum = sum + i;
    }
    /* sum of odd numbers 1-9: 1+3+5+7+9 = 25 */
    if (sum != 25) return 1;
    return 0;
}

/* --- Test: compound assignment --- */
int test_compound_assign(void) {
    int x;
    x = 10;
    x += 5;
    if (x != 15) return 1;
    x -= 3;
    if (x != 12) return 2;
    x *= 2;
    if (x != 24) return 3;
    x /= 4;
    if (x != 6) return 4;
    x %= 4;
    if (x != 2) return 5;
    x = 255;
    x &= 15;
    if (x != 15) return 6;
    x |= 240;
    if (x != 255) return 7;
    x ^= 170;
    if (x != 85) return 8;
    x = 1;
    x <<= 4;
    if (x != 16) return 9;
    x >>= 2;
    if (x != 4) return 10;
    return 0;
}

/* --- Test: postfix ++/-- --- */
int test_postfix(void) {
    int x;
    int y;
    x = 5;
    y = x++;
    if (y != 5) return 1;
    if (x != 6) return 2;
    y = x--;
    if (y != 6) return 3;
    if (x != 5) return 4;
    return 0;
}

/* --- Test: prefix ++/-- --- */
int test_prefix(void) {
    int x;
    int y;
    x = 5;
    y = ++x;
    if (y != 6) return 1;
    if (x != 6) return 2;
    y = --x;
    if (y != 5) return 3;
    if (x != 5) return 4;
    return 0;
}

/* --- Test: ternary ?: --- */
int test_ternary(void) {
    int x;
    int y;
    x = 1;
    y = x ? 42 : 99;
    if (y != 42) return 1;
    x = 0;
    y = x ? 42 : 99;
    if (y != 99) return 2;
    /* Nested ternary */
    x = 2;
    y = (x == 1) ? 10 : (x == 2) ? 20 : 30;
    if (y != 20) return 3;
    return 0;
}

/* --- Test: bitwise NOT ~ --- */
int test_bitnot(void) {
    int x;
    x = ~0;
    if (x != -1) return 1;
    x = ~255;
    if (x != -256) return 2;
    return 0;
}

/* --- Test: type cast --- */
int test_cast(void) {
    int x;
    char *p;
    x = 12345;
    p = (char *)x;
    if ((int)p != 12345) return 1;
    return 0;
}

/* --- Test: comma operator --- */
int test_comma(void) {
    int x;
    x = (1, 2, 3);
    if (x != 3) return 1;
    return 0;
}

/* --- Test: break in do/while --- */
int test_do_break(void) {
    int i;
    i = 0;
    do {
        i = i + 1;
        if (i == 3) break;
    } while (i < 100);
    if (i != 3) return 1;
    return 0;
}

/* --- Test: break in for --- */
int test_for_break(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < 100; i = i + 1) {
        if (i == 5) break;
        sum = sum + i;
    }
    /* 0+1+2+3+4 = 10 */
    if (sum != 10) return 1;
    return 0;
}

/* --- Test: postfix ++ in for step --- */
int test_for_postfix(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < 5; i++) {
        sum = sum + i;
    }
    /* 0+1+2+3+4 = 10 */
    if (sum != 10) return 1;
    if (i != 5) return 2;
    return 0;
}

/* --- Test: compound assign result value --- */
int test_compound_result(void) {
    int x;
    int y;
    x = 10;
    y = (x += 5);
    if (y != 15) return 1;
    if (x != 15) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_shift();
    if (rc) return rc;

    rc = test_do_while();
    if (rc) return rc + 10;

    rc = test_break();
    if (rc) return rc + 20;

    rc = test_continue();
    if (rc) return rc + 30;

    rc = test_for_continue();
    if (rc) return rc + 40;

    rc = test_compound_assign();
    if (rc) return rc + 50;

    rc = test_postfix();
    if (rc) return rc + 60;

    rc = test_prefix();
    if (rc) return rc + 70;

    rc = test_ternary();
    if (rc) return rc + 80;

    rc = test_bitnot();
    if (rc) return rc + 90;

    rc = test_cast();
    if (rc) return rc + 100;

    rc = test_comma();
    if (rc) return rc + 110;

    rc = test_do_break();
    if (rc) return rc + 120;

    rc = test_for_break();
    if (rc) return rc + 130;

    rc = test_for_postfix();
    if (rc) return rc + 140;

    rc = test_compound_result();
    if (rc) return rc + 150;

    my_puts("Phase 3: all tests passed\n");
    return 0;
}
