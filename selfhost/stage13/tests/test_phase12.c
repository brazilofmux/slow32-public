/* Phase 12 test: optimization passes + register allocation */

int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

int puts(char *s) {
    int len;
    len = my_strlen(s);
    write(1, s, len);
    return 0;
}

/* --- Test 1: constant folding arithmetic --- */

int test_fold_arith(void) {
    int x;
    /* These should be folded to constants at compile time */
    x = 3 + 4;
    if (x != 7) return 1;

    x = 10 - 3;
    if (x != 7) return 2;

    x = 6 * 7;
    if (x != 42) return 3;

    x = 100 / 10;
    if (x != 10) return 4;

    x = 17 % 5;
    if (x != 2) return 5;

    return 0;
}

/* --- Test 2: constant folding comparisons --- */

int test_fold_cmp(void) {
    int x;

    x = (5 == 5);
    if (x != 1) return 1;

    x = (5 != 5);
    if (x != 0) return 2;

    x = (3 < 5);
    if (x != 1) return 3;

    x = (5 > 3);
    if (x != 1) return 4;

    x = (3 <= 3);
    if (x != 1) return 5;

    x = (3 >= 4);
    if (x != 0) return 6;

    return 0;
}

/* --- Test 3: constant folding bitwise --- */

int test_fold_bitwise(void) {
    int x;

    x = 0xFF & 0x0F;
    if (x != 15) return 1;

    x = 0xF0 | 0x0F;
    if (x != 255) return 2;

    x = 0xFF ^ 0x0F;
    if (x != 240) return 3;

    x = 1 << 4;
    if (x != 16) return 4;

    x = 256 >> 4;
    if (x != 16) return 5;

    return 0;
}

/* --- Test 4: constant folding unary --- */

int test_fold_unary(void) {
    int x;

    x = -42;
    if (x != 0 - 42) return 1;

    x = !0;
    if (x != 1) return 2;

    x = !1;
    if (x != 0) return 3;

    return 0;
}

/* --- Test 5: strength reduction (multiply by power of 2) --- */

int test_strength(void) {
    int x;
    int y;

    y = 7;
    x = y * 2;
    if (x != 14) return 1;

    x = y * 4;
    if (x != 28) return 2;

    x = y * 8;
    if (x != 56) return 3;

    x = y * 16;
    if (x != 112) return 4;

    /* Multiply by non-power-of-2 should still work */
    x = y * 3;
    if (x != 21) return 5;

    /* Commutative: 4 * y */
    x = 4 * y;
    if (x != 28) return 6;

    return 0;
}

/* --- Test 6: DCE - if(0) eliminated --- */

int test_dce_if0(void) {
    int x;

    x = 10;
    if (0) {
        x = 99;
    }
    if (x != 10) return 1;

    /* if(0) A else B → B */
    if (0) {
        x = 88;
    } else {
        x = 42;
    }
    if (x != 42) return 2;

    return 0;
}

/* --- Test 7: DCE - if(1) reduced --- */

int test_dce_if1(void) {
    int x;

    x = 0;
    if (1) {
        x = 55;
    }
    if (x != 55) return 1;

    /* if(nonzero) A else B → A */
    if (1) {
        x = 77;
    } else {
        x = 88;
    }
    if (x != 77) return 2;

    return 0;
}

/* --- Test 8: DCE - while(0) eliminated --- */

int test_dce_while0(void) {
    int x;

    x = 10;
    while (0) {
        x = 99;
    }
    if (x != 10) return 1;

    return 0;
}

/* --- Test 9: register alloc - nested expressions --- */

int test_regalloc_nested(void) {
    int a;
    int b;
    int c;
    int d;
    int x;

    a = 2;
    b = 3;
    c = 5;
    d = 7;

    /* Nested binary ops — all should use registers (no calls) */
    x = (a + b) * (c - d);
    if (x != -10) return 1;

    x = a * b + c * d;
    if (x != 41) return 2;

    /* Deeply nested */
    x = ((a + b) * c + d) * 2;
    if (x != 64) return 3;

    return 0;
}

/* --- Test 10: register alloc with function calls --- */

int identity(int x) {
    return x;
}

int add(int a, int b) {
    return a + b;
}

int test_regalloc_call(void) {
    int x;
    int y;

    /* RHS has no call — should use register */
    x = 10;
    y = x + 5;
    if (y != 15) return 1;

    /* RHS has a call — must use memory push */
    y = x + identity(3);
    if (y != 13) return 2;

    /* Mixed */
    y = add(2, 3) + x;
    if (y != 15) return 3;

    return 0;
}

/* --- Test 11: compound assignment with register alloc --- */

int test_comp_assign(void) {
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

    return 0;
}

/* --- Test 12: postfix inc/dec with register alloc --- */

int test_postfix(void) {
    int x;
    int y;

    x = 10;
    y = x++;
    if (y != 10) return 1;
    if (x != 11) return 2;

    y = x--;
    if (y != 11) return 3;
    if (x != 10) return 4;

    return 0;
}

/* --- Test 13: pointer arithmetic with register alloc --- */

int test_ptr_arith(void) {
    int arr[4];
    int *p;

    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;

    p = arr;
    if (*(p + 2) != 30) return 1;

    p = p + 1;
    if (*p != 20) return 2;

    p++;
    if (*p != 30) return 3;

    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_fold_arith();
    if (rc) return rc;

    rc = test_fold_cmp();
    if (rc) return rc + 10;

    rc = test_fold_bitwise();
    if (rc) return rc + 20;

    rc = test_fold_unary();
    if (rc) return rc + 30;

    rc = test_strength();
    if (rc) return rc + 40;

    rc = test_dce_if0();
    if (rc) return rc + 50;

    rc = test_dce_if1();
    if (rc) return rc + 60;

    rc = test_dce_while0();
    if (rc) return rc + 70;

    rc = test_regalloc_nested();
    if (rc) return rc + 80;

    rc = test_regalloc_call();
    if (rc) return rc + 90;

    rc = test_comp_assign();
    if (rc) return rc + 100;

    rc = test_postfix();
    if (rc) return rc + 110;

    rc = test_ptr_arith();
    if (rc) return rc + 120;

    puts("Phase 12: all tests passed\n");
    return 0;
}
