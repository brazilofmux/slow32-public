/* Test expressions: arithmetic, comparison, bitwise, assignment, ternary */

int global_x;

int test_arith(void) {
    int a;
    int b;
    a = 10;
    b = 3;
    if (a + b != 13) return 1;
    if (a - b != 7) return 2;
    if (a * b != 30) return 3;
    if (a / b != 3) return 4;
    if (a % b != 1) return 5;
    return 0;
}

int test_compare(void) {
    if (!(5 == 5)) return 1;
    if (5 != 5) return 2;
    if (!(3 < 5)) return 3;
    if (!(5 > 3)) return 4;
    if (!(5 <= 5)) return 5;
    if (!(5 >= 5)) return 6;
    if (3 > 5) return 7;
    if (5 < 3) return 8;
    return 0;
}

int test_bitwise(void) {
    if ((0xFF & 0x0F) != 0x0F) return 1;
    if ((0xF0 | 0x0F) != 0xFF) return 2;
    if ((0xFF ^ 0x0F) != 0xF0) return 3;
    if ((1 << 4) != 16) return 4;
    if ((16 >> 2) != 4) return 5;
    return 0;
}

int test_logical(void) {
    /* Short-circuit && */
    if (!(1 && 1)) return 1;
    if (0 && 1) return 2;
    if (1 && 0) return 3;
    /* Short-circuit || */
    if (!(1 || 0)) return 4;
    if (!(0 || 1)) return 5;
    if (0 || 0) return 6;
    /* ! operator */
    if (!(!0)) return 7;
    if (!!0) return 8;
    return 0;
}

int test_unary(void) {
    int x;
    x = 5;
    if (-x != -5) return 1;
    if (~0 != -1) return 2;
    if (!0 != 1) return 3;
    return 0;
}

int test_ternary(void) {
    int a;
    a = 1 ? 42 : 99;
    if (a != 42) return 1;
    a = 0 ? 42 : 99;
    if (a != 99) return 2;
    return 0;
}

int test_assign(void) {
    int x;
    x = 10;
    x += 5;
    if (x != 15) return 1;
    x -= 3;
    if (x != 12) return 2;
    x *= 2;
    if (x != 24) return 3;
    x /= 6;
    if (x != 4) return 4;
    x %= 3;
    if (x != 1) return 5;
    x = 0xFF;
    x &= 0x0F;
    if (x != 0x0F) return 6;
    x |= 0xF0;
    if (x != 0xFF) return 7;
    x ^= 0x0F;
    if (x != 0xF0) return 8;
    x = 1;
    x <<= 4;
    if (x != 16) return 9;
    x >>= 2;
    if (x != 4) return 10;
    return 0;
}

int test_global(void) {
    global_x = 42;
    if (global_x != 42) return 1;
    global_x = global_x + 8;
    if (global_x != 50) return 2;
    return 0;
}

int test_multi_decl(void) {
    int a, b, c;
    a = 1;
    b = 2;
    c = 3;
    if (a + b + c != 6) return 1;
    return 0;
}

int main(void) {
    int rc;
    rc = test_arith();
    if (rc) return rc;
    rc = test_compare();
    if (rc) return rc + 10;
    rc = test_bitwise();
    if (rc) return rc + 20;
    rc = test_logical();
    if (rc) return rc + 30;
    rc = test_unary();
    if (rc) return rc + 40;
    rc = test_ternary();
    if (rc) return rc + 50;
    rc = test_assign();
    if (rc) return rc + 60;
    rc = test_global();
    if (rc) return rc + 80;
    rc = test_multi_decl();
    if (rc) return rc + 90;
    return 0;
}
