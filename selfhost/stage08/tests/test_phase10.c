/* Phase 10 test: block-scoped declarations, for-loop init declarations */

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

/* --- Test 1: basic block-scoped declaration --- */

int test_block_scope(void) {
    int x;
    x = 10;
    {
        int y;
        y = 20;
        x = x + y;
    }
    /* y is no longer visible, x should be 30 */
    if (x != 30) return 1;
    return 0;
}

/* --- Test 2: block-scoped with initializer --- */

int test_block_init(void) {
    int result;
    result = 0;
    {
        int a = 5;
        int b = 7;
        result = a + b;
    }
    if (result != 12) return 1;
    return 0;
}

/* --- Test 3: shadowing in inner block --- */

int test_shadow(void) {
    int x;
    x = 100;
    {
        int x;
        x = 42;
        if (x != 42) return 1;
    }
    /* outer x should still be 100 */
    if (x != 100) return 2;
    return 0;
}

/* --- Test 4: nested blocks --- */

int test_nested_blocks(void) {
    int sum;
    sum = 0;
    {
        int a = 1;
        sum = sum + a;
        {
            int b = 2;
            sum = sum + b;
            {
                int c = 3;
                sum = sum + c;
            }
        }
    }
    /* sum = 1 + 2 + 3 = 6 */
    if (sum != 6) return 1;
    return 0;
}

/* --- Test 5: block inside if --- */

int test_if_block(void) {
    int result;
    result = 0;
    if (1) {
        int temp = 42;
        result = temp;
    }
    if (result != 42) return 1;
    return 0;
}

/* --- Test 6: block inside while --- */

int test_while_block(void) {
    int sum;
    int i;
    sum = 0;
    i = 0;
    while (i < 5) {
        int val = i * 2;
        sum = sum + val;
        i = i + 1;
    }
    /* sum = 0 + 2 + 4 + 6 + 8 = 20 */
    if (sum != 20) return 1;
    return 0;
}

/* --- Test 7: for-loop init declaration --- */

int test_for_init(void) {
    int sum;
    sum = 0;
    for (int i = 1; i <= 5; i = i + 1) {
        sum = sum + i;
    }
    /* sum = 1+2+3+4+5 = 15 */
    if (sum != 15) return 1;
    return 0;
}

/* --- Test 8: for-loop var scoped to loop --- */

int test_for_scope(void) {
    int x;
    x = 99;
    for (int i = 0; i < 3; i = i + 1) {
        x = x + 1;
    }
    /* i should not be visible here; x = 102 */
    if (x != 102) return 1;
    return 0;
}

/* --- Test 9: multiple for loops with same var name --- */

int test_for_reuse(void) {
    int sum;
    sum = 0;
    for (int i = 0; i < 3; i = i + 1) {
        sum = sum + i;
    }
    for (int i = 10; i < 13; i = i + 1) {
        sum = sum + i;
    }
    /* sum = (0+1+2) + (10+11+12) = 3 + 33 = 36 */
    if (sum != 36) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_block_scope();
    if (rc) return rc;

    rc = test_block_init();
    if (rc) return rc + 10;

    rc = test_shadow();
    if (rc) return rc + 20;

    rc = test_nested_blocks();
    if (rc) return rc + 30;

    rc = test_if_block();
    if (rc) return rc + 40;

    rc = test_while_block();
    if (rc) return rc + 50;

    rc = test_for_init();
    if (rc) return rc + 60;

    rc = test_for_scope();
    if (rc) return rc + 70;

    rc = test_for_reuse();
    if (rc) return rc + 80;

    my_puts("Phase 10: all tests passed\n");
    return 0;
}
