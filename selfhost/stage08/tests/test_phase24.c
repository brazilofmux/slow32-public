/* test_phase24.c -- float type support */
int write(int fd, char *buf, int len);

int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

void my_puts(char *s) {
    write(1, s, my_strlen(s));
}

int failed;

void check(int cond, char *msg) {
    if (!cond) {
        my_puts("FAIL: ");
        my_puts(msg);
        my_puts("\n");
        failed = failed + 1;
    }
}

int main(void) {
    float a;
    float b;
    float c;
    int i;

    failed = 0;

    /* Test 1: float constant and cast to int */
    a = 3.14f;
    i = (int)a;
    check(i == 3, "float to int: (int)3.14f == 3");

    /* Test 2: float arithmetic */
    a = 10.0f;
    b = 3.0f;
    c = a + b;
    i = (int)c;
    check(i == 13, "float add: 10.0 + 3.0 == 13");

    /* Test 3: float subtraction */
    c = a - b;
    i = (int)c;
    check(i == 7, "float sub: 10.0 - 3.0 == 7");

    /* Test 4: float multiplication */
    a = 4.0f;
    b = 5.0f;
    c = a * b;
    i = (int)c;
    check(i == 20, "float mul: 4.0 * 5.0 == 20");

    /* Test 5: float division */
    a = 20.0f;
    b = 4.0f;
    c = a / b;
    i = (int)c;
    check(i == 5, "float div: 20.0 / 4.0 == 5");

    /* Test 6: int to float cast */
    i = 42;
    a = (float)i;
    i = (int)a;
    check(i == 42, "int to float to int: 42");

    /* Test 7: float comparison (less than) */
    a = 1.0f;
    b = 2.0f;
    check(a < b, "float cmp: 1.0 < 2.0");

    /* Test 8: float comparison (greater than) */
    check(b > a, "float cmp: 2.0 > 1.0");

    /* Test 9: float comparison (equal) */
    a = 5.0f;
    b = 5.0f;
    check(a == b, "float cmp: 5.0 == 5.0");

    /* Test 10: float comparison (not equal) */
    b = 6.0f;
    check(a != b, "float cmp: 5.0 != 6.0");

    /* Test 11: negative float */
    a = -1.5f;
    i = (int)a;
    check(i == -1, "float negative: (int)-1.5f == -1");

    /* Test 12: float comparison (less-equal) */
    a = 3.0f;
    b = 3.0f;
    check(a <= b, "float cmp: 3.0 <= 3.0");
    b = 4.0f;
    check(a <= b, "float cmp: 3.0 <= 4.0");

    /* Test 13: float comparison (greater-equal) */
    a = 5.0f;
    b = 5.0f;
    check(a >= b, "float cmp: 5.0 >= 5.0");
    b = 4.0f;
    check(a >= b, "float cmp: 5.0 >= 4.0");

    if (failed == 0) {
        my_puts("test_phase24: ALL PASSED\n");
    }
    return failed;
}
