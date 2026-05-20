/* test_phase26.c -- -I include path, null statement, size_t typedef */
#include <stddef.h>
#include <string.h>

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

/* --- Test angle-bracket includes (stddef.h, string.h) --- */

void test_size_t(void) {
    size_t a;
    size_t b;
    a = 10;
    b = 20;
    check(a + b == 30, "size_t arithmetic: 10 + 20 == 30");

    size_t sz;
    sz = sizeof(int);
    check(sz == 4, "size_t sizeof: sizeof(int) == 4");
}

void test_null(void) {
    char *p;
    p = NULL;
    check(p == 0, "NULL == 0");
}

/* --- Test null statement --- */

void test_null_stmt(void) {
    int i;
    int sum;

    /* while with null body */
    i = 0;
    while (i < 10) i = i + 1;
    /* above is not a null stmt (has expr body), but this is: */

    /* for with null body */
    sum = 0;
    for (i = 0; i < 5; i++) sum = sum + i;
    check(sum == 10, "for loop sum: 0+1+2+3+4 == 10");

    /* Actual null statement: semicolons as statements */
    ;
    ;
    ;
    check(1, "null statements executed without error");
}

/* --- Test null stmt in while condition with side effects --- */

void test_while_null_body(void) {
    char buf[16];
    char *src;
    char *dst;

    src = "Hello";
    dst = buf;

    /* Copy string using while with assignment in condition, null body */
    while ((*dst++ = *src++)) {}

    check(buf[0] == 'H', "while copy: buf[0]=='H'");
    check(buf[4] == 'o', "while copy: buf[4]=='o'");
    check(buf[5] == 0, "while copy: buf[5]==0");
}

/* NOTE: pointer subtraction scaling (p2 - p1 / sizeof(*p)) not yet
 * implemented in s12cc — gives byte distance, not element count.
 * Separate bug to fix later. */

/* --- Test strlen from string.h prototype (links against libc) --- */

void test_strlen_proto(void) {
    size_t n;
    n = strlen("hello");
    check(n == 5, "strlen(\"hello\") == 5");
}

int main(void) {
    failed = 0;

    test_size_t();
    test_null();
    test_null_stmt();
    test_while_null_body();
    test_strlen_proto();

    if (failed == 0) {
        my_puts("test_phase26: ALL PASSED\n");
    }
    return failed;
}
