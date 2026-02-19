/* Phase 11 test: global string initializers */

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

int strcmp(char *a, char *b) {
    while (*a && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return *a - *b;
}

/* --- Test 1: basic string initializer --- */

char *g_hello = "hello";

int test_basic_str(void) {
    if (g_hello[0] != 104) return 1;  /* 'h' */
    if (g_hello[4] != 111) return 2;  /* 'o' */
    if (g_hello[5] != 0) return 3;    /* null terminator */
    return 0;
}

/* --- Test 2: multiple string globals --- */

char *g_foo = "foo";
char *g_bar = "bar";

int test_multi_str(void) {
    if (strcmp(g_foo, "foo") != 0) return 1;
    if (strcmp(g_bar, "bar") != 0) return 2;
    return 0;
}

/* --- Test 3: string global used in function call --- */

char *g_msg = "Phase 11: all tests passed\n";

int test_str_call(void) {
    /* Just verify the string is valid and non-null */
    if (g_msg[0] != 80) return 1;  /* 'P' */
    return 0;
}

/* --- Test 4: string global reassignment --- */

char *g_ptr = "first";

int test_reassign(void) {
    if (strcmp(g_ptr, "first") != 0) return 1;
    g_ptr = "second";
    if (strcmp(g_ptr, "second") != 0) return 2;
    return 0;
}

/* --- Test 5: mixed int and string globals --- */

int g_num = 42;
char *g_str = "mixed";
int g_num2 = 99;

int test_mixed(void) {
    if (g_num != 42) return 1;
    if (strcmp(g_str, "mixed") != 0) return 2;
    if (g_num2 != 99) return 3;
    return 0;
}

/* --- Test 6: string global with strlen --- */

char *g_count = "abcdef";

int test_strlen(void) {
    if (my_strlen(g_count) != 6) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_basic_str();
    if (rc) return rc;

    rc = test_multi_str();
    if (rc) return rc + 10;

    rc = test_str_call();
    if (rc) return rc + 20;

    rc = test_reassign();
    if (rc) return rc + 30;

    rc = test_mixed();
    if (rc) return rc + 40;

    rc = test_strlen();
    if (rc) return rc + 50;

    puts(g_msg);
    return 0;
}
