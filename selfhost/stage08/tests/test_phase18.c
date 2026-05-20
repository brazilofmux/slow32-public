/* Phase 18 test: array size inference from brace initializers */

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

/* --- Global int array with inferred size --- */
int ga1[] = {10, 20, 30};

int test_global_int_inferred(void) {
    if (ga1[0] != 10) return 1;
    if (ga1[1] != 20) return 2;
    if (ga1[2] != 30) return 3;
    return 0;
}

/* --- Global int array with inferred size, single element --- */
int ga2[] = {42};

int test_global_single(void) {
    if (ga2[0] != 42) return 1;
    return 0;
}

/* --- Global int array with inferred size, negative values --- */
int ga3[] = {-1, 0, 1, -100};

int test_global_negative(void) {
    if (ga3[0] != -1) return 1;
    if (ga3[1] != 0) return 2;
    if (ga3[2] != 1) return 3;
    if (ga3[3] != -100) return 4;
    return 0;
}

/* --- Global char array with inferred size (brace init) --- */
char gc1[] = {65, 66, 67, 0};

int test_global_char_inferred(void) {
    if (gc1[0] != 65) return 1;  /* 'A' */
    if (gc1[1] != 66) return 2;  /* 'B' */
    if (gc1[2] != 67) return 3;  /* 'C' */
    if (gc1[3] != 0) return 4;
    return 0;
}

/* --- Regression: explicit size still works --- */
int ga4[3] = {100, 200, 300};

int test_global_explicit(void) {
    if (ga4[0] != 100) return 1;
    if (ga4[1] != 200) return 2;
    if (ga4[2] != 300) return 3;
    return 0;
}

/* --- Local int array with inferred size --- */
int test_local_int_inferred(void) {
    int a[] = {5, 10, 15, 20};
    if (a[0] != 5) return 1;
    if (a[1] != 10) return 2;
    if (a[2] != 15) return 3;
    if (a[3] != 20) return 4;
    return 0;
}

/* --- Local int array with inferred size, single element --- */
int test_local_single(void) {
    int a[] = {99};
    if (a[0] != 99) return 1;
    return 0;
}

/* --- Local char array with inferred size (brace init) --- */
int test_local_char_inferred(void) {
    char c[] = {72, 73, 0};
    if (c[0] != 72) return 1;  /* 'H' */
    if (c[1] != 73) return 2;  /* 'I' */
    if (c[2] != 0) return 3;
    return 0;
}

/* --- Local array with expressions --- */
int test_local_expr(void) {
    int x;
    int a[] = {1 + 2, 4 * 5, 100 - 1};
    x = 0;
    if (a[0] != 3) return 1;
    if (a[1] != 20) return 2;
    if (a[2] != 99) return 3;
    return 0;
}

/* --- Regression: local explicit size still works --- */
int test_local_explicit(void) {
    int a[3] = {7, 8, 9};
    if (a[0] != 7) return 1;
    if (a[1] != 8) return 2;
    if (a[2] != 9) return 3;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_global_int_inferred();
    if (rc) return rc;

    rc = test_global_single();
    if (rc) return rc + 10;

    rc = test_global_negative();
    if (rc) return rc + 20;

    rc = test_global_char_inferred();
    if (rc) return rc + 30;

    rc = test_global_explicit();
    if (rc) return rc + 40;

    rc = test_local_int_inferred();
    if (rc) return rc + 50;

    rc = test_local_single();
    if (rc) return rc + 60;

    rc = test_local_char_inferred();
    if (rc) return rc + 70;

    rc = test_local_expr();
    if (rc) return rc + 80;

    rc = test_local_explicit();
    if (rc) return rc + 90;

    my_puts("Phase 18: all tests passed\n");
    return 0;
}
