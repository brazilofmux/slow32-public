/* Phase 17 test: string array initializers and array size inference */

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

int my_strcmp(char *a, char *b) {
    while (*a != 0 && *a == *b) {
        a = a + 1;
        b = b + 1;
    }
    return *a - *b;
}

/* --- Global string array with explicit size --- */
char gs1[6] = "hello";

int test_global_string_explicit(void) {
    if (gs1[0] != 104) return 1;  /* 'h' */
    if (gs1[1] != 101) return 2;  /* 'e' */
    if (gs1[4] != 111) return 3;  /* 'o' */
    if (gs1[5] != 0) return 4;    /* null terminator */
    if (my_strcmp(gs1, "hello") != 0) return 5;
    return 0;
}

/* --- Global string array with inferred size --- */
char gs2[] = "world";

int test_global_string_inferred(void) {
    if (gs2[0] != 119) return 1;  /* 'w' */
    if (gs2[4] != 100) return 2;  /* 'd' */
    if (gs2[5] != 0) return 3;    /* null terminator */
    if (my_strcmp(gs2, "world") != 0) return 4;
    return 0;
}

/* --- Global string shorter than array (zero-fill) --- */
char gs3[10] = "hi";

int test_global_string_padded(void) {
    if (gs3[0] != 104) return 1;  /* 'h' */
    if (gs3[1] != 105) return 2;  /* 'i' */
    if (gs3[2] != 0) return 3;    /* null terminator */
    if (gs3[9] != 0) return 4;    /* zero fill */
    return 0;
}

/* --- Global empty string --- */
char gs4[] = "";

int test_global_empty_string(void) {
    if (gs4[0] != 0) return 1;
    return 0;
}

/* --- Local string array with explicit size --- */
int test_local_string_explicit(void) {
    char s[6] = "hello";
    if (s[0] != 104) return 1;
    if (s[4] != 111) return 2;
    if (s[5] != 0) return 3;
    if (my_strcmp(s, "hello") != 0) return 4;
    return 0;
}

/* --- Local string array with inferred size --- */
int test_local_string_inferred(void) {
    char s[] = "test";
    if (s[0] != 116) return 1;  /* 't' */
    if (s[3] != 116) return 2;  /* 't' */
    if (s[4] != 0) return 3;    /* null terminator */
    if (my_strcmp(s, "test") != 0) return 4;
    return 0;
}

/* --- Local string shorter than array --- */
int test_local_string_padded(void) {
    char s[8] = "ab";
    if (s[0] != 97) return 1;   /* 'a' */
    if (s[1] != 98) return 2;   /* 'b' */
    if (s[2] != 0) return 3;    /* null terminator */
    return 0;
}

/* --- Local empty string --- */
int test_local_empty_string(void) {
    char s[] = "";
    if (s[0] != 0) return 1;
    return 0;
}

/* --- String with escape sequences --- */
char gs5[] = "a\nb";

int test_escape_sequences(void) {
    if (gs5[0] != 97) return 1;   /* 'a' */
    if (gs5[1] != 10) return 2;   /* '\n' */
    if (gs5[2] != 98) return 3;   /* 'b' */
    if (gs5[3] != 0) return 4;    /* null terminator */
    return 0;
}

/* --- Regression: regular arrays still work --- */
int ga[3] = {10, 20, 30};

int test_regression_int_array(void) {
    if (ga[0] != 10) return 1;
    if (ga[1] != 20) return 2;
    if (ga[2] != 30) return 3;
    return 0;
}

/* --- Regression: uninitialized array still works --- */
int test_regression_uninit(void) {
    char buf[4];
    buf[0] = 65;
    buf[1] = 66;
    buf[2] = 0;
    if (buf[0] != 65) return 1;
    if (buf[1] != 66) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_global_string_explicit();
    if (rc) return rc;

    rc = test_global_string_inferred();
    if (rc) return rc + 10;

    rc = test_global_string_padded();
    if (rc) return rc + 20;

    rc = test_global_empty_string();
    if (rc) return rc + 30;

    rc = test_local_string_explicit();
    if (rc) return rc + 40;

    rc = test_local_string_inferred();
    if (rc) return rc + 50;

    rc = test_local_string_padded();
    if (rc) return rc + 60;

    rc = test_local_empty_string();
    if (rc) return rc + 70;

    rc = test_escape_sequences();
    if (rc) return rc + 80;

    rc = test_regression_int_array();
    if (rc) return rc + 90;

    rc = test_regression_uninit();
    if (rc) return rc + 100;

    my_puts("Phase 17: all tests passed\n");
    return 0;
}
