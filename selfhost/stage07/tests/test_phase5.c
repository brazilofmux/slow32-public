/* Phase 5 test: enum + switch/case */

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

/* --- Enum definitions --- */

enum Color { RED, GREEN, BLUE };

enum Explicit { A = 10, B, C = 20, D };

/* --- Test: basic enum values --- */
int test_enum_basic(void) {
    if (RED != 0) return 1;
    if (GREEN != 1) return 2;
    if (BLUE != 2) return 3;
    return 0;
}

/* --- Test: explicit enum values --- */
int test_enum_explicit(void) {
    if (A != 10) return 1;
    if (B != 11) return 2;
    if (C != 20) return 3;
    if (D != 21) return 4;
    return 0;
}

/* --- Test: enum in expression --- */
int test_enum_expr(void) {
    int x;
    x = RED + GREEN + BLUE;
    if (x != 3) return 1;
    x = A * 2;
    if (x != 20) return 2;
    return 0;
}

/* --- Test: enum defined in function body --- */
int test_enum_local(void) {
    enum Size { SMALL = 1, MEDIUM = 2, LARGE = 3 };
    if (SMALL != 1) return 1;
    if (MEDIUM != 2) return 2;
    if (LARGE != 3) return 3;
    return 0;
}

/* --- Test: basic switch --- */
int test_switch_basic(void) {
    int x;
    int r;
    x = 2;
    r = 0;
    switch (x) {
    case 1:
        r = 10;
        break;
    case 2:
        r = 20;
        break;
    case 3:
        r = 30;
        break;
    }
    if (r != 20) return 1;
    return 0;
}

/* --- Test: switch default --- */
int test_switch_default(void) {
    int x;
    int r;
    x = 99;
    r = 0;
    switch (x) {
    case 1:
        r = 10;
        break;
    default:
        r = -1;
        break;
    }
    if (r != -1) return 1;
    return 0;
}

/* --- Test: switch fallthrough --- */
int test_switch_fallthrough(void) {
    int x;
    int r;
    x = 1;
    r = 0;
    switch (x) {
    case 1:
        r = r + 1;
    case 2:
        r = r + 10;
        break;
    case 3:
        r = r + 100;
        break;
    }
    /* x=1 falls through to case 2: r = 0+1+10 = 11 */
    if (r != 11) return 1;
    return 0;
}

/* --- Test: switch with enum values --- */
int test_switch_enum(void) {
    int c;
    int r;
    c = GREEN;
    r = 0;
    switch (c) {
    case RED:
        r = 1;
        break;
    case GREEN:
        r = 2;
        break;
    case BLUE:
        r = 3;
        break;
    }
    if (r != 2) return 1;
    return 0;
}

/* --- Test: nested switch --- */
int test_switch_nested(void) {
    int a;
    int b;
    int r;
    a = 1;
    b = 2;
    r = 0;
    switch (a) {
    case 1:
        switch (b) {
        case 1:
            r = 11;
            break;
        case 2:
            r = 12;
            break;
        }
        break;
    case 2:
        r = 20;
        break;
    }
    if (r != 12) return 1;
    return 0;
}

/* --- Test: switch no match, no default --- */
int test_switch_no_match(void) {
    int x;
    int r;
    x = 99;
    r = 5;
    switch (x) {
    case 1:
        r = 10;
        break;
    case 2:
        r = 20;
        break;
    }
    /* No match, no default — r stays 5 */
    if (r != 5) return 1;
    return 0;
}

/* --- Test: switch with negative case --- */
int test_switch_negative(void) {
    int x;
    int r;
    x = -1;
    r = 0;
    switch (x) {
    case -1:
        r = 1;
        break;
    case 0:
        r = 2;
        break;
    case 1:
        r = 3;
        break;
    }
    if (r != 1) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_enum_basic();
    if (rc) return rc;

    rc = test_enum_explicit();
    if (rc) return rc + 10;

    rc = test_enum_expr();
    if (rc) return rc + 20;

    rc = test_enum_local();
    if (rc) return rc + 30;

    rc = test_switch_basic();
    if (rc) return rc + 40;

    rc = test_switch_default();
    if (rc) return rc + 50;

    rc = test_switch_fallthrough();
    if (rc) return rc + 60;

    rc = test_switch_enum();
    if (rc) return rc + 70;

    rc = test_switch_nested();
    if (rc) return rc + 80;

    rc = test_switch_no_match();
    if (rc) return rc + 90;

    rc = test_switch_negative();
    if (rc) return rc + 100;

    my_puts("Phase 5: all tests passed\n");
    return 0;
}
