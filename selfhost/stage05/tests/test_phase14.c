/* Phase 14 test: unions */

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

/* --- Union definitions --- */

union IntOrChar {
    int i;
    char c;
};

/* --- Test: basic union, sizeof --- */
int test_basic(void) {
    union IntOrChar u;
    u.i = 42;
    if (u.i != 42) return 1;
    /* sizeof union == size of largest member (int=4), rounded up */
    if (sizeof(union IntOrChar) != 4) return 2;
    return 0;
}

/* --- Test: union pointer with -> --- */
int test_pointer(void) {
    union IntOrChar u;
    union IntOrChar *up;
    u.i = 99;
    up = &u;
    if (up->i != 99) return 1;
    up->i = 77;
    if (u.i != 77) return 2;
    return 0;
}

/* --- Test: tagged union (union inside struct) --- */
struct Tagged {
    int tag;
    union IntOrChar val;
};

int test_tagged(void) {
    struct Tagged t;
    t.tag = 1;
    t.val.i = 123;
    if (t.tag != 1) return 1;
    if (t.val.i != 123) return 2;
    if (sizeof(struct Tagged) != 8) return 3;
    return 0;
}

/* --- Test: struct inside union --- */
struct Pair {
    int a;
    int b;
};

union StructOrInt {
    struct Pair p;
    int x;
};

int test_struct_in_union(void) {
    union StructOrInt u;
    u.p.a = 10;
    u.p.b = 20;
    if (u.p.a != 10) return 1;
    if (u.p.b != 20) return 2;
    /* sizeof should be max(sizeof Pair=8, sizeof int=4) = 8 */
    if (sizeof(union StructOrInt) != 8) return 3;
    return 0;
}

/* --- Test: global union --- */
union IntOrChar g_u;

int test_global(void) {
    g_u.i = 55;
    if (g_u.i != 55) return 1;
    g_u.i = 0;
    g_u.c = 65;
    /* After writing char, low byte should be 65 */
    if (g_u.c != 65) return 2;
    return 0;
}

/* --- Test: typedef union --- */
typedef union IntOrChar IOC;

int test_typedef(void) {
    IOC u;
    u.i = 100;
    if (u.i != 100) return 1;
    return 0;
}

/* --- Test: type punning (write int, read char gets low byte) --- */
int test_punning(void) {
    union IntOrChar u;
    u.i = 0;
    u.i = 0x41;  /* 65 decimal */
    if (u.c != 65) return 1;
    u.i = 0;
    u.c = 88;
    if (u.c != 88) return 2;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_basic();
    if (rc) return rc;

    rc = test_pointer();
    if (rc) return rc + 10;

    rc = test_tagged();
    if (rc) return rc + 20;

    rc = test_struct_in_union();
    if (rc) return rc + 30;

    rc = test_global();
    if (rc) return rc + 40;

    rc = test_typedef();
    if (rc) return rc + 50;

    rc = test_punning();
    if (rc) return rc + 60;

    puts("Phase 14: all tests passed\n");
    return 0;
}
