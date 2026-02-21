/* Phase 8 test: typedef */

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

/* --- Basic int alias --- */

typedef int myint;

int test_int_alias(void) {
    myint x;
    myint y;
    x = 42;
    y = x + 8;
    if (y != 50) return 1;
    return 0;
}

/* --- Pointer alias --- */

typedef char *string;

int test_ptr_alias(void) {
    string s;
    s = "hello";
    if (*s != 104) return 1;  /* 'h' */
    return 0;
}

/* --- Struct alias --- */

struct Vec2 {
    int x;
    int y;
};

typedef struct Vec2 Vec2;

int test_struct_alias(void) {
    Vec2 v;
    v.x = 10;
    v.y = 20;
    if (v.x + v.y != 30) return 1;
    return 0;
}

/* --- Struct pointer alias --- */

typedef struct Vec2 *Vec2Ptr;

int test_struct_ptr_alias(void) {
    Vec2 v;
    Vec2Ptr p;
    v.x = 5;
    v.y = 7;
    p = &v;
    if (p->x != 5) return 1;
    if (p->y != 7) return 2;
    return 0;
}

/* --- Typedef used in function params --- */

typedef int num;

num add_nums(num a, num b) {
    return a + b;
}

int test_func_param(void) {
    num result;
    result = add_nums(3, 4);
    if (result != 7) return 1;
    return 0;
}

/* --- Function pointer typedef --- */

typedef int (*BinOp)(int, int);

int my_add(int a, int b) { return a + b; }

int test_func_ptr_typedef(void) {
    /* Function pointer typedef parsed; test that it doesn't crash */
    int r;
    r = my_add(10, 20);
    if (r != 30) return 1;
    return 0;
}

/* --- Typedef of typedef --- */

typedef int base_t;
typedef base_t derived_t;

int test_typedef_chain(void) {
    derived_t x;
    x = 99;
    if (x != 99) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_int_alias();
    if (rc) return rc;

    rc = test_ptr_alias();
    if (rc) return rc + 10;

    rc = test_struct_alias();
    if (rc) return rc + 20;

    rc = test_struct_ptr_alias();
    if (rc) return rc + 30;

    rc = test_func_param();
    if (rc) return rc + 40;

    rc = test_func_ptr_typedef();
    if (rc) return rc + 50;

    rc = test_typedef_chain();
    if (rc) return rc + 60;

    my_puts("Phase 8: all tests passed\n");
    return 0;
}
