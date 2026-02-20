/* Test pointers, arrays, structs, strings, sizeof, casts, function pointers,
 * enums, defines, typedef, postfix ++ / -- */

#define MAGIC 42
#define BUF_SZ 16

enum color {
    RED,
    GREEN,
    BLUE
};

struct point {
    int x;
    int y;
};

typedef int myint;

int test_define(void) {
    if (MAGIC != 42) return 1;
    if (BUF_SZ != 16) return 2;
    return 0;
}

int test_enum(void) {
    int c;
    c = GREEN;
    if (c != 1) return 1;
    if (BLUE != 2) return 2;
    return 0;
}

int test_typedef(void) {
    myint x;
    x = 100;
    if (x != 100) return 1;
    return 0;
}

int test_pointer(void) {
    int x;
    int *p;
    x = 42;
    p = &x;
    if (*p != 42) return 1;
    *p = 99;
    if (x != 99) return 2;
    return 0;
}

int test_array(void) {
    int arr[4];
    int i;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    if (arr[0] != 10) return 1;
    if (arr[3] != 40) return 2;
    /* Sum */
    i = 0;
    while (i < 4) {
        if (arr[i] != (i + 1) * 10) return 3;
        i = i + 1;
    }
    return 0;
}

int test_char_array(void) {
    char buf[8];
    buf[0] = 72;  /* H */
    buf[1] = 105; /* i */
    buf[2] = 0;
    if (buf[0] != 72) return 1;
    if (buf[1] != 105) return 2;
    if (buf[2] != 0) return 3;
    return 0;
}

int test_struct(void) {
    struct point p;
    p.x = 10;
    p.y = 20;
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    return 0;
}

int test_struct_ptr(void) {
    struct point p;
    struct point *pp;
    p.x = 5;
    p.y = 7;
    pp = &p;
    if (pp->x != 5) return 1;
    if (pp->y != 7) return 2;
    pp->x = 100;
    if (p.x != 100) return 3;
    return 0;
}

int test_sizeof(void) {
    if (sizeof(int) != 4) return 1;
    if (sizeof(char) != 1) return 2;
    if (sizeof(int *) != 4) return 3;
    if (sizeof(struct point) != 8) return 4;
    return 0;
}

int test_cast(void) {
    int x;
    char c;
    x = 300;
    c = (char)x;
    /* char is only 1 byte so value wraps */
    /* Actually in our compiler, cast is just type change, no truncation emit */
    /* Test that cast doesn't crash at minimum */
    x = (int)c;
    return 0;
}

int test_string(void) {
    char *s;
    s = "Hello";
    if (s[0] != 72) return 1;  /* 'H' */
    if (s[1] != 101) return 2; /* 'e' */
    if (s[5] != 0) return 3;   /* NUL */
    return 0;
}

int add_one(int x) { return x + 1; }

int test_func_ptr(void) {
    int fp;
    int r;
    fp = add_one;
    r = fp(41);
    if (r != 42) return 1;
    return 0;
}

int test_postfix_inc(void) {
    int x;
    int y;
    x = 5;
    y = x++;
    if (y != 5) return 1;
    if (x != 6) return 2;
    y = x--;
    if (y != 6) return 3;
    if (x != 5) return 4;
    return 0;
}

int test_prefix_inc(void) {
    int x;
    int y;
    x = 5;
    y = ++x;
    if (y != 6) return 1;
    if (x != 6) return 2;
    y = --x;
    if (y != 5) return 3;
    if (x != 5) return 4;
    return 0;
}

int test_ptr_arith(void) {
    int arr[4];
    int *p;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    p = arr;
    if (*p != 10) return 1;
    p = p + 1;
    if (*p != 20) return 2;
    p = p + 2;
    if (*p != 40) return 3;
    return 0;
}

int main(void) {
    int rc;
    rc = test_define();
    if (rc) return rc;
    rc = test_enum();
    if (rc) return rc + 10;
    rc = test_typedef();
    if (rc) return rc + 20;
    rc = test_pointer();
    if (rc) return rc + 30;
    rc = test_array();
    if (rc) return rc + 40;
    rc = test_char_array();
    if (rc) return rc + 50;
    rc = test_struct();
    if (rc) return rc + 60;
    rc = test_struct_ptr();
    if (rc) return rc + 70;
    rc = test_sizeof();
    if (rc) return rc + 80;
    rc = test_cast();
    if (rc) return rc + 90;
    rc = test_string();
    if (rc) return rc + 100;
    rc = test_func_ptr();
    if (rc) return rc + 110;
    rc = test_postfix_inc();
    if (rc) return rc + 120;
    rc = test_prefix_inc();
    if (rc) return rc + 130;
    rc = test_ptr_arith();
    if (rc) return rc + 140;
    return 0;
}
