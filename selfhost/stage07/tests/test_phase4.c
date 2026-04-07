/* Phase 4 test: structs */

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

/* --- Struct definitions --- */

struct Point {
    int x;
    int y;
};

struct Mixed {
    char a;
    int b;
    char c;
};

/* Global struct */
struct Point g_pt;

/* --- Test: basic struct local --- */
int test_basic(void) {
    struct Point p;
    p.x = 10;
    p.y = 20;
    if (p.x != 10) return 1;
    if (p.y != 20) return 2;
    if (p.x + p.y != 30) return 3;
    return 0;
}

/* --- Test: struct pointer and -> --- */
int test_pointer(void) {
    struct Point p;
    struct Point *pp;
    p.x = 42;
    p.y = 99;
    pp = &p;
    if (pp->x != 42) return 1;
    if (pp->y != 99) return 2;
    pp->x = 7;
    if (p.x != 7) return 3;
    return 0;
}

/* --- Test: pass struct pointer to function --- */
void set_point(struct Point *p, int x, int y) {
    p->x = x;
    p->y = y;
}

int test_func_arg(void) {
    struct Point p;
    set_point(&p, 100, 200);
    if (p.x != 100) return 1;
    if (p.y != 200) return 2;
    return 0;
}

/* --- Test: sizeof --- */
int test_sizeof(void) {
    if (sizeof(struct Point) != 8) return 1;
    /* Mixed: char a (1 byte) + 3 pad + int b (4) + char c (1) + 3 pad = 12 */
    if (sizeof(struct Mixed) != 12) return 2;
    return 0;
}

/* --- Test: global struct --- */
int test_global(void) {
    g_pt.x = 55;
    g_pt.y = 66;
    if (g_pt.x != 55) return 1;
    if (g_pt.y != 66) return 2;
    return 0;
}

/* --- Test: global struct via pointer --- */
int test_global_ptr(void) {
    struct Point *pp;
    g_pt.x = 11;
    g_pt.y = 22;
    pp = &g_pt;
    if (pp->x != 11) return 1;
    if (pp->y != 22) return 2;
    pp->y = 33;
    if (g_pt.y != 33) return 3;
    return 0;
}

/* --- Test: mixed char/int alignment --- */
int test_mixed(void) {
    struct Mixed m;
    m.a = 65;
    m.b = 1000;
    m.c = 66;
    if (m.a != 65) return 1;
    if (m.b != 1000) return 2;
    if (m.c != 66) return 3;
    return 0;
}

/* --- Test: struct pointer arithmetic --- */
int test_ptr_arith(void) {
    struct Point arr[4];
    struct Point *p;
    arr[0].x = 1;
    arr[1].x = 2;
    arr[2].x = 3;
    p = &arr[0];
    if (p->x != 1) return 1;
    p = p + 1;
    if (p->x != 2) return 2;
    p = p + 1;
    if (p->x != 3) return 3;
    return 0;
}

/* --- Test: nested struct (struct with struct member) --- */
struct Rect {
    struct Point pos;
    struct Point size;
};

int test_nested(void) {
    struct Rect r;
    r.pos.x = 10;
    r.pos.y = 20;
    r.size.x = 100;
    r.size.y = 200;
    if (r.pos.x != 10) return 1;
    if (r.pos.y != 20) return 2;
    if (r.size.x != 100) return 3;
    if (r.size.y != 200) return 4;
    if (sizeof(struct Rect) != 16) return 5;
    return 0;
}

/* --- Test: nested struct via pointer --- */
int test_nested_ptr(void) {
    struct Rect r;
    struct Rect *rp;
    r.pos.x = 5;
    r.size.y = 50;
    rp = &r;
    if (rp->pos.x != 5) return 1;
    if (rp->size.y != 50) return 2;
    rp->pos.y = 15;
    if (r.pos.y != 15) return 3;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_basic();
    if (rc) return rc;

    rc = test_pointer();
    if (rc) return rc + 10;

    rc = test_func_arg();
    if (rc) return rc + 20;

    rc = test_sizeof();
    if (rc) return rc + 30;

    rc = test_global();
    if (rc) return rc + 40;

    rc = test_global_ptr();
    if (rc) return rc + 50;

    rc = test_mixed();
    if (rc) return rc + 60;

    rc = test_ptr_arith();
    if (rc) return rc + 70;

    rc = test_nested();
    if (rc) return rc + 80;

    rc = test_nested_ptr();
    if (rc) return rc + 90;

    my_puts("Phase 4: all tests passed\n");
    return 0;
}
