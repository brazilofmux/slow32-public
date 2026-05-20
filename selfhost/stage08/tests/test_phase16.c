/* Phase 16 test: array and struct initializers */

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

struct Rect {
    struct Point tl;
    struct Point br;
};

/* --- Global initialized arrays --- */
int ga[3] = {10, 20, 30};
int gpa[5] = {1, 2};

/* --- Global initialized structs --- */
struct Point gp = {5, 15};

/* --- Test: global array init --- */
int test_global_array(void) {
    if (ga[0] != 10) return 1;
    if (ga[1] != 20) return 2;
    if (ga[2] != 30) return 3;
    return 0;
}

/* --- Test: global partial array init --- */
int test_global_partial(void) {
    if (gpa[0] != 1) return 1;
    if (gpa[1] != 2) return 2;
    if (gpa[2] != 0) return 3;
    if (gpa[3] != 0) return 4;
    if (gpa[4] != 0) return 5;
    return 0;
}

/* --- Test: global struct init --- */
int test_global_struct(void) {
    if (gp.x != 5) return 1;
    if (gp.y != 15) return 2;
    return 0;
}

/* --- Test: local array init with constants --- */
int test_local_array(void) {
    int arr[4] = {100, 200, 300, 400};
    if (arr[0] != 100) return 1;
    if (arr[1] != 200) return 2;
    if (arr[2] != 300) return 3;
    if (arr[3] != 400) return 4;
    return 0;
}

/* --- Test: local array init with expressions --- */
int test_local_array_expr(void) {
    int a;
    int b;
    int arr[3] = {1 + 2, 10 * 3, 7};
    a = 1 + 2;
    b = 10 * 3;
    if (arr[0] != a) return 1;
    if (arr[1] != b) return 2;
    if (arr[2] != 7) return 3;
    return 0;
}

/* --- Test: local struct init --- */
int test_local_struct(void) {
    struct Point p = {42, 99};
    if (p.x != 42) return 1;
    if (p.y != 99) return 2;
    return 0;
}

/* --- Test: nested struct init (flattened) --- */
int test_nested_struct(void) {
    struct Rect r = {1, 2, 3, 4};
    if (r.tl.x != 1) return 1;
    if (r.tl.y != 2) return 2;
    if (r.br.x != 3) return 3;
    if (r.br.y != 4) return 4;
    return 0;
}

/* --- Test: regression - uninitialized array still works --- */
int test_uninit_array(void) {
    int arr[3];
    arr[0] = 7;
    arr[1] = 8;
    arr[2] = 9;
    if (arr[0] != 7) return 1;
    if (arr[1] != 8) return 2;
    if (arr[2] != 9) return 3;
    return 0;
}

/* --- Test: regression - uninitialized struct still works --- */
int test_uninit_struct(void) {
    struct Point p;
    p.x = 11;
    p.y = 22;
    if (p.x != 11) return 1;
    if (p.y != 22) return 2;
    return 0;
}

/* --- Test: global char array init --- */
char gc[3] = {65, 66, 0};

int test_global_char_array(void) {
    if (gc[0] != 65) return 1;
    if (gc[1] != 66) return 2;
    if (gc[2] != 0) return 3;
    return 0;
}

/* --- Test: global nested struct init --- */
struct Rect gr = {10, 20, 30, 40};

int test_global_nested_struct(void) {
    if (gr.tl.x != 10) return 1;
    if (gr.tl.y != 20) return 2;
    if (gr.br.x != 30) return 3;
    if (gr.br.y != 40) return 4;
    return 0;
}

/* --- Test: negative values in initializer --- */
int gn[3] = {-1, 0, -42};

int test_negative_init(void) {
    if (gn[0] != -1) return 1;
    if (gn[1] != 0) return 2;
    if (gn[2] != -42) return 3;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_global_array();
    if (rc) return rc;

    rc = test_global_partial();
    if (rc) return rc + 10;

    rc = test_global_struct();
    if (rc) return rc + 20;

    rc = test_local_array();
    if (rc) return rc + 30;

    rc = test_local_array_expr();
    if (rc) return rc + 40;

    rc = test_local_struct();
    if (rc) return rc + 50;

    rc = test_nested_struct();
    if (rc) return rc + 60;

    rc = test_uninit_array();
    if (rc) return rc + 70;

    rc = test_uninit_struct();
    if (rc) return rc + 80;

    rc = test_global_char_array();
    if (rc) return rc + 90;

    rc = test_global_nested_struct();
    if (rc) return rc + 100;

    rc = test_negative_init();
    if (rc) return rc + 110;

    my_puts("Phase 16: all tests passed\n");
    return 0;
}
