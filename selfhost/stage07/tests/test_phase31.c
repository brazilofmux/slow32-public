/* test_phase31.c -- C99 brace-elision into array sub-aggregates */

struct head_arr {
    int head[16];
    struct {
        int a;
        int b;
    } inner;
    int used;
    int later[4];
    int tail;
};

struct flat_arr {
    int a;
    int b[3];
    int c;
};

struct flat_arr g_flat = {1, 2, 3, 4, 5};
struct head_arr g_head = {0};

int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

void test_globals(void) {
    check(g_flat.a == 1, 1);
    check(g_flat.b[0] == 2, 2);
    check(g_flat.b[1] == 3, 3);
    check(g_flat.b[2] == 4, 4);
    check(g_flat.c == 5, 5);

    check(g_head.head[0] == 0, 6);
    check(g_head.head[15] == 0, 7);
    check(g_head.inner.a == 0, 8);
    check(g_head.inner.b == 0, 9);
    check(g_head.used == 0, 10);
    check(g_head.later[0] == 0, 11);
    check(g_head.tail == 0, 12);
}

void test_locals(void) {
    struct flat_arr l_flat = {10, 20, 30, 40, 50};
    struct head_arr l_head = {0};
    struct flat_arr l_one = {7};

    check(l_flat.a == 10, 13);
    check(l_flat.b[0] == 20, 14);
    check(l_flat.b[1] == 30, 15);
    check(l_flat.b[2] == 40, 16);
    check(l_flat.c == 50, 17);

    check(l_head.head[0] == 0, 18);
    check(l_head.head[15] == 0, 19);
    check(l_head.inner.a == 0, 20);
    check(l_head.tail == 0, 21);

    l_head.later[3] = 99;
    l_head.tail = 1;
    check(l_head.later[3] == 99, 22);
    check(l_head.tail == 1, 23);

    check(l_one.a == 7, 24);
    check(l_one.b[0] == 0, 25);
    check(l_one.b[2] == 0, 26);
    check(l_one.c == 0, 27);
}

int main(void) {
    test_globals();
    test_locals();
    return failed;
}
