/* test_phase30.c -- block-scope compound literals */

struct pair {
    int x;
    int y;
};

struct box {
    struct pair p;
    int tag;
};

int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

int sum_pair(struct pair *p) {
    return p->x + p->y;
}

void test_scalar_literal(void) {
    int v;
    int *p;

    v = (int){ 12 };
    check(v == 12, 1);

    p = &(int){ 17 };
    check(*p == 17, 2);
    *p = 19;
    check(*p == 19, 3);

    v = ((int){ 12 } = 13);
    check(v == 13, 4);
}

void test_struct_literal(void) {
    struct pair p;
    struct pair *q;
    int y;

    p = (struct pair){ .y = 7, .x = 5 };
    check(p.x == 5, 5);
    check(p.y == 7, 6);

    y = ((struct pair){ .x = 1, .y = 2 }).y;
    check(y == 2, 7);

    q = &(struct pair){ .x = 9, .y = 10 };
    check(q->x == 9, 8);
    check(q->y == 10, 9);

    check(sum_pair(&(struct pair){ .x = 11, .y = 12 }) == 23, 10);
}

void test_nested_literal(void) {
    struct box b;

    b = (struct box){
        .p.y = 4,
        .tag = 8,
        .p.x = 3
    };
    check(b.p.x == 3, 11);
    check(b.p.y == 4, 12);
    check(b.tag == 8, 13);
}

void test_array_literal(void) {
    int *a;
    int *b;
    char *letters;

    a = (int[]){ [2] = 22, 33 };
    check(a[0] == 0, 14);
    check(a[2] == 22, 15);
    check(a[3] == 33, 16);

    b = (int[5]){ 1, [3] = 4 };
    check(b[0] == 1, 17);
    check(b[1] == 0, 18);
    check(b[3] == 4, 19);
    check(b[4] == 0, 20);

    letters = (char[4]){ [2] = 'z' };
    check(letters[0] == 0, 21);
    check(letters[2] == 'z', 22);
    check(letters[3] == 0, 23);
}

int main(void) {
    test_scalar_literal();
    test_struct_literal();
    test_nested_literal();
    test_array_literal();
    return failed;
}
