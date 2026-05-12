/* test_phase27.c -- anonymous struct/union members */

struct pair_wrap {
    int head;
    struct {
        int left;
        int right;
    };
    int tail;
};

struct union_wrap {
    int tag;
    union {
        int word;
        int alias;
    };
    int tail;
};

struct deep_wrap {
    int tag;
    union {
        struct {
            int code;
            int aux;
        };
        int word;
    };
    int tail;
};

struct pair_wrap gp = {1, 2, 3, 4};
struct pair_wrap gp_braced = {5, {6, 7}, 8};
struct union_wrap gu = {9, 10, 11};

int failed;

void check(int cond, int bit) {
    if (!cond) failed = failed + bit;
}

void test_offsets(void) {
    check(offsetof(struct pair_wrap, left) == 4, 1);
    check(offsetof(struct pair_wrap, right) == 8, 2);
    check(offsetof(struct pair_wrap, tail) == 12, 4);
    check(sizeof(struct pair_wrap) == 16, 8);

    check(offsetof(struct union_wrap, word) == 4, 16);
    check(offsetof(struct union_wrap, alias) == 4, 32);
    check(offsetof(struct union_wrap, tail) == 8, 64);
    check(sizeof(struct union_wrap) == 12, 128);

    check(offsetof(struct deep_wrap, code) == 4, 256);
    check(offsetof(struct deep_wrap, aux) == 8, 512);
    check(offsetof(struct deep_wrap, word) == 4, 1024);
    check(offsetof(struct deep_wrap, tail) == 12, 2048);
}

void test_access(void) {
    struct pair_wrap p;
    struct union_wrap u;
    struct deep_wrap d;

    p.head = 12;
    p.left = 34;
    p.right = 56;
    p.tail = 78;
    check(p.head == 12, 4096);
    check(p.left == 34, 8192);
    check(p.right == 56, 16384);
    check(p.tail == 78, 32768);

    u.tag = 1;
    u.word = 99;
    u.tail = 2;
    check(u.alias == 99, 65536);
    u.alias = 123;
    check(u.word == 123, 131072);
    check(u.tail == 2, 262144);

    d.tag = 3;
    d.code = 44;
    d.aux = 55;
    d.tail = 66;
    check(d.code == 44, 524288);
    check(d.aux == 55, 1048576);
    d.word = 77;
    check(d.code == 77, 2097152);
    check(d.tail == 66, 4194304);
}

void test_initializers(void) {
    struct pair_wrap lp = {12, 13, 14, 15};
    struct pair_wrap lp_braced = {16, {17, 18}, 19};
    struct union_wrap lu = {20, 21, 22};

    check(gp.head == 1, 8388608);
    check(gp.left == 2, 16777216);
    check(gp.right == 3, 33554432);
    check(gp.tail == 4, 67108864);

    check(gp_braced.head == 5, 134217728);
    check(gp_braced.left == 6, 268435456);
    check(gp_braced.right == 7, 536870912);
    check(gp_braced.tail == 8, 1073741824);

    check(gu.tag == 9, 1);
    check(gu.word == 10, 2);
    check(gu.alias == 10, 4);
    check(gu.tail == 11, 8);

    check(lp.head == 12, 16);
    check(lp.left == 13, 32);
    check(lp.right == 14, 64);
    check(lp.tail == 15, 128);

    check(lp_braced.head == 16, 256);
    check(lp_braced.left == 17, 512);
    check(lp_braced.right == 18, 1024);
    check(lp_braced.tail == 19, 2048);

    check(lu.tag == 20, 4096);
    check(lu.word == 21, 8192);
    check(lu.alias == 21, 16384);
    check(lu.tail == 22, 32768);
}

int main(void) {
    test_offsets();
    test_access();
    test_initializers();
    return failed;
}
