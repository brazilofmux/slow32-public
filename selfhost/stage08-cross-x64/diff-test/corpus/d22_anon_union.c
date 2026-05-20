/* C11 anonymous union: writing one member must alias the other.  Tests
 * layout (kind/union/suffix offsets), load-after-store correctness
 * through different member names that map to the same offset, and
 * signed/unsigned reinterpretation. */

struct cell {
    int kind;
    union {
        int          as_int;
        unsigned int as_uint;
    };
    int suffix;
};

static int loop(struct cell *c, int n) {
    int acc = 0;
    int i;
    for (i = 0; i < n; i++) {
        c->as_int  = i * 7 - 3;
        acc        = acc + (int)c->as_uint;
        c->as_uint = (unsigned)(i * 13) + 99u;
        acc        = acc + c->as_int;
        c->suffix  = c->suffix + c->as_int;
        c->kind    = c->kind + 1;
    }
    return acc + c->suffix + c->kind;
}

int main(void) {
    /* Flat init: { kind, as_int (first union member), suffix } */
    struct cell a = { 0, 1, 2 };
    struct cell b = { 100, 0, 0 };
    int acc = 0;
    acc = acc * 17 + loop(&a, 30);
    acc = acc * 17 + loop(&b, 30);
    return acc & 0xff;
}
