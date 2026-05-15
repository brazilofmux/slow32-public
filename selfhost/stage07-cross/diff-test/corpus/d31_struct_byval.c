/* Struct passed and returned by value -- the AAPCS64 / SysV-x64
 * pass-by-value path lowered to caller-passes-pointer +
 * callee-copies-into-local-slot in the shared frontend (ISSUES.md
 * #51).  Exercises:
 *   - 2-int struct param (8 bytes)
 *   - 4-int struct param (16 bytes)
 *   - struct with byte fields and padding
 *   - struct param mixed with scalar args (positional)
 *   - callee-side mutation of the param must NOT escape to caller
 *   - struct return composed with struct param: swap(p), add_to(p, n)
 *   - nested call: swap(swap(p)) == p */

struct pair {
    int a;
    int b;
};

struct quad {
    int a, b, c, d;
};

struct mixedty {
    char a;
    int  b;
    char c;
};

static int sum2(struct pair p) {
    return p.a + p.b;
}

static int sum4(struct quad q) {
    return q.a + q.b * 2 + q.c * 3 + q.d * 4;
}

static int mixed_args(int x, struct pair p, int y) {
    return x * 100 + p.a * 10 + p.b + y;
}

static int sumbytes(struct mixedty m) {
    return (int)m.a + m.b + (int)m.c;
}

static int mutate(struct pair p) {
    p.a = 999;
    p.b = 888;
    return p.a + p.b;
}

static struct pair swap(struct pair p) {
    struct pair r;
    r.a = p.b;
    r.b = p.a;
    return r;
}

static struct pair add_to(struct pair p, int n) {
    struct pair r;
    r.a = p.a + n;
    r.b = p.b + n;
    return r;
}

int main(void) {
    struct pair    p; p.a = 2; p.b = 3;
    struct quad    q; q.a = 1; q.b = 2; q.c = 3; q.d = 4;
    struct mixedty m; m.a = 'A'; m.b = 100; m.c = 'B';
    struct pair    sw, ad, rt;
    int acc = 0;

    acc = acc * 17 + sum2(p);
    acc = acc * 17 + sum4(q);
    acc = acc * 17 + mixed_args(7, p, 11);
    acc = acc * 17 + sumbytes(m);

    /* Mutate must not affect caller's p. */
    acc = acc * 17 + mutate(p);
    acc = acc * 17 + (p.a + p.b);

    sw = swap(p);
    ad = add_to(p, 100);
    rt = swap(swap(p));    /* identity */
    acc = acc * 17 + sw.a * 31 + sw.b * 29;
    acc = acc * 17 + ad.a * 23 + ad.b * 19;
    acc = acc * 17 + rt.a * 13 + rt.b * 11;
    return acc & 0xff;
}
