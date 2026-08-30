/* test_phase32.c -- block-scope declarator lists (GitHub #8)
 *
 * A declaration at block scope may carry several declarators, each
 * with its own stars, dimensions and initializer, in any order.  The
 * parser used to end the declaration at the first brace initializer
 * and to reject a `[` after a comma.  File scope already handled all
 * of these; every case here is C89. */

typedef struct { long long v; int scale; } cob_num;

struct S { int a; int b; };

cob_num st[4];
int nsp;
struct S s0 = { 5, 6 };
int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

long long getv(void) { return 41; }

int f(void)
{
    /* the libcob.c line that stopped the self-hosted build */
    cob_num a = { getv(), 2 }, b = st[nsp - 1];
    return (int)(a.v + b.v + a.scale);
}

void test_struct_lists(void)
{
    struct S x = { 1, 2 }, y = s0;
    struct S p = { 1, 2 }, q = { 3, 4 };
    struct S r = s0, t = { 7, 8 };
    struct S u = { 9 }, *pu = &u, w;
    check(x.a == 1 && x.b == 2, 1);
    check(y.a == 5 && y.b == 6, 2);
    check(p.a == 1 && p.b == 2 && q.a == 3 && q.b == 4, 3);
    check(r.a == 5 && r.b == 6 && t.a == 7 && t.b == 8, 4);
    check(u.a == 9 && u.b == 0, 5);
    check(pu->a == 9, 6);
    w.a = 3; w.b = 4;
    check(w.a + w.b == 7, 7);
}

void test_array_lists(void)
{
    int a[2] = { 1, 2 }, b = 3;
    int c = 1, d[2] = { 2, 3 };
    int e = 1, h[2];
    int k = { 1 };
    int m[] = { 4, 5, 6 }, n = 7, o[2] = { 8 }, *pp = &n;
    char s1[8] = "one", s2[] = "two", ch = 'x';
    char lbuf[8], rbuf[8];
    h[0] = 4; h[1] = 5;
    check(a[0] == 1 && a[1] == 2 && b == 3, 10);
    check(c == 1 && d[0] == 2 && d[1] == 3, 11);
    check(e == 1 && h[0] == 4 && h[1] == 5, 12);
    check(k == 1, 13);
    check(m[0] == 4 && m[2] == 6 && n == 7, 14);
    check(o[0] == 8 && o[1] == 0, 15);
    check(*pp == 7, 16);
    check(s1[0] == 'o' && s1[3] == 0 && s2[2] == 'o' && s2[3] == 0 && ch == 'x', 17);
    lbuf[0] = 'l'; rbuf[0] = 'r';
    check(lbuf[0] == 'l' && rbuf[0] == 'r', 18);
    check(sizeof(m) == 12 && sizeof(s2) == 4, 19);
}

void test_order(void)
{
    /* initializers run left to right and may read earlier declarators */
    int i = 2, j[2] = { i, i + 1 }, kk = j[1] * 10;
    check(j[0] == 2 && j[1] == 3 && kk == 30, 20);
}

int main(void) {
    st[0].v = 1; st[0].scale = 0;
    nsp = 1;
    check(f() == 44, 30);
    test_struct_lists();
    test_array_lists();
    test_order();
    return failed;
}
