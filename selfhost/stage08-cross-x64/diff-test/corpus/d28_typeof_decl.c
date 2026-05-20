/* GNU typeof -- both typeof(expr) and typeof(type), used to declare
 * locals, parameters, and members of compound expressions.  Verifies
 * the parser resolves the typeof to the underlying type and the rest
 * of codegen treats it normally. */

struct pair {
    int a;
    int b;
};

static int twice(typeof(int) x) {
    typeof(x)   y = x + x;
    typeof((y)) z = y * 3;
    return z;
}

static int member_typed(struct pair *p) {
    typeof(((struct pair *)0)->a) hold_a = p->a;
    typeof(p->b)                  hold_b = p->b;
    return hold_a * 5 + hold_b * 7;
}

int main(void) {
    typeof(int) i;
    typeof(struct pair) pp = { 11, 22 };
    int acc = 0;

    for (i = 0; i < 16; i++) {
        typeof(i)   j   = i * 2;
        typeof(acc) tmp = acc + j;
        acc = twice(i) + tmp;
        acc = acc * 3 + member_typed(&pp);
        pp.a = pp.a + 1;
        pp.b = pp.b - 1;
    }
    return acc & 0xff;
}
