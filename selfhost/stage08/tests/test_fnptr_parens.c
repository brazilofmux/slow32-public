/* Grouping parentheses around a function-pointer declarator (6.7.6p6):
 * int ((*g))(void) is int (*g)(void).  "expected name in fn ptr decl"
 * until parser.h's ps_fnptr_parens. */
static int seven(void) { return 7; }
static int add(int a, int b) { return a + b; }
int ((*g))(void) = seven;
int (((*h)))(void);
static int ((*two))(int, int) = add;
static int call(int ((*cb))(void)) { return cb(); }
static int call2(int (((*cb)))(int, int), int x) { return cb(x, x); }
struct ops { int ((*get))(void); int (((*sum)))(int, int); };
union uop { int ((*get))(void); };
int main(void) {
    int ((*l))(void) = seven;
    int (((*m)))(int, int);
    m = add;
    h = seven;
    if (g() + h() + l() != 21) return 1;
    if (two(2, 3) != 5 || m(4, 5) != 9) return 2;
    {
        struct ops o;
        union uop u;
        o.get = seven;
        o.sum = add;
        u.get = seven;
        if (call(seven) != 7 || call2(add, 6) != 12) return 3;
        if (o.get() + o.sum(1, 2) + u.get() != 17) return 4;
    }
    return 0;
}
