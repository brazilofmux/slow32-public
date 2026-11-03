/* A block-scope static declarator list whose later declarators have
 * brace initializers: the comma loop had its own copy of the
 * initializer chain, without the structure case, so sbasic's
 * `static const value_t as_int = {..}, as_long = {..};` stopped on the
 * second brace ("unexpected token in expression"). */
struct val { int type; union { int i; double d; } u; };
struct pair { int a; int b; };

static int pick(int k) {
    static const struct val as_int = { 1, { 0 } }, as_long = { 2, { 7 } };
    static struct pair p = { 3, 4 }, q = { 5, 6 }, *pp = &q;
    static const char *s = "x", *t = "yz";
    static int n = 9, m;
    if (k == 0) return as_int.type + as_int.u.i;          /* 1 */
    if (k == 1) return as_long.type * 10 + as_long.u.i;   /* 27 */
    if (k == 2) return p.a + p.b * 10 + q.a * 100 + pp->b * 1000;  /* 6543 */
    if (k == 3) return s[0] + t[1] + n + m;               /* 'x'+'z'+9 */
    return -1;
}

int main(void) {
    if (pick(0) != 1) return 1;
    if (pick(1) != 27) return 2;
    if (pick(2) != 6543) return 3;
    if (pick(3) != 'x' + 'z' + 9) return 4;
    return 0;
}
