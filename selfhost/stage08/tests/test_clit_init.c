/* Compound literals INSIDE a local's initializer (GitHub issue 79).
 * The declarator used to bind its variable to "the most recent local"
 * after parsing the initializer -- by then the literal had added a
 * hidden local of its own, so the variable was bound to the literal's
 * slot.  The assignment form `q = &(int){7};` was never affected. */
struct pair { int a; int b; };

static int sum_pair_p(struct pair *p) { return p->a * 3 + p->b * 5; }
static int read_at(int *a, int idx) { return a[idx]; }
static int check(int cond, int code) { if (!cond) { return code; } return 0; }

int main(void) {
    int x = 5;
    int *q = &(int){ 7 };                       /* pointer local, literal initializer */
    int y = 3 + (int){ 7 };                     /* arithmetic initializer with a literal */
    struct pair p = { (int){ 1 } + 6, 2 };      /* struct init with a literal member */
    int *r = &(int){ (int){ 3 } + 4 };          /* a literal nested in a literal */
    int i;
    int acc = 0;
    int rc;

    if ((rc = check(x + *q == 12, 1)) != 0) return rc;
    if ((rc = check(y == 10, 2)) != 0) return rc;
    if ((rc = check(p.a == 7 && p.b == 2, 3)) != 0) return rc;
    if ((rc = check(*r == 7, 4)) != 0) return rc;
    for (i = 0; i < 4; i++) {
        struct pair *pp = &(struct pair){ i, i * i };          /* in a loop body */
        int *a = (int[]){ i, i + 1, i + 2, i + 3 };            /* array literal */
        acc = acc * 3 + sum_pair_p(pp) + read_at(a, i & 3);
    }
    /* i=0: 0; i=1: 0*3+8+2=10; i=2: 30+26+4=60; i=3: 180+54+6=240 */
    if ((rc = check(acc == 240, 5)) != 0) return rc;
    for (int *fq = &(int){ 9 }; *fq < 11; (*fq)++) acc = acc + *fq;   /* for-init declaration */
    if ((rc = check(acc == 259, 6)) != 0) return rc;
    return 0;
}
