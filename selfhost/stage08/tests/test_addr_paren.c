/* GitHub issue 59: &(sym) with grouping parens in an initializer is the
 * same relocation as &sym.  Extra parens around the name used to fall
 * through to the constant evaluator and die "expected constant integer". */
static int A;
static int B[4];
struct F { int n; void *p; };
static struct F t1[] = { {2, &A} };
static struct F t2[] = { {2, &(A)} };
static struct F t3[] = { {2, &((A))} };
static void *p = &(A);
static int *q = &B[2];
static int *r = &(B[2]);
static int *s = &(B)[2];

int main(void) {
    if (t1[0].p != t2[0].p) return 1;
    if (t1[0].p != t3[0].p) return 2;
    if (p != (void *)&A) return 3;
    if (q != r) return 4;
    if (q != s) return 5;
    if (q != &B[2]) return 6;
    if (t1[0].n != 2 || t2[0].n != 2 || t3[0].n != 2) return 7;
    return 0;
}
