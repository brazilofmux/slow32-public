/* GitHub issue 62: extra grouping parens around the SQLITE_INT_TO_PTR
 * subscript are the same address constant as the unwrapped form.
 * ((void*)&((char*)0)[X]) already folded; &(((char*)0)[X]) died
 * "expected constant integer" because grouping demanded ')' on '['. */
#define INT_TO_PTR(X) ((void *)&((char *)0)[X])
#define INT_TO_PTR_PAREN(X) ((void *)&(((char *)0)[X]))

struct F { int n; void *p; };
static struct F t1[] = { {2, INT_TO_PTR(7)} };
static struct F t2[] = { {2, INT_TO_PTR_PAREN(7)} };
static void *p7 = INT_TO_PTR_PAREN(7);
static void *p0 = &(((char *)0)[0]);
static void *scaled = &(((int *)0)[3]);
static void *nested = ((void *)&((((char *)0)[5])));

int main(void) {
    static struct F blk[] = { {4, INT_TO_PTR_PAREN(9)} };
    if ((int)t1[0].p != 7) return 1;
    if ((int)t2[0].p != 7) return 2;
    if (t1[0].p != t2[0].p) return 3;
    if ((int)p7 != 7) return 4;
    if ((int)p0 != 0) return 5;
    if ((int)scaled != 12) return 6;
    if ((int)nested != 5) return 7;
    if ((int)blk[0].p != 9) return 8;
    return 0;
}
