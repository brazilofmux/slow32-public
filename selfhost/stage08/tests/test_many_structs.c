/* More structures than the type encoding held: its base field was 12
 * bits, 4,088 structures a translation unit (ast.h TY_STRUCT_BASE,
 * GitHub issue 50 for what happened past it).  5,000, made by pasting;
 * the last ones used by value, by pointer and by pointer to pointer,
 * with unsigned members.  Generated. */
#define S(n) struct s##n { int a; unsigned short u; char pad[n % 7 + 1]; };
#define S10(n) S(n##0) S(n##1) S(n##2) S(n##3) S(n##4) S(n##5) S(n##6) S(n##7) S(n##8) S(n##9)
#define S100(n) S10(n##0) S10(n##1) S10(n##2) S10(n##3) S10(n##4) S10(n##5) S10(n##6) S10(n##7) S10(n##8) S10(n##9)
#define S1000(n) S100(n##0) S100(n##1) S100(n##2) S100(n##3) S100(n##4) S100(n##5) S100(n##6) S100(n##7) S100(n##8) S100(n##9)
S1000(1) S1000(2) S1000(3) S1000(4) S1000(5)
/* int, unsigned short, pad, to a multiple of 4 */
#define SZ(n) ((6 + (n) % 7 + 1 + 3) / 4 * 4)
struct last { struct s5999 *p; struct s5999 **pp; unsigned int big; };
static struct s5999 g;
static struct s4500 h[3];
static unsigned get_u(struct s5999 *p) { return p->u; }
int main(void) {
    struct s5999 *p;
    struct s5999 **pp;
    struct last l;
    p = &g;
    pp = &p;
    (*pp)->a = 41;
    p->u = 65535;
    l.p = p;
    l.pp = pp;
    l.big = 4000000000u;
    h[2].a = 7;
    if (sizeof(struct s5999) != SZ(5999) || sizeof(struct s1000) != SZ(1000)) return 1;
    if (sizeof(struct s4500) != SZ(4500)) return 2;
    if ((*l.pp)->a + 1 != 42) return 3;
    if (get_u(l.p) != 65535u || l.p->u + 1 != 65536) return 4;
    if (l.big / 2 != 2000000000u) return 5;
    if (h[2].a != 7 || sizeof h != 3 * sizeof(struct s4500)) return 6;
    return 0;
}
