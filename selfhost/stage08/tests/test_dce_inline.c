/* GitHub issue 73: after inlining, unreferenced statics are dropped.
 * add1 is small and only called from main — no out-of-line copy.
 * dec is a one-site static that contains a loop — inlined too.
 * id is address-taken — the out-of-line copy must stay.
 * many is called often enough that splicing every site would grow
 * .text, so call sites stay as jal and the body stays. */
static int add1(int x) { return x + 1; }
static int dec(int n) { while (n > 10) n = n - 1; return n; }
static int id(int x) { return x; }
static int many(int x) { return x + x + x + 3; }
static int (*fp)(int) = id;

int main(void) {
    int s;
    if (add1(41) != 42) return 1;
    if (fp(7) != 7) return 2;
    if (dec(12) != 10) return 3;
    s = many(0) + many(1) + many(2) + many(3) + many(4)
      + many(5) + many(6) + many(7) + many(8) + many(9);
    if (s != 165) return 4;
    return 0;
}
