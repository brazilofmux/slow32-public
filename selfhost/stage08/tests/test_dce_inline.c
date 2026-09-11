/* GitHub issue 73: after inlining, unreferenced statics are dropped.
 * add1 is small and only called from main — no out-of-line copy.
 * id is address-taken — the out-of-line copy must stay. */
static int add1(int x) { return x + 1; }
static int id(int x) { return x; }
static int (*fp)(int) = id;

int main(void) {
    if (add1(41) != 42) return 1;
    if (fp(7) != 7) return 2;
    return 0;
}
