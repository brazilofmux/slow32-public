/* selfhost ISSUES-81: a struct member read after a call took its
 * address reloaded from the wrong frame offset.  The BURG matched the
 * read as LOAD(faddr) while its address was ADDI(alloca, 20); the
 * regalloc's call-split then replaced that operand with a fresh COPY,
 * whose frame offset was 0, so the reload went to fp+0+20 -- past the
 * struct, into the caller's frame.  Found by cobol's selfhost-libcob
 * gate (a FLOAT-DECIMAL-16 value losing its scale); the copy carries
 * its source's frame- and symbol-address chains now. */
typedef struct { unsigned m[4]; int neg; int scale; } W;
struct G { int a; int b; };
static struct G g;

static void touch(int *scale) { if (*scale > 1000) *scale = 0; }
static int t_local(void) { W w; w.m[0] = 9; w.scale = 3; touch(&w.scale); return w.scale; }
static int t_copy(const W *w0) { W w = *w0; touch(&w.scale); return w.scale; }
static int t_store(void) { W w; w.scale = 3; touch(&w.scale); w.scale = w.scale + 1; touch(&w.scale); return w.scale; }
static int t_global(void) { g.b = 5; touch(&g.b); return g.b; }
static int t_twice(void) { W w; w.neg = 1; w.scale = 7; touch(&w.neg); touch(&w.scale); return w.neg * 10 + w.scale; }

int main(void) {
    W w; w.m[0] = 123456; w.scale = 3; w.neg = 1;
    if (t_local() != 3) return 1;
    if (t_copy(&w) != 3) return 2;
    if (t_store() != 4) return 3;
    if (t_global() != 5) return 4;
    if (t_twice() != 17) return 5;
    return 0;
}
