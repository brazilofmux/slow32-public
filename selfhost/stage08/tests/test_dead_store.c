/* A store is dead when every path from it reaches a store over the same
 * bytes, or the function's return for a frame slot, before anything may
 * read it -- across blocks, not only within one.  A read on any path, a
 * call that could reach the slot (its address escaped), a store to a
 * global or through a pointer that reaches the return, a volatile store:
 * all keep it.  The arrays reach past the scalar replacement's 128-byte
 * window (element 39) so that they stay in memory: anything smaller is
 * registers before this pass sees it.
 * run-tests.sh counts each function's stores; the results must still
 * be right.  docs/plans/memory-model.md, step 4. */

static int sink;
static void drink(int *p) { sink = sink + *p; }
static void stir(void) { sink = sink + 1; }

/* buf[0] is written in the entry block and overwritten in both arms: the
 * first store is dead.  One store per arm, plus one of buf[39]: three. */
int both_arms(int c, int v) {
    int buf[40];
    buf[0] = v;
    buf[39] = v + 1;
    if (c) buf[0] = v * 2;
    else buf[0] = v * 3;
    return buf[0] + buf[39];
}

/* Written in the entry block, overwritten in one arm, read in the
 * other: the first store is live. */
int one_arm_reads(int c, int v) {
    int buf[40];
    buf[0] = v;
    buf[39] = 0;
    if (c) buf[0] = v * 2;
    else buf[39] = buf[0];
    return buf[0] + buf[39];
}

/* big[39] is written in both arms and never read before the return:
 * both stores are dead.  stir() cannot see big: its address never left.
 * Only big[0]'s store remains. */
int never_read(int c, int v) {
    int big[40];
    big[0] = v;
    if (c) { big[39] = v + 1; stir(); }
    else { big[39] = v - 1; stir(); }
    return big[0] + sink;
}

/* The address goes to a callee: the callee may read it, nothing is dead. */
int escaped(int c, int v) {
    int box;
    box = v;
    if (c) drink(&box);
    box = v + 5;
    drink(&box);
    return box;
}

/* A global written before the return is observable: live. */
int to_global(int v) {
    sink = v;
    return v;
}

/* Written each trip and read after the loop: the store in the loop is
 * live (the read follows on the exit path). */
int in_loop(int n) {
    int keep[40];
    keep[39] = 0;
    while (n > 0) { keep[39] = n; n = n - 1; }
    return keep[39];
}

/* A union's low word is written and never read; once that store is
 * gone, the high word is the slot's only use, reached through a +4 step
 * that instruction selection folds away.  The function must still keep
 * its frame: a frameless leaf saves no callee-saved register and would
 * address the slot below the caller's stack pointer (selfhost
 * ISSUES-93).  run-tests.sh checks that nothing here stores below sp. */
int sign_bit(double x) {
    union { double d; unsigned int u[2]; } b;
    b.d = x;
    return (int)(b.u[1] >> 31);
}

int main(void) {
    sink = 0;
    if (both_arms(1, 4) != 8 + 5 || both_arms(0, 4) != 12 + 5) return 1;
    if (one_arm_reads(0, 7) != 14 || one_arm_reads(1, 7) != 14) return 2;
    sink = 10;
    if (never_read(1, 3) != 3 + 11) return 3;
    sink = 0;
    if (escaped(1, 2) != 7 || sink != 2 + 7) return 4;
    if (to_global(9) != 9 || sink != 9) return 5;
    if (in_loop(5) != 1 || in_loop(0) != 0) return 6;
    if (sign_bit(-2.5) != 1 || sign_bit(2.5) != 0) return 7;
    return 0;
}
