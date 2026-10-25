/* Memory at joins: a value read before a branch and read again after
 * it, with each path doing something different in between -- nothing,
 * a store through a pointer that may be the same, a store on one arm,
 * a call, a loop whose body writes.  Every answer must be right
 * whatever the optimizer forwards or keeps (docs/plans/memory-model.md,
 * step 2: forwarding across a join was measured and set aside; these
 * cases stay as the correctness line for whatever comes back). */

/* Both arms leave *p alone, and a is used after the join. */
int reused(int *p, int c) {
    int a = *p;
    int r = 3;
    if (c) r = r * 5;
    return a + r + *p;
}

/* Both arms leave *p alone; nothing after the join needs a. */
int straight(int *p, int c) {
    int a = *p;
    int r;
    if (c) r = a * 3;
    else r = a + 7;
    return r + *p;
}

/* The else arm stores through q, which may be p. */
int maybe_written(int *p, int *q, int c) {
    int a = *p;
    if (c) a = a + 1;
    else *q = 40;
    return a + *p;
}

/* A store on one path only: the second read is live and must see it
 * or not, by c. */
int one_arm_store(int *p, int c) {
    int a = *p;
    if (c) *p = a + 100;
    return *p;
}

/* A loop header is reached by its back edge: the load inside must be
 * made each trip, the body stores through q (which may be p). */
int looped(int *p, int *q, int n) {
    int s = 0;
    while (n > 0) {
        s = s + *p;
        *q = *q + 1;
        n = n - 1;
    }
    return s;
}

/* The value stored before the branch comes back after it: no reload. */
int stored_then(int *p, int c, int v) {
    *p = v;
    if (c) v = v * 2;
    return *p + v;
}

/* The same global named twice in one statement: one load.  Each
 * naming builds its own address value, and forwarding compares them as
 * locations, not as values.  (A plain global, not a volatile one.) */
static int knob;
int global_twice(void) {
    return knob * 3 + knob;
}

/* A call on one path forgets everything. */
static int g;
static void bump(void) { g = g + 1; }
int call_arm(int *p, int c) {
    int a = *p;
    if (c) bump();
    return a + *p;
}

int main(void) {
    int x, y;
    x = 5; y = 9;
    if (straight(&x, 1) != 15 + 5 || straight(&x, 0) != 12 + 5) return 1;
    if (reused(&x, 1) != 5 + 15 + 5 || reused(&x, 0) != 5 + 3 + 5) return 9;
    x = 5;
    if (maybe_written(&x, &x, 0) != 5 + 40) return 2;
    x = 5;
    if (maybe_written(&x, &y, 0) != 5 + 5 || y != 40) return 3;
    x = 5;
    if (one_arm_store(&x, 1) != 105 || one_arm_store(&x, 0) != 105) return 4;
    x = 2; y = 0;
    if (looped(&x, &x, 3) != 2 + 3 + 4 || x != 5) return 5;
    x = 2; y = 0;
    if (looped(&x, &y, 3) != 6 || y != 3) return 6;
    if (stored_then(&x, 1, 7) != 7 + 14 || x != 7) return 7;
    x = 1; g = 0;
    if (call_arm(&x, 1) != 2 || g != 1) return 8;
    knob = 6;
    if (global_twice() != 24) return 10;
    return 0;
}
