/* A load whose address does not change in a loop, and whose bytes
 * nothing in the loop writes, is made once before it.  One that a store
 * in the body may write, that a call may change, that is volatile, or
 * that the body might not make (a read inside an if; through a pointer
 * it could even fault where the loop would not have read it) stays in
 * the loop.
 * run-tests.sh checks where each function's loads are: a hoisted load
 * sits in the entry block, before the loop's label, which is why these
 * are do-while loops (a while's entry test would put the preheader
 * behind a label of its own).  The sums must still be right.
 * docs/plans/memory-model.md, step 3. */

static int tally;
static volatile int live;
static void note(void) { tally = tally + 1; }

/* *k read every trip: hoisted, one load before the loop. */
int rep(int *k, int n) {
    int s = 0;
    do { s = s + *k; n = n - 1; } while (n > 0);
    return s;
}

/* The body stores through q, which may be k: *k stays in the loop. */
int rep_store(int *k, int *q, int n) {
    int s = 0;
    do { s = s + *k; *q = s; n = n - 1; } while (n > 0);
    return s;
}

/* A call in the body may change anything: stays. */
int rep_call(int *k, int n) {
    int s = 0;
    do { s = s + *k; note(); n = n - 1; } while (n > 0);
    return s;
}

/* A global read on every trip: hoisted. */
int rep_global(int n) {
    int s = 0;
    do { s = s + tally; n = n - 1; } while (n > 0);
    return s;
}

/* A global read only on odd trips: stays.  It could not fault, but
 * LICM moves only what the body does on every trip (its dominance
 * rule, the same for arithmetic): a read made once before a loop that
 * might never want it is a cost question, answered no. */
int rep_global_if(int n) {
    int s = 0;
    do { if (n & 1) s = s + tally; n = n - 1; } while (n > 0);
    return s;
}

/* A pointer read only on odd trips: stays, it might fault. */
int rep_ptr_if(int *k, int n) {
    int s = 0;
    do { if (n & 1) s = s + *k; n = n - 1; } while (n > 0);
    return s;
}

/* A volatile global: read every trip, never hoisted. */
int rep_volatile(int n) {
    int s = 0;
    do { s = s + live; n = n - 1; } while (n > 0);
    return s;
}

/* The store in the body is to another word of the same array: the
 * read of a[0] still moves out. */
int rep_other_word(int *a, int n) {
    int s = 0;
    do { s = s + a[0]; a[1] = s; n = n - 1; } while (n > 0);
    return s;
}

int main(void) {
    int k, q, a[2];
    k = 7;
    if (rep(&k, 5) != 35) return 1;
    k = 7; q = 0;
    if (rep_store(&k, &q, 3) != 21 || q != 21) return 2;
    k = 2;
    if (rep_store(&k, &k, 3) != 8) return 3;   /* s and *k walk together: 2, 4, 8 */
    tally = 0; k = 4;
    if (rep_call(&k, 3) != 12 || tally != 3) return 4;
    tally = 5;
    if (rep_global(4) != 20) return 5;
    if (rep_global_if(5) != 15) return 6;
    k = 3;
    if (rep_ptr_if(&k, 5) != 9) return 7;
    live = 6;
    if (rep_volatile(3) != 18) return 8;
    a[0] = 3; a[1] = 0;
    if (rep_other_word(a, 4) != 12 || a[1] != 12) return 9;
    return 0;
}
