/* selfhost ISSUES-75: setjmp and longjmp, and what the compiler owes a
 * function that calls setjmp.
 *
 * longjmp brings control back to the setjmp call with the callee-saved
 * registers as they were when setjmp was first called, and memory as it
 * is now.  So a local the compiler kept in a register goes back to an
 * old value -- `volatile` was read and dropped, and a volatile counter
 * counted 0, 0, 0 -- and a spilled value's slot, or a block's frame
 * space, handed to something else once its lifetime ended along the
 * paths the flow graph has, is found changed on the path it does not
 * have: the one back from longjmp.  A function that calls setjmp keeps
 * its named locals in memory and shares no slots.
 *
 * Returns 0, or the number of the first check that fails. */
typedef int jmp_buf[21];
int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

static jmp_buf env;
static jmp_buf handlers[4];
static int nhandlers;

static void deep(int n, int val) {
    volatile int pad[8];
    pad[n & 7] = n;
    if (n == 0) longjmp(env, val);
    deep(n - 1, val);
}

static void fail_until(int tries, int enough) {
    if (tries < enough) longjmp(env, tries + 1);
}

/* volatile locals keep what was last stored in them -- and so does every
 * other named local here, which is more than the standard asks */
static int retry(int limit, volatile int scale) {
    volatile int tries = 0;
    volatile long long total = 0;
    volatile double weight = 0.5;
    int plain = 0;
    int r;

    r = setjmp(env);
    if (r != 0) {
        tries = tries + 1;
        total = total + 10000000000LL * r;
        weight = weight * 2.0;
        plain = plain + 1;
        scale = scale + 1;
    }
    fail_until(tries, limit);
    if (tries != limit) return 1;
    if (limit == 3 && total != 60000000000LL) return 2;
    if (limit == 3 && weight != 4.0) return 3;
    if (plain != limit) return 4;
    if (scale != 10 + limit) return 5;
    return 0;
}

static void clobber(int depth) {
    volatile int fill[16];
    int i;
    for (i = 0; i < 16; i++) fill[i] = 0x5a5a5a5a + depth;
    if (depth > 0) clobber(depth - 1);
    else longjmp(env, fill[3] != 0);
}

/* a local of a block left and come back into still holds its value */
static int blocks(void) {
    int came_back = 0;
    int ok = 0;
    {
        int a = 12345;
        int b[4] = {11, 22, 33, 44};
        if (setjmp(env)) {
            ok = (a == 12345 && b[0] == 11 && b[3] == 44);
            came_back = 1;
        }
    }
    if (!came_back) {
        int c = 777;
        int d[4] = {-1, -2, -3, -4};
        volatile int e = c + d[0];
        clobber(e > 0 ? 6 : 5);
    }
    return ok;
}

static void throw_(int code) {
    longjmp(handlers[nhandlers - 1], code);
}

static int inner_try(int code) {
    int r;
    r = setjmp(handlers[nhandlers++]);
    if (r == 0) {
        if (code) throw_(code);
        nhandlers--;
        return 0;
    }
    nhandlers--;
    if (r > 100) throw_(r + 1);         /* not ours: pass it up */
    return r;
}

static int outer_try(int code) {
    volatile int step = 0;
    int r;
    r = setjmp(handlers[nhandlers++]);
    if (r == 0) {
        step = 1;
        r = inner_try(code);
        step = 2;
        nhandlers--;
        return r * 10 + step;
    }
    nhandlers--;
    return r * 10 + step;
}

int main(void) {
    volatile int count = 0;
    int r;

    r = setjmp(env);
    if (r == 0 && count != 0) return 10;
    if (r == 42 && count != 1) return 11;
    if (r == 1 && count != 2) return 12;
    if (r == -9 && count != 3) return 13;
    count = count + 1;
    if (r == 0) deep(5, 42);
    if (r == 42) deep(40, 0);           /* a jump with 0 arrives as 1 */
    if (r == 1) deep(3, -9);
    if (r != -9 || count != 4) return 14;

    r = retry(3, 10);
    if (r) return 20 + r;
    r = retry(0, 10);
    if (r) return 30 + r;
    if (!blocks()) return 40;
    if (outer_try(0) != 2) return 41;
    if (outer_try(7) != 72) return 42;
    if (outer_try(500) != 5011) return 43;
    if (nhandlers != 0) return 44;
    return 0;
}
