/* HOSTLEG -- setjmp and longjmp: the value that comes back, a jump out
 * of nested calls, a second jump to the same place, that the registers
 * the caller keeps across a call are the caller's again, that volatile
 * locals keep what was last stored in them, and that a local not
 * changed since setjmp has its value -- in a block left and come back
 * into, too. */
#include <stdio.h>
#include <setjmp.h>

static jmp_buf env;
static jmp_buf inner_env;
static int depth_reached;

static void deep(int n, int val) {
    volatile int pad[8];
    pad[n & 7] = n;
    depth_reached = n;
    if (n == 0) longjmp(env, val);
    deep(n - 1, val);
    printf("NOT REACHED %d\n", pad[0]);
}

static int churn(int a) {
    /* enough live values that the callee-saved registers are all in use */
    int b = a * 3 + 1, c = b * 5 + 2, d = c * 7 + 3, e = d * 11 + 4, f = e * 13 + 5, g = f * 17 + 6;
    int h = g * 19 + 7, i = h * 23 + 8, j = i * 29 + 9, k = j * 31 + 10, l = k * 37 + 11, m = l * 41 + 12;
    if (a == 99) longjmp(inner_env, 7);
    return a ^ b ^ c ^ d ^ e ^ f ^ g ^ h ^ i ^ j ^ k ^ l ^ m;
}

static int keeps(int seed) {
    int a = seed + 1, b = seed + 2, c = seed + 3, d = seed + 4, e = seed + 5, f = seed + 6;
    int g = seed + 7, h = seed + 8, i = seed + 9, j = seed + 10, k = seed + 11, l = seed + 12;
    int r;
    r = setjmp(inner_env);
    if (r == 0) {
        churn(1);
        churn(99);
        printf("NOT REACHED\n");
    }
    /* none of these changed after setjmp: they have their values */
    return r * 1000000 + a + b * 2 + c * 3 + d * 4 + e * 5 + f * 6 + g * 7 + h * 8 + i * 9 + j * 10 + k * 11 + l * 12;
}

static jmp_buf retry_env;

static void fail_until(int tries, int enough) {
    if (tries < enough) longjmp(retry_env, tries + 1);
}

/* volatile locals of every kind keep what was last stored in them */
static int retry(int limit, volatile int scale) {
    volatile int tries = 0;
    volatile long long total = 0;
    volatile double weight = 0.5;
    const char *volatile where = "start";
    volatile struct { int a; int b; } pair = {1, 2};
    int r;

    r = setjmp(retry_env);
    if (r != 0) {
        tries = tries + 1;
        total = total + 10000000000LL * r;
        weight = weight * 2.0;
        where = "retried";
        pair.b = pair.b + pair.a;
        scale = scale + 1;
    }
    fail_until(tries, limit);
    printf("retry: tries %d total %lld weight %.1f where %s pair %d/%d scale %d\n",
           tries, total, weight, where, pair.a, pair.b, scale);
    return tries;
}

static jmp_buf block_env;
static void clobber(int depth) {
    volatile int fill[16];
    int i;
    for (i = 0; i < 16; i++) fill[i] = 0x5a5a5a5a + depth;
    if (depth > 0) clobber(depth - 1);
    else longjmp(block_env, fill[3] != 0);
}

/* a local of a block that control has left, and come back into by
 * longjmp, still holds what it held: it was not changed after setjmp */
static void blocks(void) {
    int came_back = 0;
    {
        int a = 12345;
        int b[4] = {11, 22, 33, 44};
        if (setjmp(block_env)) {
            printf("blocks: back in the first block with a = %d, b = %d %d %d %d\n", a, b[0], b[1], b[2], b[3]);
            came_back = 1;
        }
    }
    if (!came_back) {
        int c = 777;
        int d[4] = {-1, -2, -3, -4};
        volatile int e = c + d[0];
        clobber(e > 0 ? 6 : 5);
    }
    printf("blocks: done\n");
}

/* the pattern libraries build exceptions from: a stack of jump buffers */
static jmp_buf handlers[4];
static int nhandlers;

static void throw_(int code) {
    longjmp(handlers[nhandlers - 1], code);
}

static int inner_try(int code) {
    volatile int cleaned = 0;
    int r;
    r = setjmp(handlers[nhandlers++]);
    if (r == 0) {
        if (code) throw_(code);
        nhandlers--;
        return 0;
    }
    nhandlers--;
    cleaned = 1;
    if (r > 100) throw_(r + cleaned);        /* not ours: pass it up */
    return r;
}

static void outer_try(int code) {
    volatile int step = 0;
    int r;
    r = setjmp(handlers[nhandlers++]);
    if (r == 0) {
        step = 1;
        r = inner_try(code);
        step = 2;
        nhandlers--;
        printf("try(%d): inner returned %d, step %d\n", code, r, step);
        return;
    }
    nhandlers--;
    printf("try(%d): caught %d in the outer handler, step %d\n", code, r, step);
}

int main(void) {
    volatile int count = 0;
    int r;

    r = setjmp(env);
    printf("setjmp returned %d (count %d, depth %d)\n", r, count, depth_reached);
    count = count + 1;
    if (r == 0) deep(5, 42);
    if (r == 42) deep(40, 0);            /* a jump with 0 arrives as 1 */
    if (r == 1) deep(3, -9);
    if (r == -9) printf("three jumps to one setjmp\n");

    printf("keeps %d\n", keeps(10));
    printf("keeps %d\n", keeps(-500));

    printf("retry returned %d\n", retry(3, 10));
    printf("retry returned %d\n", retry(0, 20));
    blocks();
    outer_try(0);
    outer_try(7);
    outer_try(500);

    switch (setjmp(env)) {
    case 0:  printf("in a switch: first\n"); longjmp(env, 3);
    case 3:  printf("in a switch: jumped\n"); break;
    default: printf("in a switch: WRONG\n");
    }
    return 0;
}
