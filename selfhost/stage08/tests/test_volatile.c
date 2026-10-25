/* volatile reaches the code: every read and write of a volatile object
 * is made as written -- not forwarded from an earlier one, not dropped
 * as dead or unused, not kept in a register.  run-tests.sh counts the
 * loads and stores of each function below against what the source
 * says; the values come out the same either way, which is why the
 * counts are the test.  docs/plans/memory-model.md, step 2a. */

struct gauge {
    int ticks;
    volatile int level;
};

static volatile int flag;
static int plain;

/* Two reads through a pointer to volatile: two loads. */
int read_twice(volatile int *p) {
    return *p + *p;
}

/* Two writes: the first is not dead. */
void write_twice(volatile int *p) {
    *p = 11;
    *p = 12;
}

/* A volatile local stays in memory: two stores and a load. */
int local_box(int a) {
    volatile int box;
    box = a;
    box = a + 1;
    return box;
}

/* A read whose value is not used is still made. */
void poll_once(volatile int *p) {
    *p;
}

/* A volatile member through a non-volatile pointer: two loads;
 * the plain member beside it is read once. */
int member_reads(struct gauge *g) {
    return g->level + g->level + g->ticks + g->ticks;
}

/* The device-register idiom: a cast to pointer-to-volatile. */
int cast_reads(unsigned long base) {
    return *(volatile unsigned char *)base + *(volatile unsigned char *)base;
}

/* A volatile global read in a loop is read every trip. */
int spin(int n) {
    int s = 0;
    while (n > 0) {
        s = s + flag;
        n = n - 1;
    }
    return s;
}

/* The plain neighbour: one load, as before. */
int plain_twice(int *p) {
    return *p + *p;
}

int main(void) {
    int x;
    struct gauge g;
    unsigned char cell;
    x = 4;
    if (read_twice(&x) != 8) return 1;
    write_twice(&x);
    if (x != 12) return 2;
    if (local_box(6) != 7) return 3;
    poll_once(&x);
    g.ticks = 2; g.level = 5;
    if (member_reads(&g) != 14) return 4;
    cell = 9;
    if (cast_reads((unsigned long)&cell) != 18) return 5;
    flag = 3; plain = 8;
    if (spin(4) != 12) return 6;
    if (plain_twice(&plain) != 16) return 7;
    return 0;
}
