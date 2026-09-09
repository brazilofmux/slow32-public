/* GitHub issue 41: i64 args on a call through a local, typedef, array
 * slot, or function that returns a function pointer.  Dirty the high
 * word first: with a zero there the bug is invisible. */
static long long seen;
static int sink(long long v) { seen = v; return 0; }
typedef int (*fn_t)(long long);
struct S { int (*x)(long long); };
static int (*gfn)(long long);
static int (*methods[2])(long long);
static fn_t getf(int x) { (void)x; return sink; }

int main(void) {
    struct S s;
    fn_t t;
    int (*local)(long long);
    static int (*slot[2])(long long);
    s.x = sink;
    local = sink;
    t = sink;
    gfn = sink;
    methods[0] = sink;
    slot[1] = sink;
    s.x(0x7FFFFFFF00000000LL);
    if (seen != 0x7FFFFFFF00000000LL) return 1;
    local(0);
    if (seen != 0) return 2;
    t(0x123456789LL);
    if (seen != 0x123456789LL) return 3;
    gfn(0);
    if (seen != 0) return 4;
    s.x(0x7FFFFFFF00000000LL);
    methods[0](0);
    if (seen != 0) return 5;
    s.x(0x7FFFFFFF00000000LL);
    slot[1](0);
    if (seen != 0) return 6;
    s.x(0x7FFFFFFF00000000LL);
    getf(0)(0);
    if (seen != 0) return 7;
    return 0;
}
