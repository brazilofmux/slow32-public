/* GitHub issue 41: i64 args on a call through a local or typedef'd
 * function pointer must go as a pair.  A member call already converted;
 * `int (*f)(long long); f(0)` and `typedef ...; t(0)` did not.  Dirty
 * the high word first: with a zero there the bug is invisible. */
static long long seen;
static int sink(long long v) { seen = v; return 0; }
typedef int (*fn_t)(long long);
struct S { int (*x)(long long); };
static int (*gfn)(long long);

int main(void) {
    struct S s;
    fn_t t;
    int (*local)(long long);
    s.x = sink;
    local = sink;
    t = sink;
    gfn = sink;
    s.x(0x7FFFFFFF00000000LL);
    if (seen != 0x7FFFFFFF00000000LL) return 1;
    local(0);
    if (seen != 0) return 2;
    t(0x123456789LL);
    if (seen != 0x123456789LL) return 3;
    gfn(0);
    if (seen != 0) return 4;
    local(0x7FFFFFFF00000000LL);
    if (seen != 0x7FFFFFFF00000000LL) return 5;
    t(0);
    if (seen != 0) return 6;
    return 0;
}
