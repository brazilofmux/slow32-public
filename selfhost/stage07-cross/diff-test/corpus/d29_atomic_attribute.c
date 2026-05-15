/* C11 _Atomic and GNU __attribute__ accepted as no-op qualifiers --
 * underlying types and storage are unchanged, the program runs
 * identically to its plain-int sibling.  Verifies the parser accepts
 * these in prefix, parameter, and suffix positions. */

typedef _Atomic(int) atomic_int_t;

static int __attribute__((unused)) g_unused = 99;
static _Atomic int g_atomic = 7;

__attribute__((noinline))
static int dialect_sum(int x __attribute__((unused)), _Atomic(int) y, atomic_int_t z)
    __attribute__((noinline));

static int dialect_sum(int x __attribute__((unused)), _Atomic(int) y, atomic_int_t z) {
    _Atomic int  local_a = y * 3;
    atomic_int_t local_b = z * 5;
    int __attribute__((unused)) unused_local;
    return local_a + local_b + g_atomic;
}

int main(void) {
    atomic_int_t a;
    _Atomic(int) b;
    int acc = 0;
    int i;

    a = 1;
    b = 2;
    for (i = 0; i < 20; i++) {
        acc = acc * 3 + dialect_sum(i, a + i, b + i * 2);
        a   = a + 1;
        b   = b + 3;
        g_atomic = g_atomic + 1;
    }
    return acc & 0xff;
}
