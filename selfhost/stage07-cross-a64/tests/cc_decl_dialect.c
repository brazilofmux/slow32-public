/* cc_decl_dialect.c -- GNU/C11 declaration syntax accepted as no-op */

typedef _Atomic(int) atomic_int_t;

struct pair {
    int a;
    int b;
};

static int __attribute__((unused)) global_attr = 3;
static typeof(global_attr) typed_global = 4;

__attribute__((noinline))
static int dialect_add(int x __attribute__((unused)), typeof(x) y)
    __attribute__((noinline));

static int dialect_add(int x __attribute__((unused)), typeof(x) y) {
    _Atomic int local_atomic;
    typeof(((struct pair *)0)->b) member_type;
    int __attribute__((unused)) local_attr;

    local_atomic = x;
    member_type = y;
    local_attr = global_attr;
    return local_atomic + member_type + local_attr + typed_global;
}

int main(void) {
    atomic_int_t a;
    _Atomic(int) b;
    typeof(((struct pair *)0)->a) result;
    int fails;

    fails = 0;
    a = 7;
    b = 8;
    result = dialect_add(a, b);
    if (result != 22) fails = fails + 1;
    return fails == 0;
}
