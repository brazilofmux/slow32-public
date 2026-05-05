typedef unsigned long long uint64_t;

static int hit;

static inline uint64_t read_counter(void) {
    uint64_t val;
    __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(val));
    return val;
}

static void target(void) {
    hit = 41;
}

__attribute__((noinline))
static void call_target(void (*fn)(void)) {
    register uint64_t r_cpu     __asm__("x0") = (uint64_t)0;
    register uint64_t r_mem     __asm__("x1") = (uint64_t)0;
    register uint64_t r_block   __asm__("x2") = (uint64_t)fn;
    register uint64_t r_compact __asm__("x3") = (uint64_t)0;

    __asm__ __volatile__(
        "stp x29, x30, [sp, #-16]!\n\t"
        "stp x27, x28, [sp, #-16]!\n\t"
        "stp x25, x26, [sp, #-16]!\n\t"
        "stp x23, x24, [sp, #-16]!\n\t"
        "stp x21, x22, [sp, #-16]!\n\t"
        "stp x19, x20, [sp, #-16]!\n\t"
        "mov x20, x0\n\t"
        "mov x21, x1\n\t"
        "mov x22, x3\n\t"
        "blr x2\n\t"
        "ldp x19, x20, [sp], #16\n\t"
        "ldp x21, x22, [sp], #16\n\t"
        "ldp x23, x24, [sp], #16\n\t"
        "ldp x25, x26, [sp], #16\n\t"
        "ldp x27, x28, [sp], #16\n\t"
        "ldp x29, x30, [sp], #16\n\t"
        :
        : "r" (r_cpu), "r" (r_mem), "r" (r_block), "r" (r_compact)
        : "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11",
          "x12", "x13", "x14", "x15", "x16", "x17", "memory", "cc"
    );
}

int main(void) {
    uint64_t t;
    t = read_counter();
    call_target(target);
    if (hit == 41 && t != 0) return 1;
    return 0;
}
