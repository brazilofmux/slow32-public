/* True-variadic libc printf / fprintf / snprintf -- now that cc-x64
 * and cc-a64 implement callee-side va_*, the libc shims are real
 * variadic functions instead of fixed-arity-with-eight-slots.  This
 * test exercises:
 *   - >8 ints to snprintf (was clamped to the old 8-slot cap)
 *   - args spanning the reg/stack ABI boundary (5..7 reg slots on x64
 *     / a64, then stack)
 *   - mixed types: int / long long / pointer / char / string
 *   - printf(...) direct vs snprintf->vsnprintf callee handoff (the
 *     latter triggers the reg→stack transition inside a callee that
 *     received the va_list, which is the case the codegen had to fix
 *     by stashing the stack-overflow base in the save area). */
#include <stdio.h>

int main(void) {
    char buf[128];
    int n;

    /* 12 ints: exceeds the old 8-slot cap and spans reg/stack boundary
     * on both targets via snprintf->vsnprintf callee handoff. */
    n = snprintf(buf, sizeof(buf), "[%d %d %d %d %d %d %d %d %d %d %d %d]",
                 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12);
    printf("a: %s n=%d\n", buf, n);

    /* Mixed types in a single call: %lld and %s alternating. */
    long long big = 1234567890123LL;
    long long neg = -987654321098LL;
    n = snprintf(buf, sizeof(buf), "big=%lld neg=%lld name=%s end=%d",
                 big, neg, "hello", 99);
    printf("b: %s n=%d\n", buf, n);

    /* printf direct with >7 args (forces stack-vararg path on a64,
     * also spans on x64). */
    printf("c: %d %d %d %d %d %d %d %d %d\n",
           10, 20, 30, 40, 50, 60, 70, 80, 90);

    /* Char + width-padded int + plain string.  (Left-justify flag
     * `%-Ns` is not implemented in our snp_core; only right-justified
     * width works.) */
    n = snprintf(buf, sizeof(buf), "<%c|%5d|%s>", 'X', 42, "ab");
    printf("d: %s n=%d\n", buf, n);

    return 0;
}
