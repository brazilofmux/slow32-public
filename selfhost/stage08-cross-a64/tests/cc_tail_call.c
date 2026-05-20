/* Exercise tail-call elimination patterns the codegen now recognizes.
 *
 *  - sum_to: tail-recursive accumulator (HI_CALL → HI_RET in the same
 *    block).  TCE prevents stack growth.
 *  - swap_call: tail call where caller's arg regs are swapped relative
 *    to callee's — drives the parallel-move marshal's cycle path.
 *  - void_caller: void-returning tail call from a void function (RET
 *    has no value).
 *
 * Expected: 7. */

int sub2(int a, int b) {
    return a - b;
}

int swap_call(int a, int b) {
    /* a in x0, b in x1; we want sub2(b, a) → x0=b, x1=a — a 2-cycle that
     * the greedy parallel-move can't break, so the stack fallback runs. */
    return sub2(b, a);
}

int sum_to(int n, int acc) {
    if (n == 0) return acc;
    return sum_to(n - 1, acc + n);
}

int g;

void void_tail(int x) {
    g = x + 1;
}

void void_caller(int x) {
    void_tail(x * 2);
}

int main(void) {
    int s;
    int r;
    s = sum_to(3, 0);            /* 3+2+1 = 6 */
    r = swap_call(5, 7);         /* sub2(7, 5) = 2 */
    void_caller(2);              /* g = (2*2)+1 = 5 */
    /* 6 + 2 + (g - 6) = 6 + 2 + (-1)? */
    /* s=6, r=2, g=5; want s+r+(g-6)=7 */
    return s + r + (g - 6);
}
