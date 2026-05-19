/* test-regclass.c
 *
 * Small test for the SLOW-32 caller-saved register classification work.
 *
 * Compile with a version of s12cc that has ra_caller_saved_enabled_count = 8.
 *
 * Expected good behavior:
 *   - leaf() should be a true leaf (no calls) → ra_ncsave=0, no save/restore
 *     of any callee-saved registers.  Body should be able to use r3-r10.
 *   - with_call() has a call → values that live across the call (e.g. 'b')
 *     must be allocated from r11-r28. Short-lived values around the call
 *     can still use r3-r10.
 *
 * Look for:
 *   - Absence of stw/ldw sequences for r11+ in the prologue/epilogue of leaf()
 *   - Use of r3..r10 in leaf() and for temporaries in with_call()
 *   - The stats line should show non-zero "caller=" count.
 */

int leaf(int x, int y) {
    /* Pure leaf — many short-lived values.
     * With classification these should preferably land in r3-r10 and
     * we should emit zero callee-save traffic.
     */
    int a = x + 1;
    int b = y * 2;
    int c = a + b;
    int d = c - 3;
    return d;
}

int with_call(int a) {
    /* Value 'b' lives across the call to leaf().
     * It must go into a callee-saved register (r11-r28).
     * 'tmp' and 'ret' around the call can use caller-saved.
     */
    int b = a + 42;
    int tmp = leaf(b, 7);
    int ret = tmp + b;   /* b is still live here */
    return ret;
}

int main(void) {
    return leaf(10, 20) + with_call(5);
}
