/* Recursion + multiple return paths.  Ackermann is small but
 * exercises a lot of regalloc and call-crossing behavior. */
static int ack(int m, int n) {
    if (m == 0) return n + 1;
    if (n == 0) return ack(m - 1, 1);
    return ack(m - 1, ack(m, n - 1));
}

int main(void) {
    return ack(3, 4);  /* 125; exit code is 125 */
}
