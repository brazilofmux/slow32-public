/* Loops with invariant computations the LICM pass should hoist.
 * Also nested loops with cross-iteration dependencies. */
static int matmul_trace(int n) {
    /* Equivalent to: trace of A*B where A[i][j] = i+j and B[i][j] = i*j.
     * No actual array — done in-place to keep this a single-function file. */
    int trace = 0;
    int i;
    for (i = 0; i < n; i++) {
        int sum = 0;
        int k;
        for (k = 0; k < n; k++) {
            int a_ik = i + k;
            int b_ki = k * i;
            sum = sum + a_ik * b_ki;
        }
        trace = trace + sum;
    }
    return trace;
}

int main(void) {
    int acc = 0;
    int n;
    for (n = 1; n <= 8; n++) {
        acc = acc * 13 + matmul_trace(n);
    }
    return acc & 0xff;
}
