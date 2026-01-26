// Benchmark: Matrix multiplication - O(n^3) nested loops
// Tests: Deep nesting, array indexing, register pressure

#include <stdio.h>

#define N 20  // 20x20 matrix, 8000 multiply-add operations

static int A[N][N];
static int B[N][N];
static int C[N][N];

__attribute__((noinline))
void matrix_multiply(int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) {
            int sum = 0;
            for (int k = 0; k < n; k++) {
                sum += A[i][k] * B[k][j];
            }
            C[i][j] = sum;
        }
    }
}

int main() {
    // Initialize: A = identity, B = sequential
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A[i][j] = (i == j) ? 1 : 0;
            B[i][j] = i * N + j + 1;  // 1, 2, 3, ...
            C[i][j] = 0;
        }
    }

    // Multiply: C = A * B (should equal B since A is identity)
    matrix_multiply(N);

    // Verify: check corner elements
    int pass = 1;

    // C[0][0] should be B[0][0] = 1
    if (C[0][0] != 1) {
        printf("FAIL: C[0][0] = %d, expected 1\n", C[0][0]);
        pass = 0;
    }

    // C[0][N-1] should be B[0][N-1] = N
    if (C[0][N-1] != N) {
        printf("FAIL: C[0][N-1] = %d, expected %d\n", C[0][N-1], N);
        pass = 0;
    }

    // C[N-1][0] should be B[N-1][0] = (N-1)*N + 1
    int expected = (N-1)*N + 1;
    if (C[N-1][0] != expected) {
        printf("FAIL: C[N-1][0] = %d, expected %d\n", C[N-1][0], expected);
        pass = 0;
    }

    // C[N-1][N-1] should be B[N-1][N-1] = N*N
    if (C[N-1][N-1] != N*N) {
        printf("FAIL: C[N-1][N-1] = %d, expected %d\n", C[N-1][N-1], N*N);
        pass = 0;
    }

    if (pass) {
        printf("Matrix %dx%d multiply: PASS\n", N, N);
        return 0;
    }
    return 1;
}
