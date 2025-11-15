#include <stdio.h>
#include <time.h>
 
#define N 64
 
// Matrix multiplication benchmark
void matrix_mult(int A[N][N], int B[N][N], int C[N][N]) {
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            C[i][j] = 0;
            for (int k = 0; k < N; k++) {
                C[i][j] += A[i][k] * B[k][j];
            }
        }
    }
}
 
int main() {
    static int A[N][N], B[N][N], C[N][N];
 
    // Initialize matrices
    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            A[i][j] = i + j;
            B[i][j] = i - j;
        }
    }
 
    printf("Starting %dx%d matrix multiplication...\n", N, N);
 
    // Benchmark
    for (int iter = 0; iter < 10; iter++) {
        matrix_mult(A, B, C);
    }
 
    // Verify (prevent optimization)
    printf("Result C[0][0] = %d\n", C[0][0]);
    printf("Result C[N-1][N-1] = %d\n", C[N-1][N-1]);
 
    return 0;
}
