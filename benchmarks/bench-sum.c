// Benchmark: Array sum - tight loop, minimal branching
// Tests: Loop overhead, memory access patterns

#include <stdio.h>

#define ARRAY_SIZE 1000
#define ITERATIONS 100000

static int arr[ARRAY_SIZE];

__attribute__((noinline))
int sum_array(int *a, int n) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += a[i];
    }
    return sum;
}

int main() {
    // Initialize array
    for (int i = 0; i < ARRAY_SIZE; i++) {
        arr[i] = i + 1;
    }

    // Run multiple iterations
    // Use volatile to prevent loop optimization
    volatile int total = 0;
    for (volatile int iter = 0; iter < ITERATIONS; iter++) {
        total += sum_array(arr, ARRAY_SIZE);
    }

    // Just verify the computation completed
    int final = total;  // Read volatile once
    printf("Total: %d (%d iterations)\n", final, ITERATIONS);
    printf("PASS\n");
    return 0;
}
