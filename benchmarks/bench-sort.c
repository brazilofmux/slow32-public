// Benchmark: Bubble sort - many branches, data-dependent
// Tests: Branch prediction, swap patterns

#include <stdio.h>

#define ARRAY_SIZE 1000

static int arr[ARRAY_SIZE];

__attribute__((noinline))
void bubble_sort(int *a, int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (a[j] > a[j + 1]) {
                int temp = a[j];
                a[j] = a[j + 1];
                a[j + 1] = temp;
            }
        }
    }
}

int main() {
    // Initialize with reverse order (worst case)
    for (int i = 0; i < ARRAY_SIZE; i++) {
        arr[i] = ARRAY_SIZE - i;
    }

    bubble_sort(arr, ARRAY_SIZE);

    // Verify sorted
    int pass = 1;
    for (int i = 0; i < ARRAY_SIZE; i++) {
        if (arr[i] != i + 1) {
            printf("FAIL: arr[%d] = %d, expected %d\n", i, arr[i], i + 1);
            pass = 0;
            break;
        }
    }

    printf("Bubble sort %d elements: %s\n", ARRAY_SIZE, pass ? "PASS" : "FAIL");
    return pass ? 0 : 1;
}
