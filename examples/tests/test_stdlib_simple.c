#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Declare non-standard functions
char *itoa(int value, char *str, int base);

int main() {
    printf("Testing new stdlib functions\n");
    printf("============================\n\n");
    
    // Test strtoul
    printf("Testing strtoul:\n");
    char *endptr;
    unsigned long val = strtoul("255", &endptr, 10);
    printf("  strtoul(\"255\", 10) = %lu\n", val);
    
    // Test itoa
    printf("\nTesting itoa:\n");
    char buffer[33];
    itoa(255, buffer, 10);
    printf("  255 in base 10: %s\n", buffer);
    itoa(255, buffer, 16);
    printf("  255 in base 16: %s\n", buffer);
    
    // Test calloc
    printf("\nTesting calloc:\n");
    int *arr = calloc(5, sizeof(int));
    if (arr) {
        printf("  Allocated 5 ints: ");
        for (int i = 0; i < 5; i++) {
            printf("%d ", arr[i]);
        }
        printf("\n");
        
        // Test realloc
        printf("\nTesting realloc:\n");
        arr[0] = 10; arr[1] = 20; arr[2] = 30;
        printf("  Original: %d %d %d\n", arr[0], arr[1], arr[2]);
        
        arr = realloc(arr, 7 * sizeof(int));
        if (arr) {
            arr[3] = 40; arr[4] = 50;
            printf("  After realloc: ");
            for (int i = 0; i < 5; i++) {
                printf("%d ", arr[i]);
            }
            printf("\n");
        }
        free(arr);
    }
    
    printf("\nTests completed!\n");
    return 0;
}