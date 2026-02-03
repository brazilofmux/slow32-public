#include <stdio.h>
#include <string.h>

// Packed struct to force misalignment
struct __attribute__((packed)) Unaligned {
    char pad;
    double val;
};

// Global to ensure it's not optimized away
volatile struct Unaligned g_unaligned = {0, 0.0};

int main() {
    struct Unaligned u;
    u.pad = 'A';
    u.val = 123.456;

    // Test Store (stack)
    // The compiler should emit byte/halfword stores or unaligned store sequence
    // depending on the alignment analysis.
    
    // Test Load (stack)
    double loaded = u.val;
    
    // Check value
    if (loaded == 123.456) {
        printf("Stack Unaligned Load: PASS\n");
    } else {
        printf("Stack Unaligned Load: FAIL (got %d)\n", (int)loaded);
    }

    // Test Global Store
    g_unaligned.val = 789.012;
    
    // Test Global Load
    double glob_loaded = g_unaligned.val;
    
    if (glob_loaded == 789.012) {
        printf("Global Unaligned Load: PASS\n");
    } else {
        printf("Global Unaligned Load: FAIL (got %d)\n", (int)glob_loaded);
    }

    // Check padding wasn't overwritten
    if (u.pad == 'A') {
        printf("Padding Check: PASS\n");
    } else {
        printf("Padding Check: FAIL\n");
    }

    return 0;
}
