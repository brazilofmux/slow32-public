// switch_demo.c - Demonstrates various switch statement optimizations on SLOW-32
// This file accompanies the article "Jump Tables Without Jump Tables"

// Forward declaration to avoid stdio.h issues
extern int printf(const char *format, ...);

// Example 1: Simple value switch - will use a value table
int value_switch(int x) {
    switch (x) {
        case 1: return 10;
        case 2: return 20;
        case 3: return 30;
        case 4: return 40;
        case 5: return 50;
        default: return -1;
    }
}

// Example 2: Sparse values - will use comparisons
int sparse_switch(int x) {
    switch (x) {
        case 1:    return 100;
        case 10:   return 200;
        case 100:  return 300;
        case 1000: return 400;
        default:   return -1;
    }
}

// Example 3: Complex switch with function calls
void complex_switch(int cmd) {
    switch (cmd) {
        case 1:
            printf("Initializing system...\n");
            printf("System ready.\n");
            break;
        case 2:
            printf("Running diagnostics...\n");
            printf("All systems operational.\n");
            break;
        case 3:
            printf("Shutting down...\n");
            printf("Goodbye!\n");
            break;
        case 4:
            printf("Entering maintenance mode.\n");
            break;
        case 5:
            printf("Restarting services...\n");
            printf("Services restarted.\n");
            break;
        default:
            printf("Unknown command: %d\n", cmd);
            break;
    }
}

// Example 4: Character switch (common in parsers)
int char_switch(char c) {
    switch (c) {
        case 'a': return 1;
        case 'b': return 2;
        case 'c': return 3;
        case 'd': return 4;
        case 'e': return 5;
        case 'f': return 6;
        case 'g': return 7;
        case 'h': return 8;
        default:  return 0;
    }
}

// Example 5: Enum-like switch with contiguous values
int state_machine(int state) {
    switch (state) {
        case 0:  return 1;   // IDLE -> STARTING
        case 1:  return 2;   // STARTING -> RUNNING
        case 2:  return 2;   // RUNNING -> RUNNING (stay)
        case 3:  return 4;   // STOPPING -> STOPPED
        case 4:  return 0;   // STOPPED -> IDLE
        default: return 0;   // Error: go to IDLE
    }
}

// Example 6: Large switch to test scalability
int large_switch(int x) {
    switch (x) {
        case 0:  return 1000;
        case 1:  return 1001;
        case 2:  return 1002;
        case 3:  return 1003;
        case 4:  return 1004;
        case 5:  return 1005;
        case 6:  return 1006;
        case 7:  return 1007;
        case 8:  return 1008;
        case 9:  return 1009;
        case 10: return 1010;
        case 11: return 1011;
        case 12: return 1012;
        case 13: return 1013;
        case 14: return 1014;
        case 15: return 1015;
        case 16: return 1016;
        case 17: return 1017;
        case 18: return 1018;
        case 19: return 1019;
        case 20: return 1020;
        default: return -1;
    }
}

// Benchmark function to test performance
void benchmark_switches() {
    int sum = 0;
    
    printf("Benchmarking switch implementations...\n");
    
    // Test value switch
    for (int i = 0; i < 1000; i++) {
        sum += value_switch(i % 6);
    }
    printf("Value switch sum: %d\n", sum);
    
    // Test sparse switch
    sum = 0;
    int sparse_values[] = {1, 10, 100, 1000, 5000};
    for (int i = 0; i < 1000; i++) {
        sum += sparse_switch(sparse_values[i % 5]);
    }
    printf("Sparse switch sum: %d\n", sum);
    
    // Test char switch
    sum = 0;
    char chars[] = "abcdefghijk";
    for (int i = 0; i < 1000; i++) {
        sum += char_switch(chars[i % 11]);
    }
    printf("Char switch sum: %d\n", sum);
    
    // Test state machine
    int state = 0;
    for (int i = 0; i < 20; i++) {
        state = state_machine(state);
        printf("State %d -> %d\n", i, state);
    }
}

int main() {
    printf("=== SLOW-32 Switch Statement Demo ===\n\n");
    
    // Test individual switches
    printf("value_switch(3) = %d\n", value_switch(3));
    printf("sparse_switch(100) = %d\n", sparse_switch(100));
    printf("char_switch('e') = %d\n", char_switch('e'));
    printf("state_machine(2) = %d\n", state_machine(2));
    printf("large_switch(15) = %d\n\n", large_switch(15));
    
    // Test complex switch
    printf("Testing complex switch:\n");
    for (int i = 0; i <= 6; i++) {
        printf("\nCommand %d:\n", i);
        complex_switch(i);
    }
    
    printf("\n");
    benchmark_switches();
    
    printf("\n=== Demo Complete ===\n");
    return 0;
}