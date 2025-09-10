// switch_patterns.c - Different switch patterns for SLOW-32 article
// No external dependencies - pure computation

// Pattern 1: Dense value table (best case for optimization)
int dense_switch(int x) {
    switch (x) {
        case 0: return 100;
        case 1: return 101;
        case 2: return 102;
        case 3: return 103;
        case 4: return 104;
        case 5: return 105;
        default: return -1;
    }
}

// Pattern 2: Sparse values (requires comparisons)
int sparse_switch(int x) {
    switch (x) {
        case 1:    return 10;
        case 10:   return 20;
        case 100:  return 30;
        case 1000: return 40;
        default:   return -1;
    }
}

// Pattern 3: Gaps in sequence (LLVM fills gaps with defaults)
int gap_switch(int x) {
    switch (x) {
        case 1: return 10;
        case 2: return 20;
        case 3: return 30;
        // case 4 missing
        case 5: return 50;
        case 6: return 60;
        default: return -1;
    }
}

// Pattern 4: Character/byte switch (common in parsers)
int char_switch(int c) {
    switch (c) {
        case 'a': return 1;
        case 'b': return 2;
        case 'c': return 3;
        case 'd': return 4;
        case 'e': return 5;
        default:  return 0;
    }
}

// Pattern 5: Large contiguous switch (tests scalability)
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
        default: return -1;
    }
}

// Pattern 6: Binary pattern (powers of 2)
int binary_switch(int x) {
    switch (x) {
        case 1:   return 10;
        case 2:   return 20;
        case 4:   return 40;
        case 8:   return 80;
        case 16:  return 160;
        case 32:  return 320;
        case 64:  return 640;
        case 128: return 1280;
        default:  return 0;
    }
}

// Pattern 7: State machine (typical embedded use case)
int state_machine(int state, int input) {
    switch (state) {
        case 0:  // IDLE
            return (input > 0) ? 1 : 0;
        case 1:  // STARTING
            return 2;
        case 2:  // RUNNING
            return (input == 0) ? 3 : 2;
        case 3:  // STOPPING
            return 0;
        default:
            return 0;  // Error -> IDLE
    }
}

// Main function to test all patterns
int main() {
    int sum = 0;
    
    // Test dense switch
    sum += dense_switch(3);
    
    // Test sparse switch
    sum += sparse_switch(100);
    
    // Test gap switch
    sum += gap_switch(5);
    
    // Test char switch
    sum += char_switch('c');
    
    // Test large switch
    sum += large_switch(10);
    
    // Test binary switch
    sum += binary_switch(16);
    
    // Test state machine
    int state = 0;
    state = state_machine(state, 1);  // IDLE -> STARTING
    state = state_machine(state, 0);  // STARTING -> RUNNING
    state = state_machine(state, 0);  // RUNNING -> STOPPING
    sum += state;
    
    return sum;  // Should return specific value if all work correctly
}