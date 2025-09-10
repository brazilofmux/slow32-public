// Test constant loading
extern void debug_char(char c);

void print_value(unsigned int val) {
    // Just print something to show we got the value
    if (val == 0x12345678) {
        debug_char('Y');  // Yes, got the right value
    } else {
        debug_char('N');  // No, wrong value
    }
    debug_char('\n');
}

int main() {
    // Test various constants
    print_value(0);           // Should print N
    print_value(1);           // Should print N  
    print_value(0xFF);        // Should print N
    print_value(0x1234);      // Should print N
    print_value(0x12340000);  // Should print N
    print_value(0x12345678);  // Should print Y
    
    return 0;
}