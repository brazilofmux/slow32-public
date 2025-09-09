// Test rotate expansion
extern void debug_char(char c);

// Simple rotate left by 1
unsigned int rotl1(unsigned int x) {
    return (x << 1) | (x >> 31);
}

// Print hex digit
void print_hex_digit(int digit) {
    if (digit < 10) {
        debug_char('0' + digit);
    } else {
        debug_char('A' + (digit - 10));
    }
}

// Print 32-bit value as hex
void print_hex(unsigned int value) {
    debug_char('0');
    debug_char('x');
    
    for (int i = 7; i >= 0; i--) {
        int nibble = (value >> (i * 4)) & 0xF;
        print_hex_digit(nibble);
    }
    debug_char('\n');
}

int main() {
    unsigned int val = 0x12345678;
    
    // Show original
    print_hex(val);
    
    // Rotate left by 1
    val = rotl1(val);
    print_hex(val);  // Should be 0x2468ACF0
    
    // Rotate again
    val = rotl1(val);  
    print_hex(val);  // Should be 0x48D159E0
    
    return 0;
}