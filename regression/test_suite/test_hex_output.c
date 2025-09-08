// Test hex output conversion
// This tests integer to hex string conversion and displays a calculated checksum

extern void debug_char(char c);

// Simple checksum function
unsigned int calculate_checksum(const char *data, int len) {
    unsigned int sum = 0x12345678;  // Initial value
    for (int i = 0; i < len; i++) {
        sum = sum ^ ((unsigned int)data[i] << (i & 3) * 8);
        sum = (sum << 1) | (sum >> 31);  // Rotate left by 1
    }
    return sum;
}

// Convert a nibble (4 bits) to hex character
char nibble_to_hex(int n) {
    n = n & 0xF;
    if (n < 10) {
        return '0' + n;
    } else {
        return 'A' + (n - 10);
    }
}

// Display a 32-bit value as hex
void print_hex(unsigned int value) {
    debug_char('0');
    debug_char('x');
    
    // Print 8 hex digits (32 bits = 8 nibbles)
    for (int i = 7; i >= 0; i--) {
        int nibble = (value >> (i * 4)) & 0xF;
        debug_char(nibble_to_hex(nibble));
    }
    debug_char('\n');
}

int main() {
    // Test data
    const char test_string[] = "Hello, SLOW32!";
    
    // Calculate checksum
    unsigned int checksum = calculate_checksum(test_string, 14);
    
    // Display result in hex
    print_hex(checksum);
    
    // Also test with a simple known value
    print_hex(0xDEADBEEF);
    
    // Test nibble conversion
    for (int i = 0; i < 16; i++) {
        debug_char(nibble_to_hex(i));
    }
    debug_char('\n');
    
    return 0;
}