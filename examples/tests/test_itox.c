// Test integer to hex string conversion
void debug_char(char c);

void puts(const char *str) {
    while (*str) {
        debug_char(*str++);
    }
}

// Convert integer to hex string (returns pointer to static buffer)
char* itox(unsigned int value) {
    static char buffer[11];  // "0x" + 8 hex digits + null
    buffer[0] = '0';
    buffer[1] = 'x';
    buffer[10] = '\0';
    
    // Work backwards from the end
    for (int i = 9; i >= 2; i--) {
        int digit = value & 0xF;
        if (digit < 10) {
            buffer[i] = '0' + digit;
        } else {
            buffer[i] = 'A' + (digit - 10);
        }
        value >>= 4;
    }
    
    return buffer;
}

int main() {
    // Test various values
    puts("Testing itox:\n");
    
    puts("0 = ");
    puts(itox(0));
    puts("\n");
    
    puts("1 = ");
    puts(itox(1));
    puts("\n");
    
    puts("10 = ");
    puts(itox(10));
    puts("\n");
    
    puts("16 = ");
    puts(itox(16));
    puts("\n");
    
    puts("255 = ");
    puts(itox(255));
    puts("\n");
    
    puts("256 = ");
    puts(itox(256));
    puts("\n");
    
    puts("0xDEADBEEF = ");
    puts(itox(0xDEADBEEF));
    puts("\n");
    
    puts("0x12345678 = ");
    puts(itox(0x12345678));
    puts("\n");
    
    puts("0xFFFFFFFF = ");
    puts(itox(0xFFFFFFFF));
    puts("\n");
    
    return 0;
}