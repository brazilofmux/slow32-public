// Test integer to hex string conversion without static
void debug_char(char c);

void puts(const char *str) {
    while (*str) {
        debug_char(*str++);
    }
}

// Convert integer to hex string (buffer provided by caller)
void itox(unsigned int value, char* buffer) {
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
}

int main() {
    char buffer[11];  // "0x" + 8 hex digits + null
    
    // Test various values
    puts("Testing itox:\n");
    
    puts("0 = ");
    itox(0, buffer);
    puts(buffer);
    puts("\n");
    
    puts("1 = ");
    itox(1, buffer);
    puts(buffer);
    puts("\n");
    
    puts("10 = ");
    itox(10, buffer);
    puts(buffer);
    puts("\n");
    
    puts("16 = ");
    itox(16, buffer);
    puts(buffer);
    puts("\n");
    
    puts("255 = ");
    itox(255, buffer);
    puts(buffer);
    puts("\n");
    
    puts("256 = ");
    itox(256, buffer);
    puts(buffer);
    puts("\n");
    
    puts("0xDEADBEEF = ");
    itox(0xDEADBEEF, buffer);
    puts(buffer);
    puts("\n");
    
    puts("0x12345678 = ");
    itox(0x12345678, buffer);
    puts(buffer);
    puts("\n");
    
    puts("0xFFFFFFFF = ");
    itox(0xFFFFFFFF, buffer);
    puts(buffer);
    puts("\n");
    
    return 0;
}