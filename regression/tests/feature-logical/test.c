void debug_char(char c);

int main() {
    int a = 0x55;  // 01010101
    int b = 0x33;  // 00110011
    
    int and_result = a & b;  // 00010001 = 0x11
    int or_result = a | b;   // 01110111 = 0x77
    int xor_result = a ^ b;  // 01100110 = 0x66
    
    // Test immediate forms
    int andi = a & 0x0F;     // 00000101 = 0x05
    int ori = a | 0x80;      // 11010101 = 0xD5
    int xori = a ^ 0xFF;     // 10101010 = 0xAA
    
    // Verify all results
    if (and_result == 0x11 && or_result == 0x77 && xor_result == 0x66 &&
        andi == 0x05 && ori == 0xD5 && xori == 0xAA) {
        debug_char('O');
        debug_char('K');
        debug_char('\n');
    } else {
        debug_char('F');
        debug_char('A');
        debug_char('I');
        debug_char('L');
        debug_char('\n');
    }
    
    return 0;
}