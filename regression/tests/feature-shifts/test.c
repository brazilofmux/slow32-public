void debug_char(char c);

int main() {
    int val = 0x12345678;
    
    // Test shift left
    int sll_imm = val << 4;      // 0x23456780
    int sll_reg = val << 8;      // 0x34567800
    
    // Test logical shift right
    unsigned int uval = 0x87654321;
    unsigned int srl_imm = uval >> 4;   // 0x08765432
    unsigned int srl_reg = uval >> 8;   // 0x00876543
    
    // Test arithmetic shift right (sign extension)
    int neg = -256;  // 0xFFFFFF00
    int sra_imm = neg >> 4;  // 0xFFFFFFF0 (-16)
    int sra_reg = neg >> 8;  // 0xFFFFFFFF (-1)
    
    if (sll_imm == 0x23456780 && sll_reg == 0x34567800 &&
        srl_imm == 0x08765432 && srl_reg == 0x00876543 &&
        sra_imm == -16 && sra_reg == -1) {
        debug_char('P');
        debug_char('A');
        debug_char('S');
        debug_char('S');
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