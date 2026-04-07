/* Test bitwise operations, shifts, field extraction (like instruction decoding) */

int extract_opcode(int raw) {
    return raw & 0x7F;
}

int extract_rd(int raw) {
    return (raw >> 7) & 0x1F;
}

int extract_rs1(int raw) {
    return (raw >> 15) & 0x1F;
}

int main(int argc, char **argv) {
    int raw;
    int op;
    int rd;
    int rs1;
    /* Encode: opcode=0x10 (ADDI), rd=5, rs1=3 */
    raw = 0x10 | (5 << 7) | (3 << 15);
    op = extract_opcode(raw);
    rd = extract_rd(raw);
    rs1 = extract_rs1(raw);
    return op + rd + rs1;  /* 16 + 5 + 3 = 24 */
}
