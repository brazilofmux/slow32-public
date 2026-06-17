/* slow32dis.c -- selfhost disassembler for .s32x/.s32o code sections */

#include "s32util_min.h"

int rd16(char *p) {
    int b0;
    int b1;
    b0 = p[0] & 255;
    b1 = p[1] & 255;
    return b0 | (b1 << 8);
}

int rd32(char *p) {
    int b0;
    int b1;
    int b2;
    int b3;
    b0 = p[0] & 255;
    b1 = p[1] & 255;
    b2 = p[2] & 255;
    b3 = p[3] & 255;
    return b0 | (b1 << 8) | (b2 << 16) | (b3 << 24);
}

void out_hex_nibble(int f, int v) {
    v = v & 15;
    if (v < 10) fdputc('0' + v, f);
    else fdputc('A' + (v - 10), f);
}

void out_hex32(int f, int v) {
    out_hex_nibble(f, (v >> 28) & 15);
    out_hex_nibble(f, (v >> 24) & 15);
    out_hex_nibble(f, (v >> 20) & 15);
    out_hex_nibble(f, (v >> 16) & 15);
    out_hex_nibble(f, (v >> 12) & 15);
    out_hex_nibble(f, (v >> 8) & 15);
    out_hex_nibble(f, (v >> 4) & 15);
    out_hex_nibble(f, v & 15);
}

void out_dec(int f, int v) {
    if (v < 0) {
        fdputc('-', f);
        if (v == -2147483648) {
            fdputs("2147483648", f);
            return;
        }
        v = -v;
    }
    fdputuint(f, v);
}

int signext(int v, int bits) {
    int s;
    s = 1 << (bits - 1);
    if (v & s) v = v | (~((1 << bits) - 1));
    return v;
}

int in_bounds(int off, int size, int total) {
    if (off < 0 || size < 0 || total < 0) return 0;
    if (off > total) return 0;
    if (size > total - off) return 0;
    return 1;
}

char *reg_name(int r) {
    static char buf0[8];
    static char buf1[8];
    static char buf2[8];
    static char buf3[8];
    static int idx;
    char *b;
    if (r == 0) return "zero";
    if (r == 29) return "sp";
    if (r == 30) return "fp";
    if (r == 31) return "lr";
    if (idx == 0) b = buf0;
    else if (idx == 1) b = buf1;
    else if (idx == 2) b = buf2;
    else b = buf3;
    idx = (idx + 1) & 3;
    b[0] = 'r';
    if (r >= 10) {
        b[1] = '0' + (r / 10);
        b[2] = '0' + (r % 10);
        b[3] = 0;
    } else {
        b[1] = '0' + r;
        b[2] = 0;
    }
    return b;
}

char *op_name(int op) {
    if (op == 0x00) return "add";
    if (op == 0x01) return "sub";
    if (op == 0x02) return "xor";
    if (op == 0x03) return "or";
    if (op == 0x04) return "and";
    if (op == 0x05) return "sll";
    if (op == 0x06) return "srl";
    if (op == 0x07) return "sra";
    if (op == 0x08) return "slt";
    if (op == 0x09) return "sltu";
    if (op == 0x0A) return "mul";
    if (op == 0x0B) return "mulh";
    if (op == 0x0C) return "div";
    if (op == 0x0D) return "rem";
    if (op == 0x0E) return "seq";
    if (op == 0x0F) return "sne";
    if (op == 0x10) return "addi";
    if (op == 0x11) return "ori";
    if (op == 0x12) return "andi";
    if (op == 0x13) return "slli";
    if (op == 0x14) return "srli";
    if (op == 0x15) return "srai";
    if (op == 0x16) return "slti";
    if (op == 0x17) return "sltiu";
    if (op == 0x18) return "sgt";
    if (op == 0x19) return "sgtu";
    if (op == 0x1A) return "sle";
    if (op == 0x1B) return "sleu";
    if (op == 0x1C) return "sge";
    if (op == 0x1D) return "sgeu";
    if (op == 0x1E) return "xori";
    if (op == 0x1F) return "mulhu";
    if (op == 0x20) return "lui";
    if (op == 0x30) return "ldb";
    if (op == 0x31) return "ldh";
    if (op == 0x32) return "ldw";
    if (op == 0x33) return "ldbu";
    if (op == 0x34) return "ldhu";
    if (op == 0x38) return "stb";
    if (op == 0x39) return "sth";
    if (op == 0x3A) return "stw";
    if (op == 0x3F) return "assert_eq";
    if (op == 0x40) return "jal";
    if (op == 0x41) return "jalr";
    if (op == 0x48) return "beq";
    if (op == 0x49) return "bne";
    if (op == 0x4A) return "blt";
    if (op == 0x4B) return "bge";
    if (op == 0x4C) return "bltu";
    if (op == 0x4D) return "bgeu";
    if (op == 0x50) return "nop";
    if (op == 0x51) return "yield";
    if (op == 0x52) return "debug";
    if (op >= 0x53 && op <= 0x78) return "fop";
    if (op == 0x7F) return "halt";
    return "unknown";
}

void disassemble_inst(int pc, int inst) {
    int op;
    int rd;
    int rs1;
    int rs2;
    int imm_i;
    int imm_s;
    int imm_b;
    int imm_u;
    int imm_j;

    op = inst & 0x7F;
    rd = (inst >> 7) & 0x1F;
    rs1 = (inst >> 15) & 0x1F;
    rs2 = (inst >> 20) & 0x1F;
    imm_i = signext((inst >> 20) & 0xFFF, 12);
    imm_s = signext(((inst >> 7) & 0x1F) | (((inst >> 25) & 0x7F) << 5), 12);
    imm_b = signext((((inst >> 8) & 0xF) << 1) |
                    (((inst >> 25) & 0x3F) << 5) |
                    (((inst >> 7) & 1) << 11) |
                    (((inst >> 31) & 1) << 12), 13);
    imm_u = inst & 0xFFFFF000;
    imm_j = signext((((inst >> 21) & 0x3FF) << 1) |
                    (((inst >> 20) & 1) << 11) |
                    (((inst >> 12) & 0xFF) << 12) |
                    (((inst >> 31) & 1) << 20), 21);

    out_hex32(1, pc);
    fdputs(": ", 1);
    out_hex32(1, inst);
    fdputs("  ", 1);
    fdputs(op_name(op), 1);
    fdputs(" ", 1);

    if (op == 0x20) {
        fdputs(reg_name(rd), 1);
        fdputs(", 0x", 1);
        out_hex32(1, imm_u);
    } else if (op == 0x40) {
        fdputs(reg_name(rd), 1);
        fdputs(", 0x", 1);
        out_hex32(1, pc + imm_j);
    } else if (op == 0x41 || (op >= 0x10 && op <= 0x17) || op == 0x1E) {
        fdputs(reg_name(rd), 1);
        fdputs(", ", 1);
        fdputs(reg_name(rs1), 1);
        fdputs(", ", 1);
        out_dec(1, imm_i);
    } else if (op >= 0x30 && op <= 0x34) {
        fdputs(reg_name(rd), 1);
        fdputs(", ", 1);
        out_dec(1, imm_i);
        fdputs("(", 1);
        fdputs(reg_name(rs1), 1);
        fdputs(")", 1);
    } else if (op >= 0x38 && op <= 0x3A) {
        fdputs(reg_name(rs2), 1);
        fdputs(", ", 1);
        out_dec(1, imm_s);
        fdputs("(", 1);
        fdputs(reg_name(rs1), 1);
        fdputs(")", 1);
    } else if (op >= 0x48 && op <= 0x4D) {
        fdputs(reg_name(rs1), 1);
        fdputs(", ", 1);
        fdputs(reg_name(rs2), 1);
        fdputs(", 0x", 1);
        out_hex32(1, pc + imm_b);
    } else if (op == 0x50 || op == 0x51 || op == 0x7F) {
    } else if (op == 0x52) {
        fdputs(reg_name(rs1), 1);
    } else {
        fdputs(reg_name(rd), 1);
        fdputs(", ", 1);
        fdputs(reg_name(rs1), 1);
        fdputs(", ", 1);
        fdputs(reg_name(rs2), 1);
    }
    fdputc('\n', 1);
}

void disassemble_code(char *buf, int off, int sz, int base, char *name) {
    int p;
    fdputs("\nsection ", 1);
    fdputs(name, 1);
    fdputs(" @0x", 1);
    out_hex32(1, base);
    fdputc('\n', 1);
    p = 0;
    while (p + 4 <= sz) {
        int inst;
        inst = rd32(buf + off + p);
        disassemble_inst(base + p, inst);
        p = p + 4;
    }
}

int main(int argc, char **argv) {
    int f;
    int size;
    char *buf;
    int magic;
    char *filename;

    if (argc < 2) {
        fdputs("Usage: slow32dis file.s32x|file.s32o\n", 2);
        return 1;
    }
    filename = argv[1];

    f = fdopen_path(filename, "rb");
    if (!f) {
        fdputs("cannot open input\n", 2);
        return 1;
    }
    if (fdseek(f, 0, SEEK_END) != 0) {
        fdclose(f);
        return 1;
    }
    size = fdtell(f);
    if (size < 4) {
        fdclose(f);
        return 1;
    }
    if (fdseek(f, 0, SEEK_SET) != 0) {
        fdclose(f);
        return 1;
    }
    buf = malloc(size);
    if (!buf) {
        fdclose(f);
        return 1;
    }
    if (fdread(buf, 1, size, f) != size) {
        fdclose(f);
        free(buf);
        return 1;
    }
    fdclose(f);

    magic = rd32(buf);
    if (magic == S32X_MAGIC) {
        int nsections;
        int sec_off;
        int str_off;
        int str_sz;
        int i;
        char *strtab;
        if (size < SIZEOF_S32X_HEADER) {
            free(buf);
            return 1;
        }
        nsections = rd32(buf + 12);
        sec_off = rd32(buf + 16);
        str_off = rd32(buf + 20);
        str_sz = rd32(buf + 24);
        if (!in_bounds(sec_off, nsections * SIZEOF_S32X_SECTION, size) ||
            !in_bounds(str_off, str_sz, size)) {
            free(buf);
            return 1;
        }
        strtab = buf + str_off;
        i = 0;
        while (i < nsections) {
            int so;
            int noff;
            int stype;
            int vaddr;
            int off;
            int sz;
            so = sec_off + i * SIZEOF_S32X_SECTION;
            noff = rd32(buf + so + 0);
            stype = rd32(buf + so + 4);
            vaddr = rd32(buf + so + 8);
            off = rd32(buf + so + 12);
            sz = rd32(buf + so + 16);
            if (stype == S32_SEC_CODE && in_bounds(off, sz, size)) {
                char *nm;
                if (noff >= 0 && noff < str_sz) nm = strtab + noff;
                else nm = "<invalid>";
                disassemble_code(buf, off, sz, vaddr, nm);
            }
            i = i + 1;
        }
    } else if (magic == S32O_MAGIC) {
        int nsections2;
        int sec_off2;
        int str_off2;
        int str_sz2;
        int i2;
        char *strtab2;
        if (size < SIZEOF_S32O_HEADER) {
            free(buf);
            return 1;
        }
        nsections2 = rd32(buf + 12);
        sec_off2 = rd32(buf + 16);
        str_off2 = rd32(buf + 28);
        str_sz2 = rd32(buf + 32);
        if (!in_bounds(sec_off2, nsections2 * SIZEOF_S32O_SECTION, size) ||
            !in_bounds(str_off2, str_sz2, size)) {
            free(buf);
            return 1;
        }
        strtab2 = buf + str_off2;
        i2 = 0;
        while (i2 < nsections2) {
            int so2;
            int noff2;
            int stype2;
            int off2;
            int sz2;
            so2 = sec_off2 + i2 * SIZEOF_S32O_SECTION;
            noff2 = rd32(buf + so2 + 0);
            stype2 = rd32(buf + so2 + 4);
            sz2 = rd32(buf + so2 + 12);
            off2 = rd32(buf + so2 + 16);
            if (stype2 == S32_SEC_CODE && in_bounds(off2, sz2, size)) {
                char *nm2;
                if (noff2 >= 0 && noff2 < str_sz2) nm2 = strtab2 + noff2;
                else nm2 = "<invalid>";
                disassemble_code(buf, off2, sz2, 0, nm2);
            }
            i2 = i2 + 1;
        }
    } else {
        fdputs("unknown file format\n", 2);
        free(buf);
        return 1;
    }

    free(buf);
    return 0;
}
