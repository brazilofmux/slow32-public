/* slow32dump.c -- selfhost .s32o/.s32x dumper (subset-C port) */

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

int in_bounds(int off, int size, int total) {
    if (off < 0 || size < 0 || total < 0) return 0;
    if (off > total) return 0;
    if (size > total - off) return 0;
    return 1;
}

char *sec_type_name(int t) {
    if (t == S32_SEC_NULL) return "NULL";
    if (t == S32_SEC_CODE) return "CODE";
    if (t == S32_SEC_DATA) return "DATA";
    if (t == S32_SEC_BSS) return "BSS";
    if (t == S32_SEC_RODATA) return "RODATA";
    if (t == S32_SEC_EVT) return "EVT";
    if (t == S32_SEC_TSR) return "TSR";
    if (t == S32_SEC_DEBUG) return "DEBUG";
    if (t == S32_SEC_SYMTAB) return "SYMTAB";
    if (t == S32_SEC_STRTAB) return "STRTAB";
    return "UNKNOWN";
}

char *sym_type_name(int t) {
    if (t == S32O_SYM_NOTYPE) return "NOTYPE";
    if (t == S32O_SYM_FUNC) return "FUNC";
    if (t == S32O_SYM_OBJECT) return "OBJECT";
    if (t == S32O_SYM_SECTION) return "SECTION";
    return "UNKNOWN";
}

char *sym_bind_name(int b) {
    if (b == S32O_BIND_LOCAL) return "LOCAL";
    if (b == S32O_BIND_GLOBAL) return "GLOBAL";
    if (b == S32O_BIND_WEAK) return "WEAK";
    return "UNKNOWN";
}

char *rel_type_name(int t) {
    if (t == S32O_REL_NONE) return "NONE";
    if (t == S32O_REL_32) return "32";
    if (t == S32O_REL_HI20) return "HI20";
    if (t == S32O_REL_LO12) return "LO12";
    if (t == S32O_REL_BRANCH) return "BRANCH";
    if (t == S32O_REL_JAL) return "JAL";
    if (t == S32O_REL_CALL) return "CALL";
    if (t == S32O_REL_PCREL_HI20) return "PCREL_HI20";
    if (t == S32O_REL_PCREL_LO12) return "PCREL_LO12";
    return "UNKNOWN";
}

char *safe_name(char *strtab, int str_sz, int off) {
    if (off < 0 || off >= str_sz) return "<invalid>";
    return strtab + off;
}

void print_usage(char *prog) {
    fdputs("Usage: ", 2);
    fdputs(prog, 2);
    fdputs(" [options] file.s32o|file.s32x\n", 2);
    fdputs("  -h  header\n", 2);
    fdputs("  -S  sections\n", 2);
    fdputs("  -s  symbols\n", 2);
    fdputs("  -r  relocations\n", 2);
    fdputs("  -a  all (default)\n", 2);
}

int main(int argc, char **argv) {
    int show_h;
    int show_sct;
    int show_sym;
    int show_rel;
    int i;
    char *filename;
    int f;
    int size;
    char *buf;
    int magic;

    show_h = 0;
    show_sct = 0;
    show_sym = 0;
    show_rel = 0;
    filename = NULL;

    i = 1;
    while (i < argc) {
        if (argv[i][0] == '-') {
            int j;
            j = 1;
            while (argv[i][j]) {
                if (argv[i][j] == 'h') show_h = 1;
                else if (argv[i][j] == 'S') show_sct = 1;
                else if (argv[i][j] == 's') show_sym = 1;
                else if (argv[i][j] == 'r') show_rel = 1;
                else if (argv[i][j] == 'a') {
                    show_h = 1;
                    show_sct = 1;
                    show_sym = 1;
                    show_rel = 1;
                } else {
                    fdputs("unknown option\n", 2);
                    print_usage(argv[0]);
                    return 1;
                }
                j = j + 1;
            }
        } else {
            filename = argv[i];
        }
        i = i + 1;
    }
    if (!filename) {
        print_usage(argv[0]);
        return 1;
    }
    if (!show_h && !show_sct && !show_sym && !show_rel) {
        show_h = 1;
        show_sct = 1;
        show_sym = 1;
        show_rel = 1;
    }

    f = fdopen_path(filename, "rb");
    if (!f) {
        fdputs("cannot open input\n", 2);
        return 1;
    }
    if (fdseek(f, 0, SEEK_END) != 0) {
        fdclose(f);
        fdputs("fseek failed\n", 2);
        return 1;
    }
    size = fdtell(f);
    if (size < 4) {
        fdclose(f);
        fdputs("file too small\n", 2);
        return 1;
    }
    if (fdseek(f, 0, SEEK_SET) != 0) {
        fdclose(f);
        fdputs("fseek failed\n", 2);
        return 1;
    }
    buf = malloc(size);
    if (!buf) {
        fdclose(f);
        fdputs("out of memory\n", 2);
        return 1;
    }
    if (fdread(buf, 1, size, f) != size) {
        fdclose(f);
        free(buf);
        fdputs("read failed\n", 2);
        return 1;
    }
    fdclose(f);

    magic = rd32(buf);
    if (magic == S32O_MAGIC) {
        int version;
        int endian;
        int machine;
        int flags;
        int nsections;
        int sec_off;
        int nsymbols;
        int sym_off;
        int str_off;
        int str_sz;
        int si;
        char *strtab;

        if (size < SIZEOF_S32O_HEADER) {
            free(buf);
            fdputs("bad s32o header\n", 2);
            return 1;
        }
        version = rd16(buf + 4);
        endian = buf[6] & 255;
        machine = buf[7] & 255;
        flags = rd32(buf + 8);
        nsections = rd32(buf + 12);
        sec_off = rd32(buf + 16);
        nsymbols = rd32(buf + 20);
        sym_off = rd32(buf + 24);
        str_off = rd32(buf + 28);
        str_sz = rd32(buf + 32);

        if (!in_bounds(sec_off, nsections * SIZEOF_S32O_SECTION, size) ||
            !in_bounds(sym_off, nsymbols * SIZEOF_S32O_SYMBOL, size) ||
            !in_bounds(str_off, str_sz, size)) {
            free(buf);
            fdputs("corrupt s32o table bounds\n", 2);
            return 1;
        }
        strtab = buf + str_off;

        fdputs(filename, 1);
        fdputs(": file format s32o-slow32\n", 1);

        if (show_h) {
            fdputs("header: ver=", 1); out_dec(1, version);
            fdputs(" endian=", 1); out_dec(1, endian);
            fdputs(" machine=", 1); out_dec(1, machine);
            fdputs(" flags=0x", 1); out_hex32(1, flags);
            fdputc('\n', 1);
            fdputs("  nsections=", 1); out_dec(1, nsections);
            fdputs(" nsymbols=", 1); out_dec(1, nsymbols);
            fdputs(" str_size=", 1); out_dec(1, str_sz);
            fdputc('\n', 1);
        }
        if (show_sct) {
            fdputs("sections:\n", 1);
            si = 0;
            while (si < nsections) {
                int so;
                int noff;
                int stype;
                int sflags;
                int ssize;
                int soffset;
                int salign;
                int snrel;
                so = sec_off + si * SIZEOF_S32O_SECTION;
                noff = rd32(buf + so + 0);
                stype = rd32(buf + so + 4);
                sflags = rd32(buf + so + 8);
                ssize = rd32(buf + so + 12);
                soffset = rd32(buf + so + 16);
                salign = rd32(buf + so + 20);
                snrel = rd32(buf + so + 24);

                fdputs("  [", 1); out_dec(1, si); fdputs("] ", 1);
                fdputs(safe_name(strtab, str_sz, noff), 1);
                fdputs(" type=", 1); fdputs(sec_type_name(stype), 1);
                fdputs(" size=", 1); out_dec(1, ssize);
                fdputs(" off=0x", 1); out_hex32(1, soffset);
                fdputs(" align=", 1); out_dec(1, salign);
                fdputs(" relocs=", 1); out_dec(1, snrel);
                fdputs(" flags=0x", 1); out_hex32(1, sflags);
                fdputc('\n', 1);
                si = si + 1;
            }
        }
        if (show_sym) {
            int si2;
            fdputs("symbols:\n", 1);
            si2 = 0;
            while (si2 < nsymbols) {
                int so2;
                int noff2;
                int val;
                int sec;
                int typ;
                int bind;
                int ssize2;
                so2 = sym_off + si2 * SIZEOF_S32O_SYMBOL;
                noff2 = rd32(buf + so2 + 0);
                val = rd32(buf + so2 + 4);
                sec = rd16(buf + so2 + 8);
                typ = buf[so2 + 10] & 255;
                bind = buf[so2 + 11] & 255;
                ssize2 = rd32(buf + so2 + 12);
                fdputs("  [", 1); out_dec(1, si2); fdputs("] ", 1);
                fdputs(safe_name(strtab, str_sz, noff2), 1);
                fdputs(" val=0x", 1); out_hex32(1, val);
                fdputs(" sec=", 1); out_dec(1, sec);
                fdputs(" type=", 1); fdputs(sym_type_name(typ), 1);
                fdputs(" bind=", 1); fdputs(sym_bind_name(bind), 1);
                fdputs(" size=", 1); out_dec(1, ssize2);
                fdputc('\n', 1);
                si2 = si2 + 1;
            }
        }
        if (show_rel) {
            int si3;
            fdputs("relocations:\n", 1);
            si3 = 0;
            while (si3 < nsections) {
                int so3;
                int snrel3;
                int roff3;
                int rj;
                so3 = sec_off + si3 * SIZEOF_S32O_SECTION;
                snrel3 = rd32(buf + so3 + 24);
                roff3 = rd32(buf + so3 + 28);
                if (snrel3 > 0) {
                    fdputs("  section[", 1); out_dec(1, si3); fdputs("]\n", 1);
                }
                if (!in_bounds(roff3, snrel3 * SIZEOF_S32O_RELOC, size)) {
                    free(buf);
                    fdputs("corrupt reloc bounds\n", 2);
                    return 1;
                }
                rj = 0;
                while (rj < snrel3) {
                    int ro;
                    int off;
                    int sym;
                    int typ2;
                    int add;
                    ro = roff3 + rj * SIZEOF_S32O_RELOC;
                    off = rd32(buf + ro + 0);
                    sym = rd32(buf + ro + 4);
                    typ2 = rd32(buf + ro + 8);
                    add = rd32(buf + ro + 12);
                    fdputs("    off=0x", 1); out_hex32(1, off);
                    fdputs(" sym=", 1); out_dec(1, sym);
                    fdputs(" type=", 1); fdputs(rel_type_name(typ2), 1);
                    fdputs(" add=", 1); out_dec(1, add);
                    fdputc('\n', 1);
                    rj = rj + 1;
                }
                si3 = si3 + 1;
            }
        }
    } else if (magic == S32X_MAGIC) {
        int version2;
        int endian2;
        int machine2;
        int entry;
        int nsections2;
        int sec_off2;
        int str_off2;
        int str_sz2;
        int flags2;
        int code_limit;
        int data_limit;
        int stack_base;
        int mem_size;
        int heap_base;
        int mmio_base;
        int si4;
        char *strtab2;

        if (size < SIZEOF_S32X_HEADER) {
            free(buf);
            fdputs("bad s32x header\n", 2);
            return 1;
        }
        version2 = rd16(buf + 4);
        endian2 = buf[6] & 255;
        machine2 = buf[7] & 255;
        entry = rd32(buf + 8);
        nsections2 = rd32(buf + 12);
        sec_off2 = rd32(buf + 16);
        str_off2 = rd32(buf + 20);
        str_sz2 = rd32(buf + 24);
        flags2 = rd32(buf + 28);
        code_limit = rd32(buf + 32);
        data_limit = rd32(buf + 40);
        stack_base = rd32(buf + 44);
        mem_size = rd32(buf + 48);
        heap_base = rd32(buf + 52);
        mmio_base = rd32(buf + 60);

        if (!in_bounds(sec_off2, nsections2 * SIZEOF_S32X_SECTION, size) ||
            !in_bounds(str_off2, str_sz2, size)) {
            free(buf);
            fdputs("corrupt s32x table bounds\n", 2);
            return 1;
        }
        strtab2 = buf + str_off2;

        fdputs(filename, 1);
        fdputs(": file format s32x-slow32\n", 1);

        if (show_h) {
            fdputs("header: ver=", 1); out_dec(1, version2);
            fdputs(" endian=", 1); out_dec(1, endian2);
            fdputs(" machine=", 1); out_dec(1, machine2);
            fdputs(" entry=0x", 1); out_hex32(1, entry);
            fdputs(" flags=0x", 1); out_hex32(1, flags2);
            fdputc('\n', 1);
            fdputs("  nsections=", 1); out_dec(1, nsections2);
            fdputs(" mem_size=0x", 1); out_hex32(1, mem_size);
            fdputs(" code_limit=0x", 1); out_hex32(1, code_limit);
            fdputs(" data_limit=0x", 1); out_hex32(1, data_limit);
            fdputc('\n', 1);
            fdputs("  heap=0x", 1); out_hex32(1, heap_base);
            fdputs(" stack=0x", 1); out_hex32(1, stack_base);
            fdputs(" mmio=0x", 1); out_hex32(1, mmio_base);
            fdputc('\n', 1);
        }
        if (show_sct) {
            fdputs("sections:\n", 1);
            si4 = 0;
            while (si4 < nsections2) {
                int so4;
                int noff4;
                int stype4;
                int vaddr4;
                int off4;
                int sz4;
                int msz4;
                int sflags4;
                so4 = sec_off2 + si4 * SIZEOF_S32X_SECTION;
                noff4 = rd32(buf + so4 + 0);
                stype4 = rd32(buf + so4 + 4);
                vaddr4 = rd32(buf + so4 + 8);
                off4 = rd32(buf + so4 + 12);
                sz4 = rd32(buf + so4 + 16);
                msz4 = rd32(buf + so4 + 20);
                sflags4 = rd32(buf + so4 + 24);
                fdputs("  [", 1); out_dec(1, si4); fdputs("] ", 1);
                fdputs(safe_name(strtab2, str_sz2, noff4), 1);
                fdputs(" type=", 1); fdputs(sec_type_name(stype4), 1);
                fdputs(" vaddr=0x", 1); out_hex32(1, vaddr4);
                fdputs(" off=0x", 1); out_hex32(1, off4);
                fdputs(" size=", 1); out_dec(1, sz4);
                fdputs(" mem=", 1); out_dec(1, msz4);
                fdputs(" flags=0x", 1); out_hex32(1, sflags4);
                fdputc('\n', 1);
                si4 = si4 + 1;
            }
        }
        if (show_sym) {
            fdputs("symbols: not emitted in base s32x (see SYMTAB section if present)\n", 1);
        }
        if (show_rel) {
            fdputs("relocations: none in linked s32x\n", 1);
        }
    } else {
        fdputs("unknown file magic\n", 2);
        free(buf);
        return 1;
    }

    free(buf);
    return 0;
}
