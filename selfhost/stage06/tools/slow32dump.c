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
    if (v < 10) fputc('0' + v, f);
    else fputc('A' + (v - 10), f);
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
        fputc('-', f);
        if (v == -2147483648) {
            fputs("2147483648", f);
            return;
        }
        v = -v;
    }
    fput_uint(f, v);
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
    fputs("Usage: ", stderr);
    fputs(prog, stderr);
    fputs(" [options] file.s32o|file.s32x\n", stderr);
    fputs("  -h  header\n", stderr);
    fputs("  -S  sections\n", stderr);
    fputs("  -s  symbols\n", stderr);
    fputs("  -r  relocations\n", stderr);
    fputs("  -a  all (default)\n", stderr);
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
                    fputs("unknown option\n", stderr);
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

    f = fopen(filename, "rb");
    if (!f) {
        fputs("cannot open input\n", stderr);
        return 1;
    }
    if (fseek(f, 0, SEEK_END) != 0) {
        fclose(f);
        fputs("fseek failed\n", stderr);
        return 1;
    }
    size = ftell(f);
    if (size < 4) {
        fclose(f);
        fputs("file too small\n", stderr);
        return 1;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        fclose(f);
        fputs("fseek failed\n", stderr);
        return 1;
    }
    buf = malloc(size);
    if (!buf) {
        fclose(f);
        fputs("out of memory\n", stderr);
        return 1;
    }
    if (fread(buf, 1, size, f) != size) {
        fclose(f);
        free(buf);
        fputs("read failed\n", stderr);
        return 1;
    }
    fclose(f);

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
            fputs("bad s32o header\n", stderr);
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
            fputs("corrupt s32o table bounds\n", stderr);
            return 1;
        }
        strtab = buf + str_off;

        fputs(filename, stdout);
        fputs(": file format s32o-slow32\n", stdout);

        if (show_h) {
            fputs("header: ver=", stdout); out_dec(stdout, version);
            fputs(" endian=", stdout); out_dec(stdout, endian);
            fputs(" machine=", stdout); out_dec(stdout, machine);
            fputs(" flags=0x", stdout); out_hex32(stdout, flags);
            fputc('\n', stdout);
            fputs("  nsections=", stdout); out_dec(stdout, nsections);
            fputs(" nsymbols=", stdout); out_dec(stdout, nsymbols);
            fputs(" str_size=", stdout); out_dec(stdout, str_sz);
            fputc('\n', stdout);
        }
        if (show_sct) {
            fputs("sections:\n", stdout);
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

                fputs("  [", stdout); out_dec(stdout, si); fputs("] ", stdout);
                fputs(safe_name(strtab, str_sz, noff), stdout);
                fputs(" type=", stdout); fputs(sec_type_name(stype), stdout);
                fputs(" size=", stdout); out_dec(stdout, ssize);
                fputs(" off=0x", stdout); out_hex32(stdout, soffset);
                fputs(" align=", stdout); out_dec(stdout, salign);
                fputs(" relocs=", stdout); out_dec(stdout, snrel);
                fputs(" flags=0x", stdout); out_hex32(stdout, sflags);
                fputc('\n', stdout);
                si = si + 1;
            }
        }
        if (show_sym) {
            int si2;
            fputs("symbols:\n", stdout);
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
                fputs("  [", stdout); out_dec(stdout, si2); fputs("] ", stdout);
                fputs(safe_name(strtab, str_sz, noff2), stdout);
                fputs(" val=0x", stdout); out_hex32(stdout, val);
                fputs(" sec=", stdout); out_dec(stdout, sec);
                fputs(" type=", stdout); fputs(sym_type_name(typ), stdout);
                fputs(" bind=", stdout); fputs(sym_bind_name(bind), stdout);
                fputs(" size=", stdout); out_dec(stdout, ssize2);
                fputc('\n', stdout);
                si2 = si2 + 1;
            }
        }
        if (show_rel) {
            int si3;
            fputs("relocations:\n", stdout);
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
                    fputs("  section[", stdout); out_dec(stdout, si3); fputs("]\n", stdout);
                }
                if (!in_bounds(roff3, snrel3 * SIZEOF_S32O_RELOC, size)) {
                    free(buf);
                    fputs("corrupt reloc bounds\n", stderr);
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
                    fputs("    off=0x", stdout); out_hex32(stdout, off);
                    fputs(" sym=", stdout); out_dec(stdout, sym);
                    fputs(" type=", stdout); fputs(rel_type_name(typ2), stdout);
                    fputs(" add=", stdout); out_dec(stdout, add);
                    fputc('\n', stdout);
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
            fputs("bad s32x header\n", stderr);
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
            fputs("corrupt s32x table bounds\n", stderr);
            return 1;
        }
        strtab2 = buf + str_off2;

        fputs(filename, stdout);
        fputs(": file format s32x-slow32\n", stdout);

        if (show_h) {
            fputs("header: ver=", stdout); out_dec(stdout, version2);
            fputs(" endian=", stdout); out_dec(stdout, endian2);
            fputs(" machine=", stdout); out_dec(stdout, machine2);
            fputs(" entry=0x", stdout); out_hex32(stdout, entry);
            fputs(" flags=0x", stdout); out_hex32(stdout, flags2);
            fputc('\n', stdout);
            fputs("  nsections=", stdout); out_dec(stdout, nsections2);
            fputs(" mem_size=0x", stdout); out_hex32(stdout, mem_size);
            fputs(" code_limit=0x", stdout); out_hex32(stdout, code_limit);
            fputs(" data_limit=0x", stdout); out_hex32(stdout, data_limit);
            fputc('\n', stdout);
            fputs("  heap=0x", stdout); out_hex32(stdout, heap_base);
            fputs(" stack=0x", stdout); out_hex32(stdout, stack_base);
            fputs(" mmio=0x", stdout); out_hex32(stdout, mmio_base);
            fputc('\n', stdout);
        }
        if (show_sct) {
            fputs("sections:\n", stdout);
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
                fputs("  [", stdout); out_dec(stdout, si4); fputs("] ", stdout);
                fputs(safe_name(strtab2, str_sz2, noff4), stdout);
                fputs(" type=", stdout); fputs(sec_type_name(stype4), stdout);
                fputs(" vaddr=0x", stdout); out_hex32(stdout, vaddr4);
                fputs(" off=0x", stdout); out_hex32(stdout, off4);
                fputs(" size=", stdout); out_dec(stdout, sz4);
                fputs(" mem=", stdout); out_dec(stdout, msz4);
                fputs(" flags=0x", stdout); out_hex32(stdout, sflags4);
                fputc('\n', stdout);
                si4 = si4 + 1;
            }
        }
        if (show_sym) {
            fputs("symbols: not emitted in base s32x (see SYMTAB section if present)\n", stdout);
        }
        if (show_rel) {
            fputs("relocations: none in linked s32x\n", stdout);
        }
    } else {
        fputs("unknown file magic\n", stderr);
        free(buf);
        return 1;
    }

    free(buf);
    return 0;
}
