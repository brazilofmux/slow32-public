/* POSIX/Linux constants */
#define NULL 0
#define EOF (-1)

#define O_RDONLY  0
#define O_WRONLY  1
#define O_RDWR   2
#define O_CREAT   64
#define O_TRUNC  512
#define O_APPEND 1024

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

#define STDIN_FILENO  0
#define STDOUT_FILENO 1
#define STDERR_FILENO 2

/* External functions (provided by libc_x64.a) */
int write(int fd, char *buf, int len);
int read(int fd, char *buf, int len);
int open(char *path, int flags, int mode);
int close(int fd);
int lseek(int fd, int offset, int whence);
int stat(char *path, char *buf);
int fstat(int fd, char *buf);
char *malloc(int size);
void free(char *ptr);
char *calloc(int n, int size);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
int strlen(char *s);
void exit(int code);

/* FPU operations (compiled by host GCC, linked in) */
unsigned int fpu_add_s(unsigned int a, unsigned int b);
unsigned int fpu_sub_s(unsigned int a, unsigned int b);
unsigned int fpu_mul_s(unsigned int a, unsigned int b);
unsigned int fpu_div_s(unsigned int a, unsigned int b);
unsigned int fpu_sqrt_s(unsigned int a);
int fpu_eq_s(unsigned int a, unsigned int b);
int fpu_lt_s(unsigned int a, unsigned int b);
int fpu_le_s(unsigned int a, unsigned int b);
unsigned int fpu_neg_s(unsigned int a);
unsigned int fpu_abs_s(unsigned int a);
int fpu_cvt_w_s(unsigned int a);
unsigned int fpu_cvt_wu_s(unsigned int a);
unsigned int fpu_cvt_s_w(int a);
unsigned int fpu_cvt_s_wu(unsigned int a);
void fpu_add_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_sub_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_mul_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_div_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_sqrt_d(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
int fpu_eq_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi);
int fpu_lt_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi);
int fpu_le_d(unsigned int a_lo, unsigned int a_hi, unsigned int b_lo, unsigned int b_hi);
void fpu_neg_d(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_abs_d(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
int fpu_cvt_w_d(unsigned int a_lo, unsigned int a_hi);
unsigned int fpu_cvt_wu_d(unsigned int a_lo, unsigned int a_hi);
void fpu_cvt_d_w(int a, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_d_wu(unsigned int a, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_d_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi);
unsigned int fpu_cvt_s_d(unsigned int a_lo, unsigned int a_hi);
void fpu_cvt_l_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_lu_s(unsigned int a, unsigned int *r_lo, unsigned int *r_hi);
unsigned int fpu_cvt_s_l(unsigned int a_lo, unsigned int a_hi);
unsigned int fpu_cvt_s_lu(unsigned int a_lo, unsigned int a_hi);
void fpu_cvt_l_d(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_lu_d(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_d_l(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);
void fpu_cvt_d_lu(unsigned int a_lo, unsigned int a_hi, unsigned int *r_lo, unsigned int *r_hi);

/* s32x format constants */
#define S32X_MAGIC      0x53333258
#define S32X_FLAG_MMIO  0x0080

#define SEC_NULL   0x0000
#define SEC_CODE   0x0001
#define SEC_DATA   0x0002
#define SEC_BSS    0x0003
#define SEC_RODATA 0x0004

#define MMIO_REQ_HEAD    0x0000
#define MMIO_REQ_TAIL    0x0004
#define MMIO_REQ_RING    0x1000
#define MMIO_RESP_HEAD   0x2000
#define MMIO_RESP_TAIL   0x2004
#define MMIO_RESP_RING   0x3000
#define MMIO_DATA_BUF    0x4000
#define MMIO_RING_ENTRIES 256
#define MMIO_DESC_SIZE   16
#define MMIO_DATA_CAP    49152
#define MMIO_WINDOW_SIZE 0x10000

#define OP_MMIO_NOP      0x00
#define OP_MMIO_PUTCHAR  0x01
#define OP_MMIO_GETCHAR  0x02
#define OP_MMIO_WRITE    0x03
#define OP_MMIO_READ     0x04
#define OP_MMIO_OPEN     0x05
#define OP_MMIO_CLOSE    0x06
#define OP_MMIO_SEEK     0x07
#define OP_MMIO_EXIT     0x09
#define OP_MMIO_STAT     0x0A
#define OP_MMIO_FLUSH    0x0B
#define OP_MMIO_ARGS_INFO 0x60
#define OP_MMIO_ARGS_DATA 0x61

#define MMIO_STATUS_OK   0x00000000
#define MMIO_STATUS_ERR  0xFFFFFFFF

#define MAX_FDS 128

#define GFLAG_READ   0x01
#define GFLAG_WRITE  0x02
#define GFLAG_APPEND 0x04
#define GFLAG_CREATE 0x08
#define GFLAG_TRUNC  0x10

#define PC_HIST_LEN 64

static void err_str(char *s) {
    write(2, s, strlen(s));
}

static void err_hex8(unsigned int v) {
    char buf[8];
    char *digits;
    int i;
    digits = "0123456789ABCDEF";
    i = 7;
    while (i >= 0) {
        buf[i] = digits[v & 15];
        v = v >> 4;
        i = i - 1;
    }
    write(2, buf, 8);
}

static void err_dec(unsigned int v) {
    char buf[12];
    int i;
    int len;
    if (v == 0) {
        write(2, "0", 1);
        return;
    }
    i = 11;
    while (v > 0) {
        buf[i] = '0' + (v % 10);
        v = v / 10;
        i = i - 1;
    }
    len = 11 - i;
    write(2, buf + i + 1, len);
}

static void err_nl() {
    write(2, "\n", 1);
}

static unsigned int rd32(unsigned char *m, unsigned int a) {
    unsigned int v;
    memcpy((char *)&v, (char *)(m + a), 4);
    return v;
}

static unsigned short rd16(unsigned char *m, unsigned int a) {
    unsigned short v;
    memcpy((char *)&v, (char *)(m + a), 2);
    return v;
}

static void wr32(unsigned char *m, unsigned int a, unsigned int v) {
    memcpy((char *)(m + a), (char *)&v, 4);
}

static void wr16(unsigned char *m, unsigned int a, unsigned short v) {
    memcpy((char *)(m + a), (char *)&v, 2);
}

static int range_ok_u32(unsigned int base, unsigned int sz, unsigned int total) {
    if (sz > total) return 0;
    return base <= (total - sz);
}

static int sign_extend_u32(unsigned int v, unsigned int bits) {
    unsigned int m;
    m = 1 << (bits - 1);
    if (bits == 32) {
        v = v & 0xFFFFFFFF;
    } else {
        v = v & ((1 << bits) - 1);
    }
    return (int)((v ^ m) - m);
}

struct emu {
    unsigned int r[32];
    unsigned int pc;
    unsigned char *mem;
    unsigned int mem_total;
    unsigned int code_limit;
    unsigned int mmio_base;
    int halted;
    int host_fds[MAX_FDS];
    int fd_owned[MAX_FDS];
    unsigned char *args_blob;
    unsigned int args_argc;
    unsigned int args_total;
    unsigned int pc_hist[PC_HIST_LEN];
    unsigned int pc_hist_pos;
    unsigned int pc_hist_count;
};

static void dump_pc_hist(struct emu *e, char *why);

static int mem_check(struct emu *e, unsigned int addr, unsigned int sz, char *what) {
    if (range_ok_u32(addr, sz, e->mem_total)) return 1;
    err_str(what);
    err_str(" fault: addr=0x");
    err_hex8(addr);
    err_str(" size=");
    err_dec(sz);
    err_str(" total=0x");
    err_hex8(e->mem_total);
    err_str(" PC=0x");
    err_hex8(e->pc);
    err_nl();
    dump_pc_hist(e, what);
    e->halted = 1;
    return 0;
}

static int mem_read16(struct emu *e, unsigned int addr, unsigned short *out, char *what) {
    if (!mem_check(e, addr, 2, what)) return 0;
    *out = rd16(e->mem, addr);
    return 1;
}

static int mem_read32(struct emu *e, unsigned int addr, unsigned int *out, char *what) {
    if (!mem_check(e, addr, 4, what)) return 0;
    *out = rd32(e->mem, addr);
    return 1;
}

static int mem_write32(struct emu *e, unsigned int addr, unsigned int v, char *what) {
    if (!mem_check(e, addr, 4, what)) return 0;
    wr32(e->mem, addr, v);
    return 1;
}

static void pc_hist_push(struct emu *e, unsigned int pc) {
    e->pc_hist[e->pc_hist_pos] = pc;
    e->pc_hist_pos = (e->pc_hist_pos + 1) % PC_HIST_LEN;
    if (e->pc_hist_count < PC_HIST_LEN) {
        e->pc_hist_count = e->pc_hist_count + 1;
    }
}

static void dump_pc_hist(struct emu *e, char *why) {
    unsigned int start;
    unsigned int i;
    unsigned int idx;
    if (e->pc_hist_count == 0) {
        return;
    }
    err_str("Recent PCs before ");
    err_str(why);
    err_str(":");
    err_nl();
    start = (e->pc_hist_pos + PC_HIST_LEN - e->pc_hist_count) % PC_HIST_LEN;
    i = 0;
    while (i < e->pc_hist_count) {
        idx = (start + i) % PC_HIST_LEN;
        err_str("  #");
        err_dec(i);
        err_str(" pc=0x");
        err_hex8(e->pc_hist[idx]);
        err_nl();
        i = i + 1;
    }
}

static void emu_init(struct emu *e) {
    int i;
    memset((char *)e, 0, sizeof(struct emu));
    i = 0;
    while (i < MAX_FDS) {
        e->host_fds[i] = -1;
        i = i + 1;
    }
    e->host_fds[0] = STDIN_FILENO;
    e->host_fds[1] = STDOUT_FILENO;
    e->host_fds[2] = STDERR_FILENO;
}

static void emu_destroy(struct emu *e) {
    int i;
    i = 3;
    while (i < MAX_FDS) {
        if (e->host_fds[i] >= 0 && e->fd_owned[i])
            close(e->host_fds[i]);
        i = i + 1;
    }
    free((char *)e->mem);
    free((char *)e->args_blob);
}

static int alloc_fd(struct emu *e, int host_fd) {
    int i;
    i = 0;
    while (i < MAX_FDS) {
        if (e->host_fds[i] == -1) {
            e->host_fds[i] = host_fd;
            e->fd_owned[i] = 1;
            return i;
        }
        i = i + 1;
    }
    return -1;
}

static int read_file(int fd, unsigned char *buf, int count) {
    int total;
    int n;
    total = 0;
    while (total < count) {
        n = read(fd, (char *)(buf + total), count - total);
        if (n <= 0) break;
        total = total + n;
    }
    return total;
}

static int load_s32x(struct emu *e, char *path) {
    int fd;
    unsigned char hdr[64];
    unsigned int magic;
    unsigned int entry;
    unsigned int nsections;
    unsigned int sec_offset;
    unsigned int flags;
    unsigned int code_limit;
    unsigned int stack_base;
    unsigned int mem_size;
    unsigned int mmio_base;
    int has_mmio;
    unsigned int total;
    unsigned int mmio_end_check;
    unsigned int i;
    unsigned char sec[28];
    unsigned int type;
    unsigned int vaddr;
    unsigned int offset;
    unsigned int sz;
    int saved;
    int nr;

    fd = open(path, O_RDONLY, 0);
    if (fd < 0) {
        err_str("Cannot open: ");
        err_str(path);
        err_nl();
        return 0;
    }

    if (read_file(fd, hdr, 64) != 64) {
        err_str(path);
        err_str(": truncated header");
        err_nl();
        close(fd);
        return 0;
    }

    magic = rd32(hdr, 0x00);
    if (magic != S32X_MAGIC) {
        err_str(path);
        err_str(": bad magic 0x");
        err_hex8(magic);
        err_nl();
        close(fd);
        return 0;
    }

    entry      = rd32(hdr, 0x08);
    nsections  = rd32(hdr, 0x0C);
    sec_offset = rd32(hdr, 0x10);
    flags      = rd32(hdr, 0x1C);
    code_limit = rd32(hdr, 0x20);
    stack_base = rd32(hdr, 0x2C);
    mem_size   = rd32(hdr, 0x30);
    mmio_base  = rd32(hdr, 0x3C);
    has_mmio   = (flags & S32X_FLAG_MMIO) != 0;

    total = mem_size;
    if (stack_base > total)
        total = stack_base;
    if (has_mmio) {
        mmio_end_check = mmio_base + MMIO_WINDOW_SIZE;
        if (mmio_end_check < mmio_base) {
            err_str(path);
            err_str(": MMIO region overflows 32-bit address space");
            err_nl();
            close(fd);
            return 0;
        }
        if (mmio_end_check > total)
            total = mmio_end_check;
    }
    if (total == 0) {
        err_str(path);
        err_str(": invalid memory size");
        err_nl();
        close(fd);
        return 0;
    }

    if (code_limit > total) {
        err_str(path);
        err_str(": code_limit out of range");
        err_nl();
        close(fd);
        return 0;
    }
    if (entry >= code_limit) {
        err_str(path);
        err_str(": entry out of code range");
        err_nl();
        close(fd);
        return 0;
    }

    e->mem = (unsigned char *)calloc(1, total);
    if (!e->mem) {
        err_str("Failed to allocate memory");
        err_nl();
        close(fd);
        return 0;
    }
    e->mem_total  = total;
    e->code_limit = code_limit;
    if (has_mmio) {
        e->mmio_base = mmio_base;
    } else {
        e->mmio_base = 0;
    }

    lseek(fd, sec_offset, SEEK_SET);

    i = 0;
    while (i < nsections) {
        if (read_file(fd, sec, 28) != 28) {
            err_str(path);
            err_str(": truncated section table");
            err_nl();
            close(fd);
            free((char *)e->mem);
            e->mem = 0;
            e->mem_total = 0;
            e->code_limit = 0;
            return 0;
        }

        type   = rd32(sec, 0x04);
        vaddr  = rd32(sec, 0x08);
        offset = rd32(sec, 0x0C);
        sz     = rd32(sec, 0x10);

        if (type != SEC_CODE && type != SEC_DATA && type != SEC_RODATA) {
            i = i + 1;
            continue;
        }
        if (sz == 0) {
            i = i + 1;
            continue;
        }
        if (!range_ok_u32(vaddr, sz, total)) {
            err_str(path);
            err_str(": section overflows memory");
            err_nl();
            close(fd);
            free((char *)e->mem);
            e->mem = 0;
            e->mem_total = 0;
            e->code_limit = 0;
            return 0;
        }

        saved = lseek(fd, 0, SEEK_CUR);
        lseek(fd, offset, SEEK_SET);
        nr = read_file(fd, e->mem + vaddr, sz);
        if (nr != (int)sz) {
            err_str(path);
            err_str(": short read for section");
            err_nl();
            close(fd);
            free((char *)e->mem);
            e->mem = 0;
            e->mem_total = 0;
            e->code_limit = 0;
            return 0;
        }
        lseek(fd, saved, SEEK_SET);

        i = i + 1;
    }

    close(fd);

    e->pc    = entry;
    e->r[29] = stack_base;
    e->r[30] = stack_base;
    e->halted = 0;

    return 1;
}

static void stage_args(struct emu *e, int argc, char **argv) {
    unsigned int total;
    unsigned int off;
    unsigned int len;
    int i;

    total = 0;
    i = 0;
    while (i < argc) {
        total = total + strlen(argv[i]) + 1;
        i = i + 1;
    }

    e->args_blob = (unsigned char *)malloc(total);
    if (!e->args_blob) {
        e->args_argc = 0;
        e->args_total = 0;
        return;
    }

    off = 0;
    i = 0;
    while (i < argc) {
        len = strlen(argv[i]) + 1;
        memcpy((char *)(e->args_blob + off), argv[i], len);
        off = off + len;
        i = i + 1;
    }
    e->args_argc = argc;
    e->args_total = total;
}

static void mmio_process(struct emu *e) {
    unsigned char *m;
    unsigned int mb;
    unsigned char *data;
    unsigned int req_head;
    unsigned int req_tail;
    unsigned int da;
    unsigned int opcode;
    unsigned int length;
    unsigned int offset;
    unsigned int status;
    unsigned int r_opcode;
    unsigned int r_length;
    unsigned int r_offset;
    unsigned int r_status;
    unsigned int resp_head;
    unsigned int resp_tail;
    unsigned int next_head;
    unsigned int ra;
    unsigned char ch;
    int ich;
    int hfd;
    unsigned int off;
    unsigned int max;
    unsigned int cnt;
    int n;
    char path_buf[4096];
    unsigned int plen;
    int gflags;
    int gfd;
    unsigned char whence;
    int dist;
    int pos;
    unsigned int src_off;
    unsigned int remaining;
    char stat_buf[144];
    unsigned char result_buf[112];
    int rc;

    if (!e->mmio_base) return;

    m  = e->mem;
    mb = e->mmio_base;

    if (!range_ok_u32(mb, MMIO_WINDOW_SIZE, e->mem_total)) {
        err_str("MMIO fault: window out of bounds");
        err_nl();
        e->halted = 1;
        return;
    }
    data = m + mb + MMIO_DATA_BUF;

    if (!mem_read32(e, mb + MMIO_REQ_HEAD, &req_head, "MMIO load")) return;
    if (!mem_read32(e, mb + MMIO_REQ_TAIL, &req_tail, "MMIO load")) return;
    if (req_head >= MMIO_RING_ENTRIES || req_tail >= MMIO_RING_ENTRIES) {
        err_str("MMIO fault: invalid ring pointers");
        err_nl();
        e->halted = 1;
        return;
    }

    while (req_head != req_tail) {
        da = mb + MMIO_REQ_RING + req_tail * MMIO_DESC_SIZE;
        if (!range_ok_u32(da, MMIO_DESC_SIZE, e->mem_total)) {
            err_str("MMIO fault: request descriptor out of bounds");
            err_nl();
            e->halted = 1;
            return;
        }
        if (!mem_read32(e, da + 0, &opcode, "MMIO load")) return;
        if (!mem_read32(e, da + 4, &length, "MMIO load")) return;
        if (!mem_read32(e, da + 8, &offset, "MMIO load")) return;
        if (!mem_read32(e, da + 12, &status, "MMIO load")) return;

        r_opcode = opcode;
        r_length = 0;
        r_offset = offset;
        r_status = MMIO_STATUS_OK;

        if (opcode == OP_MMIO_NOP) {
            /* nop */
        } else if (opcode == OP_MMIO_PUTCHAR) {
            ch = data[offset % MMIO_DATA_CAP];
            fputc(ch, stdout);
            fflush(stdout);
        } else if (opcode == OP_MMIO_GETCHAR) {
            ich = fgetc(stdin);
            if (ich != EOF) {
                data[offset % MMIO_DATA_CAP] = (unsigned char)ich;
                r_length = 1;
            } else {
                r_status = MMIO_STATUS_ERR;
            }
        } else if (opcode == OP_MMIO_WRITE) {
            if (status < MAX_FDS) {
                hfd = e->host_fds[status];
            } else {
                hfd = -1;
            }
            off = offset % MMIO_DATA_CAP;
            max = MMIO_DATA_CAP - off;
            cnt = length;
            if (cnt > max) cnt = max;
            if (hfd < 0 || cnt == 0) {
                r_status = MMIO_STATUS_ERR;
                r_length = 0;
            } else {
                n = write(hfd, (char *)(data + off), cnt);
                if (n < 0) {
                    r_status = MMIO_STATUS_ERR;
                    r_length = 0;
                } else {
                    r_status = (unsigned int)n;
                    r_length = (unsigned int)n;
                }
            }
        } else if (opcode == OP_MMIO_READ) {
            if (status < MAX_FDS) {
                hfd = e->host_fds[status];
            } else {
                hfd = -1;
            }
            off = offset % MMIO_DATA_CAP;
            max = MMIO_DATA_CAP - off;
            cnt = length;
            if (cnt > max) cnt = max;
            if (hfd < 0 || cnt == 0) {
                r_status = MMIO_STATUS_ERR;
                r_length = 0;
            } else {
                n = read(hfd, (char *)(data + off), cnt);
                if (n < 0) {
                    r_status = MMIO_STATUS_ERR;
                    r_length = 0;
                } else {
                    r_status = (unsigned int)n;
                    r_length = (unsigned int)n;
                }
            }
        } else if (opcode == OP_MMIO_OPEN) {
            if (length == 0 || length > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR;
            } else {
                off = offset % MMIO_DATA_CAP;
                max = MMIO_DATA_CAP - off;
                plen = length;
                if (plen > max) plen = max;
                if (plen >= 4095) plen = 4095;
                memcpy(path_buf, (char *)(data + off), plen);
                path_buf[plen] = 0;

                gflags = O_RDONLY;
                if (status & GFLAG_WRITE) {
                    if (status & GFLAG_READ) {
                        gflags = O_RDWR;
                    } else {
                        gflags = O_WRONLY;
                    }
                }
                if (status & GFLAG_APPEND) gflags = gflags | O_APPEND;
                if (status & GFLAG_CREATE) gflags = gflags | O_CREAT;
                if (status & GFLAG_TRUNC)  gflags = gflags | O_TRUNC;

                hfd = open(path_buf, gflags, 420);
                if (hfd < 0) {
                    r_status = MMIO_STATUS_ERR;
                } else {
                    gfd = alloc_fd(e, hfd);
                    if (gfd < 0) {
                        close(hfd);
                        r_status = MMIO_STATUS_ERR;
                    } else {
                        r_status = (unsigned int)gfd;
                    }
                }
            }
        } else if (opcode == OP_MMIO_CLOSE) {
            if (status >= MAX_FDS || e->host_fds[status] < 0) {
                r_status = MMIO_STATUS_ERR;
            } else {
                if (e->fd_owned[status])
                    close(e->host_fds[status]);
                e->host_fds[status] = -1;
                e->fd_owned[status] = 0;
            }
        } else if (opcode == OP_MMIO_SEEK) {
            if (status < MAX_FDS) {
                hfd = e->host_fds[status];
            } else {
                hfd = -1;
            }
            if (hfd < 0 || length < 8) {
                r_status = MMIO_STATUS_ERR;
            } else {
                off = offset % MMIO_DATA_CAP;
                if (off + 8 > MMIO_DATA_CAP) {
                    r_status = MMIO_STATUS_ERR;
                } else {
                    whence = data[off];
                    memcpy((char *)&dist, (char *)(data + off + 4), 4);
                    pos = lseek(hfd, dist, (int)whence);
                    if (pos == -1) {
                        r_status = MMIO_STATUS_ERR;
                    } else {
                        r_status = (unsigned int)pos;
                    }
                }
            }
        } else if (opcode == OP_MMIO_EXIT) {
            e->halted = 1;
            e->r[1] = status;
            r_status = status;
        } else if (opcode == OP_MMIO_STAT) {
            off = offset % MMIO_DATA_CAP;
            if (status == 0xFFFFFFFF) {
                max = MMIO_DATA_CAP - off;
                plen = length;
                if (plen > max) plen = max;
                if (plen >= 4095) plen = 4095;
                memcpy(path_buf, (char *)(data + off), plen);
                path_buf[plen] = 0;
                rc = stat(path_buf, stat_buf);
            } else {
                if (status < MAX_FDS) {
                    hfd = e->host_fds[status];
                } else {
                    hfd = -1;
                }
                if (hfd < 0) {
                    r_status = MMIO_STATUS_ERR;
                    goto mmio_stat_done;
                }
                rc = fstat(hfd, stat_buf);
            }
            if (rc != 0) {
                r_status = MMIO_STATUS_ERR;
            } else {
                memset((char *)result_buf, 0, 112);
                /* Extract from Linux x86-64 struct stat raw bytes:
                   raw  0: st_dev (8)    -> result  0: st_dev (8)
                   raw  8: st_ino (8)    -> result  8: st_ino (8)
                   raw 24: st_mode (4)   -> result 16: st_mode (4)
                   raw 16: st_nlink (4)  -> result 20: st_nlink (4)
                   raw 28: st_uid (4)    -> result 24: st_uid (4)
                   raw 32: st_gid (4)    -> result 28: st_gid (4)
                   raw 40: st_rdev (8)   -> result 32: st_rdev (8)
                   raw 48: st_size (8)   -> result 40: st_size (8)
                   raw 56: st_blksize (8)-> result 48: st_blksize (8)
                   raw 64: st_blocks (8) -> result 56: st_blocks (8)
                   raw 72: st_atime (8)  -> result 64: st_atime (8)
                   raw 80: st_atimensec  -> result 72: st_atimensec (4)
                   raw 88: st_mtime (8)  -> result 80: st_mtime (8)
                   raw 96: st_mtimensec  -> result 88: st_mtimensec (4)
                   raw104: st_ctime (8)  -> result 96: st_ctime (8)
                   raw112: st_ctimensec  -> result104: st_ctimensec (4)
                */
                memcpy((char *)(result_buf + 0), stat_buf + 0, 8);
                memcpy((char *)(result_buf + 8), stat_buf + 8, 8);
                memcpy((char *)(result_buf + 16), stat_buf + 24, 4);
                memcpy((char *)(result_buf + 20), stat_buf + 16, 4);
                memcpy((char *)(result_buf + 24), stat_buf + 28, 4);
                memcpy((char *)(result_buf + 28), stat_buf + 32, 4);
                memcpy((char *)(result_buf + 32), stat_buf + 40, 8);
                memcpy((char *)(result_buf + 40), stat_buf + 48, 8);
                memcpy((char *)(result_buf + 48), stat_buf + 56, 8);
                memcpy((char *)(result_buf + 56), stat_buf + 64, 8);
                memcpy((char *)(result_buf + 64), stat_buf + 72, 8);
                memcpy((char *)(result_buf + 72), stat_buf + 80, 4);
                memcpy((char *)(result_buf + 80), stat_buf + 88, 8);
                memcpy((char *)(result_buf + 88), stat_buf + 96, 4);
                memcpy((char *)(result_buf + 96), stat_buf + 104, 8);
                memcpy((char *)(result_buf + 104), stat_buf + 112, 4);

                if (off + 112 > MMIO_DATA_CAP) {
                    r_status = MMIO_STATUS_ERR;
                } else {
                    memcpy((char *)(data + off), (char *)result_buf, 112);
                    r_length = 112;
                }
            }
            mmio_stat_done: ;
        } else if (opcode == OP_MMIO_FLUSH) {
            fflush(stdout);
            fflush(stderr);
        } else if (opcode == OP_MMIO_ARGS_INFO) {
            off = offset % MMIO_DATA_CAP;
            if (off + 16 > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR;
            } else {
                if (!mem_write32(e, mb + MMIO_DATA_BUF + off + 0, e->args_argc, "MMIO store")) return;
                if (!mem_write32(e, mb + MMIO_DATA_BUF + off + 4, e->args_total, "MMIO store")) return;
                if (!mem_write32(e, mb + MMIO_DATA_BUF + off + 8, 0, "MMIO store")) return;
                if (!mem_write32(e, mb + MMIO_DATA_BUF + off + 12, 0, "MMIO store")) return;
                r_length = 16;
            }
        } else if (opcode == OP_MMIO_ARGS_DATA) {
            if (length != 0) {
                off = offset % MMIO_DATA_CAP;
                max = MMIO_DATA_CAP - off;
                src_off = status;
                if (src_off > e->args_total) {
                    r_status = MMIO_STATUS_ERR;
                } else {
                    remaining = e->args_total - src_off;
                    cnt = length;
                    if (cnt > remaining) cnt = remaining;
                    if (cnt > max) cnt = max;
                    if (cnt > 0 && e->args_blob)
                        memcpy((char *)(data + off), (char *)(e->args_blob + src_off), cnt);
                    r_length = cnt;
                }
            }
        } else {
            r_status = MMIO_STATUS_ERR;
        }

        if (!mem_read32(e, mb + MMIO_RESP_HEAD, &resp_head, "MMIO load")) return;
        if (!mem_read32(e, mb + MMIO_RESP_TAIL, &resp_tail, "MMIO load")) return;
        next_head = (resp_head + 1) % MMIO_RING_ENTRIES;
        if (next_head != resp_tail) {
            ra = mb + MMIO_RESP_RING + resp_head * MMIO_DESC_SIZE;
            if (!range_ok_u32(ra, MMIO_DESC_SIZE, e->mem_total)) {
                err_str("MMIO fault: response descriptor out of bounds");
                err_nl();
                e->halted = 1;
                return;
            }
            if (!mem_write32(e, ra + 0, r_opcode, "MMIO store")) return;
            if (!mem_write32(e, ra + 4, r_length, "MMIO store")) return;
            if (!mem_write32(e, ra + 8, r_offset, "MMIO store")) return;
            if (!mem_write32(e, ra + 12, r_status, "MMIO store")) return;
            if (!mem_write32(e, mb + MMIO_RESP_HEAD, next_head, "MMIO store")) return;
        }

        req_tail = (req_tail + 1) % MMIO_RING_ENTRIES;
        if (!mem_write32(e, mb + MMIO_REQ_TAIL, req_tail, "MMIO store")) return;
    }
}

static void step(struct emu *e) {
    unsigned int raw;
    unsigned int op;
    unsigned int rd;
    unsigned int rs1;
    unsigned int rs2;
    int imm_i;
    unsigned int imm_iz;
    int imm_s;
    int imm_b;
    unsigned int imm_u;
    unsigned int jbits;
    int imm_j;
    unsigned int next_pc;
    unsigned int *r;
    unsigned char *m;
    unsigned int a;
    unsigned short v16;
    unsigned int v32;
    unsigned int mulh_a_lo;
    unsigned int mulh_a_hi;
    unsigned int mulh_b_lo;
    unsigned int mulh_b_hi;
    unsigned int mulh_cross1;
    unsigned int mulh_cross2;
    unsigned int mulh_hi;
    unsigned int mulh_carry;
    unsigned int fpu_lo;
    unsigned int fpu_hi;

    pc_hist_push(e, e->pc);

    if (!mem_check(e, e->pc, 4, "Execute")) return;
    if (e->pc > e->code_limit || e->code_limit - e->pc < 4) {
        err_str("Execute fault: PC=0x");
        err_hex8(e->pc);
        err_str(" past code_limit=0x");
        err_hex8(e->code_limit);
        err_nl();
        dump_pc_hist(e, "execute fault");
        e->halted = 1;
        return;
    }

    if (!mem_read32(e, e->pc, &raw, "Execute")) return;
    op  = raw & 0x7F;
    rd  = (raw >> 7) & 0x1F;
    rs1 = (raw >> 15) & 0x1F;
    rs2 = (raw >> 20) & 0x1F;

    imm_i  = sign_extend_u32((raw >> 20) & 0xFFF, 12);
    imm_iz = (raw >> 20) & 0xFFF;
    imm_s  = sign_extend_u32(
                ((raw >> 7) & 0x1F) |
                (((raw >> 25) & 0x7F) << 5),
                12);
    imm_b  = sign_extend_u32(
                (((raw >> 8) & 0xF) << 1) |
                (((raw >> 25) & 0x3F) << 5) |
                (((raw >> 7) & 0x1) << 11) |
                (((raw >> 31) & 0x1) << 12),
                13);
    imm_u  = raw & 0xFFFFF000;
    jbits  = (((raw >> 31) & 1) << 20)
           | (((raw >> 12) & 0xFF) << 12)
           | (((raw >> 20) & 1) << 11)
           | (((raw >> 21) & 0x3FF) << 1);
    imm_j  = sign_extend_u32(jbits, 21);

    next_pc = e->pc + 4;
    r = e->r;
    m = e->mem;

    if (op == 0x00) { r[rd] = r[rs1] + r[rs2]; }
    else if (op == 0x01) { r[rd] = r[rs1] - r[rs2]; }
    else if (op == 0x02) { r[rd] = r[rs1] ^ r[rs2]; }
    else if (op == 0x03) { r[rd] = r[rs1] | r[rs2]; }
    else if (op == 0x04) { r[rd] = r[rs1] & r[rs2]; }
    else if (op == 0x05) { r[rd] = r[rs1] << (r[rs2] & 0x1F); }
    else if (op == 0x06) { r[rd] = r[rs1] >> (r[rs2] & 0x1F); }
    else if (op == 0x07) { r[rd] = (unsigned int)((int)r[rs1] >> (r[rs2] & 0x1F)); }
    else if (op == 0x08) { r[rd] = ((int)r[rs1] < (int)r[rs2]) ? 1 : 0; }
    else if (op == 0x09) { r[rd] = (r[rs1] < r[rs2]) ? 1 : 0; }
    else if (op == 0x0A) { r[rd] = r[rs1] * r[rs2]; }
    else if (op == 0x0B) {
        /* MULH (signed high): compute via unsigned MULHU + correction */
        /* First compute unsigned high product */
        mulh_a_lo = r[rs1] & 0xFFFF;
        mulh_a_hi = r[rs1] >> 16;
        mulh_b_lo = r[rs2] & 0xFFFF;
        mulh_b_hi = r[rs2] >> 16;
        /* cross terms */
        mulh_cross1 = mulh_a_lo * mulh_b_hi;
        mulh_cross2 = mulh_a_hi * mulh_b_lo;
        /* carry from low 32 bits into high */
        mulh_carry = ((mulh_a_lo * mulh_b_lo) >> 16);
        mulh_carry = mulh_carry + (mulh_cross1 & 0xFFFF) + (mulh_cross2 & 0xFFFF);
        mulh_carry = mulh_carry >> 16;
        /* unsigned high product */
        mulh_hi = mulh_a_hi * mulh_b_hi + (mulh_cross1 >> 16) + (mulh_cross2 >> 16) + mulh_carry;
        /* signed correction: hi_signed = hi_unsigned - (a<0 ? b : 0) - (b<0 ? a : 0) */
        if ((int)r[rs1] < 0) mulh_hi = mulh_hi - r[rs2];
        if ((int)r[rs2] < 0) mulh_hi = mulh_hi - r[rs1];
        r[rd] = mulh_hi;
    }
    else if (op == 0x0C) {
        if (r[rs2] == 0) r[rd] = 0xFFFFFFFF;
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0x80000000;
        else r[rd] = (unsigned int)((int)r[rs1] / (int)r[rs2]);
    }
    else if (op == 0x0D) {
        if (r[rs2] == 0) r[rd] = r[rs1];
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0;
        else r[rd] = (unsigned int)((int)r[rs1] % (int)r[rs2]);
    }
    else if (op == 0x0E) { r[rd] = (r[rs1] == r[rs2]) ? 1 : 0; }
    else if (op == 0x0F) { r[rd] = (r[rs1] != r[rs2]) ? 1 : 0; }

    else if (op == 0x10) { r[rd] = r[rs1] + imm_i; }
    else if (op == 0x11) { r[rd] = r[rs1] | imm_iz; }
    else if (op == 0x12) { r[rd] = r[rs1] & imm_iz; }
    else if (op == 0x13) { r[rd] = r[rs1] << (imm_i & 0x1F); }
    else if (op == 0x14) { r[rd] = r[rs1] >> (imm_i & 0x1F); }
    else if (op == 0x15) { r[rd] = (unsigned int)((int)r[rs1] >> (imm_i & 0x1F)); }
    else if (op == 0x16) { r[rd] = ((int)r[rs1] < imm_i) ? 1 : 0; }
    else if (op == 0x17) { r[rd] = (r[rs1] < imm_iz) ? 1 : 0; }
    else if (op == 0x18) { r[rd] = ((int)r[rs1] > (int)r[rs2]) ? 1 : 0; }
    else if (op == 0x19) { r[rd] = (r[rs1] > r[rs2]) ? 1 : 0; }
    else if (op == 0x1A) { r[rd] = ((int)r[rs1] <= (int)r[rs2]) ? 1 : 0; }
    else if (op == 0x1B) { r[rd] = (r[rs1] <= r[rs2]) ? 1 : 0; }
    else if (op == 0x1C) { r[rd] = ((int)r[rs1] >= (int)r[rs2]) ? 1 : 0; }
    else if (op == 0x1D) { r[rd] = (r[rs1] >= r[rs2]) ? 1 : 0; }
    else if (op == 0x1E) { r[rd] = r[rs1] ^ imm_iz; }
    else if (op == 0x1F) {
        /* MULHU (unsigned high) using 16-bit halves */
        mulh_a_lo = r[rs1] & 0xFFFF;
        mulh_a_hi = r[rs1] >> 16;
        mulh_b_lo = r[rs2] & 0xFFFF;
        mulh_b_hi = r[rs2] >> 16;
        mulh_cross1 = mulh_a_lo * mulh_b_hi;
        mulh_cross2 = mulh_a_hi * mulh_b_lo;
        mulh_carry = ((mulh_a_lo * mulh_b_lo) >> 16);
        mulh_carry = mulh_carry + (mulh_cross1 & 0xFFFF) + (mulh_cross2 & 0xFFFF);
        mulh_carry = mulh_carry >> 16;
        r[rd] = mulh_a_hi * mulh_b_hi + (mulh_cross1 >> 16) + (mulh_cross2 >> 16) + mulh_carry;
    }

    else if (op == 0x20) { r[rd] = imm_u; }

    else if (op == 0x30) {
        a = r[rs1] + imm_i;
        if (!mem_check(e, a, 1, "Load")) return;
        /* Sign-extend 8 → 32: if bit 7 set, fill upper 24 bits */
        r[rd] = m[a];
        if (r[rd] & 0x80) r[rd] = r[rd] | 0xFFFFFF00;
    }
    else if (op == 0x31) {
        a = r[rs1] + imm_i;
        if (!mem_read16(e, a, &v16, "Load")) return;
        /* Sign-extend 16 → 32: if bit 15 set, fill upper 16 bits */
        r[rd] = v16;
        if (v16 & 0x8000) r[rd] = r[rd] | 0xFFFF0000;
    }
    else if (op == 0x32) {
        a = r[rs1] + imm_i;
        if (!mem_read32(e, a, &v32, "Load")) return;
        r[rd] = v32;
    }
    else if (op == 0x33) {
        a = r[rs1] + imm_i;
        if (!mem_check(e, a, 1, "Load")) return;
        r[rd] = m[a];
    }
    else if (op == 0x34) {
        a = r[rs1] + imm_i;
        if (!mem_read16(e, a, &v16, "Load")) return;
        r[rd] = v16;
    }

    else if (op == 0x38) {
        a = r[rs1] + imm_s;
        if (!mem_check(e, a, 1, "Store")) return;
        m[a] = (unsigned char)r[rs2];
    }
    else if (op == 0x39) {
        a = r[rs1] + imm_s;
        if (!mem_check(e, a, 2, "Store")) return;
        wr16(m, a, (unsigned short)r[rs2]);
    }
    else if (op == 0x3A) {
        a = r[rs1] + imm_s;
        if (!mem_write32(e, a, r[rs2], "Store")) return;
    }

    else if (op == 0x3F) {
        if (r[rs1] != r[rs2]) {
            err_str("Assertion failed: r");
            err_dec(rs1);
            err_str("(0x");
            err_hex8(r[rs1]);
            err_str(") != r");
            err_dec(rs2);
            err_str("(0x");
            err_hex8(r[rs2]);
            err_str(") at PC=0x");
            err_hex8(e->pc);
            err_nl();
            e->halted = 1;
        }
    }

    else if (op == 0x40) {
        r[rd] = e->pc + 4;
        next_pc = e->pc + imm_j;
    }
    else if (op == 0x41) {
        r[rd] = e->pc + 4;
        next_pc = (r[rs1] + imm_i) & ~1;
    }

    else if (op == 0x48) { if (r[rs1] == r[rs2]) next_pc = next_pc + imm_b; }
    else if (op == 0x49) { if (r[rs1] != r[rs2]) next_pc = next_pc + imm_b; }
    else if (op == 0x4A) { if ((int)r[rs1] < (int)r[rs2]) next_pc = next_pc + imm_b; }
    else if (op == 0x4B) { if ((int)r[rs1] >= (int)r[rs2]) next_pc = next_pc + imm_b; }
    else if (op == 0x4C) { if (r[rs1] < r[rs2]) next_pc = next_pc + imm_b; }
    else if (op == 0x4D) { if (r[rs1] >= r[rs2]) next_pc = next_pc + imm_b; }

    else if (op == 0x50) { /* NOP */ }
    else if (op == 0x51) { mmio_process(e); }
    else if (op == 0x52) {
        putchar(r[rs1] & 0xFF);
        fflush(stdout);
    }

    /* ---- f32 instructions (0x53-0x60) ---- */
    else if (op == 0x53) { r[rd] = fpu_add_s(r[rs1], r[rs2]); }
    else if (op == 0x54) { r[rd] = fpu_sub_s(r[rs1], r[rs2]); }
    else if (op == 0x55) { r[rd] = fpu_mul_s(r[rs1], r[rs2]); }
    else if (op == 0x56) { r[rd] = fpu_div_s(r[rs1], r[rs2]); }
    else if (op == 0x57) { r[rd] = fpu_sqrt_s(r[rs1]); }
    else if (op == 0x58) { r[rd] = fpu_eq_s(r[rs1], r[rs2]); }
    else if (op == 0x59) { r[rd] = fpu_lt_s(r[rs1], r[rs2]); }
    else if (op == 0x5A) { r[rd] = fpu_le_s(r[rs1], r[rs2]); }
    else if (op == 0x5B) { r[rd] = fpu_cvt_w_s(r[rs1]); }
    else if (op == 0x5C) { r[rd] = fpu_cvt_wu_s(r[rs1]); }
    else if (op == 0x5D) { r[rd] = fpu_cvt_s_w(r[rs1]); }
    else if (op == 0x5E) { r[rd] = fpu_cvt_s_wu(r[rs1]); }
    else if (op == 0x5F) { r[rd] = fpu_neg_s(r[rs1]); }
    else if (op == 0x60) { r[rd] = fpu_abs_s(r[rs1]); }

    /* ---- f64 instructions (0x61-0x70) — register pairs ---- */
    else if (op == 0x61) { fpu_add_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x62) { fpu_sub_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x63) { fpu_mul_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x64) { fpu_div_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x65) { fpu_sqrt_d(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x66) { r[rd] = fpu_eq_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1]); }
    else if (op == 0x67) { r[rd] = fpu_lt_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1]); }
    else if (op == 0x68) { r[rd] = fpu_le_d(r[rs1], r[rs1+1], r[rs2], r[rs2+1]); }
    else if (op == 0x69) { r[rd] = fpu_cvt_w_d(r[rs1], r[rs1+1]); }
    else if (op == 0x6A) { r[rd] = fpu_cvt_wu_d(r[rs1], r[rs1+1]); }
    else if (op == 0x6B) { fpu_cvt_d_w(r[rs1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x6C) { fpu_cvt_d_wu(r[rs1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x6D) { fpu_cvt_d_s(r[rs1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x6E) { r[rd] = fpu_cvt_s_d(r[rs1], r[rs1+1]); }
    else if (op == 0x6F) { fpu_neg_d(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x70) { fpu_abs_d(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }

    /* ---- float ↔ int64 conversions (0x71-0x78) ---- */
    else if (op == 0x71) { fpu_cvt_l_s(r[rs1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x72) { fpu_cvt_lu_s(r[rs1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x73) { r[rd] = fpu_cvt_s_l(r[rs1], r[rs1+1]); }
    else if (op == 0x74) { r[rd] = fpu_cvt_s_lu(r[rs1], r[rs1+1]); }
    else if (op == 0x75) { fpu_cvt_l_d(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x76) { fpu_cvt_lu_d(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x77) { fpu_cvt_d_l(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }
    else if (op == 0x78) { fpu_cvt_d_lu(r[rs1], r[rs1+1], &fpu_lo, &fpu_hi); r[rd] = fpu_lo; r[rd+1] = fpu_hi; }

    else if (op == 0x7F) {
        mmio_process(e);
        err_str("HALT at PC=0x");
        err_hex8(e->pc);
        err_nl();
        e->halted = 1;
    }

    else {
        err_str("Unknown opcode 0x");
        err_hex8(op);
        err_str(" at PC=0x");
        err_hex8(e->pc);
        err_nl();
        e->halted = 1;
    }

    r[0] = 0;
    e->pc = next_pc;
}

int main(int argc, char **argv) {
    unsigned int test;
    struct emu e;
    int exit_code;

    file_sys_init();

    test = 1;
    if (*(unsigned char *)&test != 1) {
        err_str("Error: This emulator only supports Little-Endian host platforms.");
        err_nl();
        return 1;
    }

    if (argc < 2) {
        err_str("Usage: s32-emu <program.s32x> [args...]");
        err_nl();
        return 1;
    }

    emu_init(&e);

    if (!load_s32x(&e, argv[1])) {
        emu_destroy(&e);
        return 1;
    }

    stage_args(&e, argc - 1, argv + 1);

    err_str("s32-emu: starting ");
    err_str(argv[1]);
    err_str(" (code_limit=0x");
    err_hex8(e.code_limit);
    err_str(" mmio=0x");
    err_hex8(e.mmio_base);
    err_str(")");
    err_nl();

    while (!e.halted)
        step(&e);

    exit_code = (int)e.r[1];
    emu_destroy(&e);
    return exit_code;
}
