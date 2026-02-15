/* Feature test macros — must precede all includes */
#define _POSIX_C_SOURCE 200809L

/*
 * s32-emu.c — Stage 0 Bootstrap Emulator for SLOW-32
 *
 * A minimal, self-contained emulator for bootstrapping the self-hosted
 * toolchain. No external dependencies beyond standard C and POSIX I/O.
 *
 * Supports: all integer instructions, MMIO ring buffer I/O, .s32x loading.
 * Does NOT support: floating point, service negotiation, debugging features.
 *
 * Build:  cc -O2 -o s32-emu s32-emu.c
 * Usage:  ./s32-emu program.s32x [args...]
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <limits.h>

/* ======================================================================
 * Constants
 * ====================================================================== */

/* .s32x executable format */
#define S32X_MAGIC      0x53333258u
#define S32X_FLAG_MMIO  0x0080u

/* Section types */
#define SEC_NULL   0x0000
#define SEC_CODE   0x0001
#define SEC_DATA   0x0002
#define SEC_BSS    0x0003
#define SEC_RODATA 0x0004

/* MMIO layout (offsets from mmio_base) */
#define MMIO_REQ_HEAD    0x0000u
#define MMIO_REQ_TAIL    0x0004u
#define MMIO_REQ_RING    0x1000u
#define MMIO_RESP_HEAD   0x2000u
#define MMIO_RESP_TAIL   0x2004u
#define MMIO_RESP_RING   0x3000u
#define MMIO_DATA_BUF    0x4000u
#define MMIO_RING_ENTRIES 256u
#define MMIO_DESC_SIZE   16u       /* 4 x uint32_t */
#define MMIO_DATA_CAP    (48u * 1024u)
#define MMIO_WINDOW_SIZE 0x10000u  /* 64 KB total */

/* MMIO opcodes */
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

#define MMIO_STATUS_OK   0x00000000u
#define MMIO_STATUS_ERR  0xFFFFFFFFu

/* File descriptor table */
#define MAX_FDS 128

/* Guest open flags */
#define GFLAG_READ   0x01u
#define GFLAG_WRITE  0x02u
#define GFLAG_APPEND 0x04u
#define GFLAG_CREATE 0x08u
#define GFLAG_TRUNC  0x10u

/* ======================================================================
 * Memory access helpers (little-endian)
 * ====================================================================== */

static inline uint32_t rd32(const uint8_t *m, uint32_t a) {
    uint32_t v; memcpy(&v, m + a, 4); return v;
}
static inline uint16_t rd16(const uint8_t *m, uint32_t a) {
    uint16_t v; memcpy(&v, m + a, 2); return v;
}
static inline void wr32(uint8_t *m, uint32_t a, uint32_t v) {
    memcpy(m + a, &v, 4);
}
static inline void wr16(uint8_t *m, uint32_t a, uint16_t v) {
    memcpy(m + a, &v, 2);
}

static bool range_ok_u32(uint32_t base, uint32_t size, uint32_t total) {
    if (size > total) return false;
    return base <= (total - size);
}

/* ======================================================================
 * Emulator state
 * ====================================================================== */

typedef struct {
    uint32_t r[32];         /* Registers (r[0] = 0 always) */
    uint32_t pc;
    uint8_t *mem;           /* Flat memory array */
    uint32_t mem_total;     /* Total allocated bytes */
    uint32_t code_limit;    /* End of executable region */
    uint32_t mmio_base;     /* MMIO region base (0 = no MMIO) */
    bool     halted;

    /* File descriptor table */
    int  host_fds[MAX_FDS];
    bool fd_owned[MAX_FDS];

    /* Guest arguments (flattened NUL-separated blob) */
    uint8_t *args_blob;
    uint32_t args_argc;
    uint32_t args_total;
} emu_t;

static void emu_init(emu_t *e) {
    memset(e, 0, sizeof(*e));
    for (int i = 0; i < MAX_FDS; i++)
        e->host_fds[i] = -1;
    e->host_fds[0] = STDIN_FILENO;
    e->host_fds[1] = STDOUT_FILENO;
    e->host_fds[2] = STDERR_FILENO;
}

static void emu_destroy(emu_t *e) {
    /* Close owned file descriptors */
    for (int i = 3; i < MAX_FDS; i++) {
        if (e->host_fds[i] >= 0 && e->fd_owned[i])
            close(e->host_fds[i]);
    }
    free(e->mem);
    free(e->args_blob);
}

static int alloc_fd(emu_t *e, int host_fd) {
    for (int i = 0; i < MAX_FDS; i++) {
        if (e->host_fds[i] == -1) {
            e->host_fds[i] = host_fd;
            e->fd_owned[i] = true;
            return i;
        }
    }
    return -1;
}

/* ======================================================================
 * .s32x loader
 * ====================================================================== */

static bool load_s32x(emu_t *e, const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); return false; }

    /* Read 64-byte header */
    uint8_t hdr[64];
    if (fread(hdr, 1, 64, f) != 64) {
        fprintf(stderr, "%s: truncated header\n", path);
        fclose(f); return false;
    }

    uint32_t magic = rd32(hdr, 0x00);
    if (magic != S32X_MAGIC) {
        fprintf(stderr, "%s: bad magic 0x%08X (expected 0x%08X)\n",
                path, magic, S32X_MAGIC);
        fclose(f); return false;
    }

    uint32_t entry      = rd32(hdr, 0x08);
    uint32_t nsections  = rd32(hdr, 0x0C);
    uint32_t sec_offset = rd32(hdr, 0x10);
    uint32_t flags      = rd32(hdr, 0x1C);
    uint32_t code_limit = rd32(hdr, 0x20);
    uint32_t stack_base = rd32(hdr, 0x2C);
    uint32_t mem_size   = rd32(hdr, 0x30);
    uint32_t mmio_base  = rd32(hdr, 0x3C);
    bool     has_mmio   = (flags & S32X_FLAG_MMIO) != 0;

    /* Compute total memory needed (overflow-safe) */
    uint64_t total64 = mem_size;
    if (has_mmio) {
        uint64_t mmio_end = (uint64_t)mmio_base + (uint64_t)MMIO_WINDOW_SIZE;
        if (mmio_end > total64)
            total64 = mmio_end;
    }
    if (total64 == 0 || total64 > UINT32_MAX || total64 > SIZE_MAX) {
        fprintf(stderr, "%s: invalid memory size\n", path);
        fclose(f); return false;
    }
    uint32_t total = (uint32_t)total64;

    if (code_limit > total) {
        fprintf(stderr, "%s: code_limit out of range (0x%X > 0x%X)\n",
                path, code_limit, total);
        fclose(f); return false;
    }
    if (entry >= code_limit) {
        fprintf(stderr, "%s: entry out of code range (entry=0x%X code_limit=0x%X)\n",
                path, entry, code_limit);
        fclose(f); return false;
    }
    if (stack_base > total) {
        fprintf(stderr, "%s: stack_base out of range (0x%X > 0x%X)\n",
                path, stack_base, total);
        fclose(f); return false;
    }

    e->mem = calloc(1, total);
    if (!e->mem) {
        fprintf(stderr, "Failed to allocate %u bytes\n", total);
        fclose(f); return false;
    }
    e->mem_total  = total;
    e->code_limit = code_limit;
    e->mmio_base  = has_mmio ? mmio_base : 0;

    /* Read section table and load sections */
    if (fseek(f, sec_offset, SEEK_SET) != 0) {
        fprintf(stderr, "%s: bad section offset\n", path);
        fclose(f); return false;
    }

    for (uint32_t i = 0; i < nsections; i++) {
        uint8_t sec[28];
        if (fread(sec, 1, 28, f) != 28) break;

        uint32_t type   = rd32(sec, 0x04);
        uint32_t vaddr  = rd32(sec, 0x08);
        uint32_t offset = rd32(sec, 0x0C);
        uint32_t size   = rd32(sec, 0x10);

        /* Only load CODE, DATA, RODATA (BSS already zeroed by calloc) */
        if (type != SEC_CODE && type != SEC_DATA && type != SEC_RODATA)
            continue;
        if (size == 0) continue;
        if (!range_ok_u32(vaddr, size, total)) {
            fprintf(stderr, "%s: section %u overflows memory (vaddr=0x%X size=0x%X)\n",
                    path, i, vaddr, size);
            continue;
        }

        long saved = ftell(f);
        fseek(f, offset, SEEK_SET);
        if (fread(e->mem + vaddr, 1, size, f) != size) {
            fprintf(stderr, "%s: short read for section %u\n", path, i);
        }
        fseek(f, saved, SEEK_SET);
    }

    fclose(f);

    /* Initialize CPU state */
    e->pc    = entry;
    e->r[29] = stack_base;  /* SP */
    e->r[30] = stack_base;  /* FP */
    e->halted = false;

    return true;
}

/* ======================================================================
 * Guest argument staging
 * ====================================================================== */

static void stage_args(emu_t *e, int argc, char **argv) {
    /* Compute total size: NUL-separated strings */
    uint32_t total = 0;
    for (int i = 0; i < argc; i++)
        total += strlen(argv[i]) + 1;

    e->args_blob = malloc(total);
    if (!e->args_blob) { e->args_argc = 0; e->args_total = 0; return; }

    uint32_t off = 0;
    for (int i = 0; i < argc; i++) {
        uint32_t len = strlen(argv[i]) + 1;
        memcpy(e->args_blob + off, argv[i], len);
        off += len;
    }
    e->args_argc = argc;
    e->args_total = total;
}

/* ======================================================================
 * MMIO ring buffer processing (called on YIELD)
 * ====================================================================== */

static void mmio_process(emu_t *e) {
    if (!e->mmio_base) return;

    uint8_t *m  = e->mem;
    uint32_t mb = e->mmio_base;

    if (!range_ok_u32(mb, MMIO_WINDOW_SIZE, e->mem_total)) {
        fprintf(stderr, "MMIO fault: window out of bounds (base=0x%08X size=0x%X total=0x%X)\n",
                mb, MMIO_WINDOW_SIZE, e->mem_total);
        e->halted = true;
        return;
    }
    uint8_t *data = m + mb + MMIO_DATA_BUF;

    uint32_t req_head = rd32(m, mb + MMIO_REQ_HEAD);
    uint32_t req_tail = rd32(m, mb + MMIO_REQ_TAIL);
    if (req_head >= MMIO_RING_ENTRIES || req_tail >= MMIO_RING_ENTRIES) {
        fprintf(stderr, "MMIO fault: invalid ring pointers (head=%u tail=%u)\n",
                req_head, req_tail);
        e->halted = true;
        return;
    }

    while (req_head != req_tail) {
        /* Read request descriptor */
        uint32_t da = mb + MMIO_REQ_RING + req_tail * MMIO_DESC_SIZE;
        if (!range_ok_u32(da, MMIO_DESC_SIZE, e->mem_total)) {
            fprintf(stderr, "MMIO fault: request descriptor out of bounds\n");
            e->halted = true;
            return;
        }
        uint32_t opcode = rd32(m, da + 0);
        uint32_t length = rd32(m, da + 4);
        uint32_t offset = rd32(m, da + 8);
        uint32_t status = rd32(m, da + 12);

        /* Prepare response */
        uint32_t r_opcode = opcode;
        uint32_t r_length = 0;
        uint32_t r_offset = offset;
        uint32_t r_status = MMIO_STATUS_OK;

        switch (opcode) {
        case OP_MMIO_NOP:
            break;

        case OP_MMIO_PUTCHAR: {
            uint8_t ch = data[offset % MMIO_DATA_CAP];
            fputc(ch, stdout);
            fflush(stdout);
            break;
        }

        case OP_MMIO_GETCHAR: {
            int ch = fgetc(stdin);
            if (ch != EOF) {
                data[offset % MMIO_DATA_CAP] = (uint8_t)ch;
                r_length = 1;
            } else {
                r_status = MMIO_STATUS_ERR;
            }
            break;
        }

        case OP_MMIO_WRITE: {
            int hfd = (status < MAX_FDS) ? e->host_fds[status] : -1;
            uint32_t off = offset % MMIO_DATA_CAP;
            uint32_t max = MMIO_DATA_CAP - off;
            uint32_t cnt = (length > max) ? max : length;
            if (hfd < 0 || cnt == 0) {
                r_status = MMIO_STATUS_ERR; r_length = 0; break;
            }
            ssize_t n = write(hfd, data + off, cnt);
            if (n < 0) { r_status = MMIO_STATUS_ERR; r_length = 0; }
            else        { r_status = (uint32_t)n; r_length = (uint32_t)n; }
            break;
        }

        case OP_MMIO_READ: {
            int hfd = (status < MAX_FDS) ? e->host_fds[status] : -1;
            uint32_t off = offset % MMIO_DATA_CAP;
            uint32_t max = MMIO_DATA_CAP - off;
            uint32_t cnt = (length > max) ? max : length;
            if (hfd < 0 || cnt == 0) {
                r_status = MMIO_STATUS_ERR; r_length = 0; break;
            }
            ssize_t n = read(hfd, data + off, cnt);
            if (n < 0) { r_status = MMIO_STATUS_ERR; r_length = 0; }
            else        { r_status = (uint32_t)n; r_length = (uint32_t)n; }
            break;
        }

        case OP_MMIO_OPEN: {
            if (length == 0 || length > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR; break;
            }
            uint32_t off = offset % MMIO_DATA_CAP;
            uint32_t max = MMIO_DATA_CAP - off;
            /* Extract path (NUL-terminated in data buffer) */
            char path_buf[4096];
            uint32_t plen = (length < max) ? length : max;
            if (plen >= sizeof(path_buf))
                plen = sizeof(path_buf) - 1;
            memcpy(path_buf, data + off, plen);
            path_buf[plen] = '\0';

            /* Translate guest flags to POSIX */
            int flags = O_RDONLY;
            if (status & GFLAG_WRITE)
                flags = (status & GFLAG_READ) ? O_RDWR : O_WRONLY;
            if (status & GFLAG_APPEND) flags |= O_APPEND;
            if (status & GFLAG_CREATE) flags |= O_CREAT;
            if (status & GFLAG_TRUNC)  flags |= O_TRUNC;

            int hfd = (flags & O_CREAT)
                ? open(path_buf, flags, 0644)
                : open(path_buf, flags);
            if (hfd < 0) { r_status = MMIO_STATUS_ERR; break; }

            int gfd = alloc_fd(e, hfd);
            if (gfd < 0) { close(hfd); r_status = MMIO_STATUS_ERR; break; }
            r_status = (uint32_t)gfd;
            break;
        }

        case OP_MMIO_CLOSE: {
            uint32_t gfd = status;
            if (gfd >= MAX_FDS || e->host_fds[gfd] < 0) {
                r_status = MMIO_STATUS_ERR; break;
            }
            if (e->fd_owned[gfd])
                close(e->host_fds[gfd]);
            e->host_fds[gfd] = -1;
            e->fd_owned[gfd] = false;
            break;
        }

        case OP_MMIO_SEEK: {
            int hfd = (status < MAX_FDS) ? e->host_fds[status] : -1;
            if (hfd < 0 || length < 8) {
                r_status = MMIO_STATUS_ERR; break;
            }
            uint32_t off = offset % MMIO_DATA_CAP;
            if (off + 8 > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR; break;
            }
            uint8_t whence = data[off];
            int32_t dist;
            memcpy(&dist, data + off + 4, 4);
            off_t pos = lseek(hfd, (off_t)dist, (int)whence);
            if (pos == (off_t)-1) r_status = MMIO_STATUS_ERR;
            else                  r_status = (uint32_t)pos;
            break;
        }

        case OP_MMIO_EXIT:
            e->halted = true;
            e->r[1] = status;  /* Exit code in r1 */
            r_status = status;
            break;

        case OP_MMIO_STAT: {
            uint32_t off = offset % MMIO_DATA_CAP;
            struct stat st;
            int rc;
            if (status == 0xFFFFFFFFu) {
                /* Path-based stat */
                char path_buf[4096];
                uint32_t max = MMIO_DATA_CAP - off;
                uint32_t plen = (length < max) ? length : max;
                if (plen >= sizeof(path_buf))
                    plen = sizeof(path_buf) - 1;
                memcpy(path_buf, data + off, plen);
                path_buf[plen] = '\0';
                rc = stat(path_buf, &st);
            } else {
                int hfd = (status < MAX_FDS) ? e->host_fds[status] : -1;
                if (hfd < 0) { r_status = MMIO_STATUS_ERR; break; }
                rc = fstat(hfd, &st);
            }
            if (rc != 0) { r_status = MMIO_STATUS_ERR; break; }
            /* Write s32_mmio_stat_result_t (112 bytes, packed) */
            uint8_t buf[112];
            memset(buf, 0, sizeof(buf));
            uint64_t tmp;
            tmp = (uint64_t)st.st_dev;   memcpy(buf + 0, &tmp, 8);
            tmp = (uint64_t)st.st_ino;   memcpy(buf + 8, &tmp, 8);
            uint32_t t32;
            t32 = (uint32_t)st.st_mode;  memcpy(buf + 16, &t32, 4);
            t32 = (uint32_t)st.st_nlink; memcpy(buf + 20, &t32, 4);
            t32 = (uint32_t)st.st_uid;   memcpy(buf + 24, &t32, 4);
            t32 = (uint32_t)st.st_gid;   memcpy(buf + 28, &t32, 4);
            tmp = (uint64_t)st.st_rdev;  memcpy(buf + 32, &tmp, 8);
            tmp = (uint64_t)(st.st_size < 0 ? 0 : st.st_size);
                                         memcpy(buf + 40, &tmp, 8);
            tmp = (uint64_t)(st.st_blksize < 0 ? 0 : st.st_blksize);
                                         memcpy(buf + 48, &tmp, 8);
            tmp = (uint64_t)(st.st_blocks < 0 ? 0 : st.st_blocks);
                                         memcpy(buf + 56, &tmp, 8);
            /* atime */
            tmp = (uint64_t)st.st_atim.tv_sec;  memcpy(buf + 64, &tmp, 8);
            t32 = (uint32_t)st.st_atim.tv_nsec; memcpy(buf + 72, &t32, 4);
            /* mtime */
            tmp = (uint64_t)st.st_mtim.tv_sec;  memcpy(buf + 80, &tmp, 8);
            t32 = (uint32_t)st.st_mtim.tv_nsec; memcpy(buf + 88, &t32, 4);
            /* ctime */
            tmp = (uint64_t)st.st_ctim.tv_sec;  memcpy(buf + 96, &tmp, 8);
            t32 = (uint32_t)st.st_ctim.tv_nsec; memcpy(buf + 104, &t32, 4);
            if (off + sizeof(buf) > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR; break;
            }
            memcpy(data + off, buf, sizeof(buf));
            r_length = sizeof(buf);
            break;
        }

        case OP_MMIO_FLUSH:
            fflush(stdout);
            fflush(stderr);
            break;

        case OP_MMIO_ARGS_INFO: {
            uint32_t off = offset % MMIO_DATA_CAP;
            if (off + 16 > MMIO_DATA_CAP) {
                r_status = MMIO_STATUS_ERR; break;
            }
            /* Write s32_mmio_args_info_t: argc, total_bytes, flags, reserved */
            wr32(data, off + 0, e->args_argc);
            wr32(data, off + 4, e->args_total);
            wr32(data, off + 8, 0);
            wr32(data, off + 12, 0);
            r_length = 16;
            break;
        }

        case OP_MMIO_ARGS_DATA: {
            if (length == 0) break;
            uint32_t off = offset % MMIO_DATA_CAP;
            uint32_t max = MMIO_DATA_CAP - off;
            uint32_t src_off = status;  /* Source offset into args blob */
            if (src_off > e->args_total) {
                r_status = MMIO_STATUS_ERR; break;
            }
            uint32_t remaining = e->args_total - src_off;
            uint32_t cnt = (length < remaining) ? length : remaining;
            if (cnt > max) cnt = max;
            if (cnt > 0 && e->args_blob)
                memcpy(data + off, e->args_blob + src_off, cnt);
            r_length = cnt;
            break;
        }

        default:
            r_status = MMIO_STATUS_ERR;
            break;
        }

        /* Write response descriptor */
        uint32_t resp_head = rd32(m, mb + MMIO_RESP_HEAD);
        uint32_t resp_tail = rd32(m, mb + MMIO_RESP_TAIL);
        uint32_t next_head = (resp_head + 1) % MMIO_RING_ENTRIES;
        if (next_head != resp_tail) {  /* Not full */
            uint32_t ra = mb + MMIO_RESP_RING + resp_head * MMIO_DESC_SIZE;
            if (!range_ok_u32(ra, MMIO_DESC_SIZE, e->mem_total)) {
                fprintf(stderr, "MMIO fault: response descriptor out of bounds\n");
                e->halted = true;
                return;
            }
            wr32(m, ra + 0,  r_opcode);
            wr32(m, ra + 4,  r_length);
            wr32(m, ra + 8,  r_offset);
            wr32(m, ra + 12, r_status);
            wr32(m, mb + MMIO_RESP_HEAD, next_head);
        }

        /* Advance request tail */
        req_tail = (req_tail + 1) % MMIO_RING_ENTRIES;
        wr32(m, mb + MMIO_REQ_TAIL, req_tail);
    }
}

/* ======================================================================
 * CPU execution — one instruction per call
 * ====================================================================== */

static void step(emu_t *e) {
    #define MEM_FAULT(what, addr, size) do { \
        fprintf(stderr, what " fault: addr=0x%08X size=%u total=0x%08X PC=0x%08X\n", \
                (uint32_t)(addr), (uint32_t)(size), e->mem_total, e->pc); \
        e->halted = true; \
        return; \
    } while (0)
    #define CHECK_MEM(what, addr, size) do { \
        if (!range_ok_u32((uint32_t)(addr), (uint32_t)(size), e->mem_total)) \
            MEM_FAULT(what, addr, size); \
    } while (0)

    /* Bounds check PC */
    if (!range_ok_u32(e->pc, 4, e->mem_total)) {
        fprintf(stderr, "Execute fault: PC=0x%08X out of memory (total=0x%08X)\n",
                e->pc, e->mem_total);
        e->halted = true;
        return;
    }
    if (e->pc > e->code_limit || e->code_limit - e->pc < 4) {
        fprintf(stderr, "Execute fault: PC=0x%08X past code_limit=0x%08X\n",
                e->pc, e->code_limit);
        e->halted = true;
        return;
    }

    uint32_t raw = rd32(e->mem, e->pc);
    uint32_t op  = raw & 0x7F;
    uint32_t rd  = (raw >> 7) & 0x1F;
    uint32_t rs1 = (raw >> 15) & 0x1F;
    uint32_t rs2 = (raw >> 20) & 0x1F;

    /* Pre-compute all immediate formats (only one used per instruction) */
    int32_t  imm_i  = ((int32_t)raw) >> 20;                   /* Sign-extended */
    uint32_t imm_iz = (raw >> 20) & 0xFFF;                    /* Zero-extended */
    int32_t  imm_s  = ((raw >> 7) & 0x1F) | (((int32_t)raw >> 25) << 5);
    int32_t  imm_b  = (((raw >> 8) & 0xF) << 1)
                    | (((raw >> 25) & 0x3F) << 5)
                    | (((raw >> 7) & 0x1) << 11)
                    | (((int32_t)raw >> 31) << 12);
    uint32_t imm_u  = raw & 0xFFFFF000u;
    uint32_t jbits  = (((raw >> 31) & 1) << 20)
                    | (((raw >> 12) & 0xFF) << 12)
                    | (((raw >> 20) & 1) << 11)
                    | (((raw >> 21) & 0x3FF) << 1);
    int32_t  imm_j  = (jbits & 0x100000) ? (int32_t)(jbits | 0xFFE00000u)
                                          : (int32_t)jbits;

    uint32_t next_pc = e->pc + 4;
    uint32_t *r = e->r;
    uint8_t  *m = e->mem;

    switch (op) {
    /* ---- R-type arithmetic (0x00-0x0F) ---- */
    case 0x00: r[rd] = r[rs1] + r[rs2]; break;                     /* ADD  */
    case 0x01: r[rd] = r[rs1] - r[rs2]; break;                     /* SUB  */
    case 0x02: r[rd] = r[rs1] ^ r[rs2]; break;                     /* XOR  */
    case 0x03: r[rd] = r[rs1] | r[rs2]; break;                     /* OR   */
    case 0x04: r[rd] = r[rs1] & r[rs2]; break;                     /* AND  */
    case 0x05: r[rd] = r[rs1] << (r[rs2] & 0x1F); break;           /* SLL  */
    case 0x06: r[rd] = r[rs1] >> (r[rs2] & 0x1F); break;           /* SRL  */
    case 0x07: r[rd] = (int32_t)r[rs1] >> (r[rs2] & 0x1F); break;  /* SRA  */
    case 0x08: r[rd] = ((int32_t)r[rs1] < (int32_t)r[rs2]) ? 1:0; break;   /* SLT  */
    case 0x09: r[rd] = (r[rs1] < r[rs2]) ? 1 : 0; break;                   /* SLTU */
    case 0x0A: r[rd] = r[rs1] * r[rs2]; break;                     /* MUL  */
    case 0x0B: { /* MULH (signed) */
        int64_t p = (int64_t)(int32_t)r[rs1] * (int64_t)(int32_t)r[rs2];
        r[rd] = (uint32_t)(p >> 32);
        break;
    }
    case 0x0C: /* DIV */
        if (r[rs2] == 0) r[rd] = 0xFFFFFFFF;
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0x80000000;
        else r[rd] = (int32_t)r[rs1] / (int32_t)r[rs2];
        break;
    case 0x0D: /* REM */
        if (r[rs2] == 0) r[rd] = r[rs1];
        else if (r[rs1] == 0x80000000 && r[rs2] == 0xFFFFFFFF) r[rd] = 0;
        else r[rd] = (int32_t)r[rs1] % (int32_t)r[rs2];
        break;
    case 0x0E: r[rd] = (r[rs1] == r[rs2]) ? 1 : 0; break;  /* SEQ  */
    case 0x0F: r[rd] = (r[rs1] != r[rs2]) ? 1 : 0; break;  /* SNE  */

    /* ---- I-type arithmetic (0x10-0x1F) ---- */
    case 0x10: r[rd] = r[rs1] + imm_i; break;               /* ADDI (sign-ext) */
    case 0x11: r[rd] = r[rs1] | imm_iz; break;              /* ORI  (zero-ext) */
    case 0x12: r[rd] = r[rs1] & imm_iz; break;              /* ANDI (zero-ext) */
    case 0x13: r[rd] = r[rs1] << (imm_i & 0x1F); break;     /* SLLI (sign-ext) */
    case 0x14: r[rd] = r[rs1] >> (imm_i & 0x1F); break;     /* SRLI (sign-ext) */
    case 0x15: r[rd] = (int32_t)r[rs1] >> (imm_i & 0x1F); break;  /* SRAI */
    case 0x16: r[rd] = ((int32_t)r[rs1] < imm_i) ? 1 : 0; break;  /* SLTI */
    case 0x17: r[rd] = (r[rs1] < imm_iz) ? 1 : 0; break;          /* SLTIU */
    case 0x18: r[rd] = ((int32_t)r[rs1] > (int32_t)r[rs2]) ? 1:0; break;   /* SGT  */
    case 0x19: r[rd] = (r[rs1] > r[rs2]) ? 1 : 0; break;                   /* SGTU */
    case 0x1A: r[rd] = ((int32_t)r[rs1] <= (int32_t)r[rs2]) ? 1:0; break;  /* SLE  */
    case 0x1B: r[rd] = (r[rs1] <= r[rs2]) ? 1 : 0; break;                  /* SLEU */
    case 0x1C: r[rd] = ((int32_t)r[rs1] >= (int32_t)r[rs2]) ? 1:0; break;  /* SGE  */
    case 0x1D: r[rd] = (r[rs1] >= r[rs2]) ? 1 : 0; break;                  /* SGEU */
    case 0x1E: r[rd] = r[rs1] ^ imm_iz; break;              /* XORI (zero-ext) */
    case 0x1F: { /* MULHU (unsigned) */
        uint64_t p = (uint64_t)r[rs1] * (uint64_t)r[rs2];
        r[rd] = (uint32_t)(p >> 32);
        break;
    }

    /* ---- U-type (0x20) ---- */
    case 0x20: r[rd] = imm_u; break;                        /* LUI */

    /* ---- Load instructions (0x30-0x34) ---- */
    case 0x30: { /* LDB (sign-extend) */
        uint32_t a = r[rs1] + imm_i;
        CHECK_MEM("Load", a, 1);
        r[rd] = (int32_t)(int8_t)m[a];
        break;
    }
    case 0x31: { /* LDH (sign-extend) */
        uint32_t a = r[rs1] + imm_i;
        CHECK_MEM("Load", a, 2);
        r[rd] = (int32_t)(int16_t)rd16(m, a);
        break;
    }
    case 0x32: { /* LDW */
        uint32_t a = r[rs1] + imm_i;
        CHECK_MEM("Load", a, 4);
        r[rd] = rd32(m, a);
        break;
    }
    case 0x33: { /* LDBU (zero-extend) */
        uint32_t a = r[rs1] + imm_i;
        CHECK_MEM("Load", a, 1);
        r[rd] = m[a];
        break;
    }
    case 0x34: { /* LDHU (zero-extend) */
        uint32_t a = r[rs1] + imm_i;
        CHECK_MEM("Load", a, 2);
        r[rd] = rd16(m, a);
        break;
    }

    /* ---- Store instructions (0x38-0x3A) ---- */
    case 0x38: { /* STB */
        uint32_t a = r[rs1] + imm_s;
        CHECK_MEM("Store", a, 1);
        m[a] = (uint8_t)r[rs2];
        break;
    }
    case 0x39: { /* STH */
        uint32_t a = r[rs1] + imm_s;
        CHECK_MEM("Store", a, 2);
        wr16(m, a, (uint16_t)r[rs2]);
        break;
    }
    case 0x3A: { /* STW */
        uint32_t a = r[rs1] + imm_s;
        CHECK_MEM("Store", a, 4);
        wr32(m, a, r[rs2]);
        break;
    }

    /* ---- ASSERT_EQ (0x3F) ---- */
    case 0x3F:
        if (r[rs1] != r[rs2]) {
            fprintf(stderr, "Assertion failed: r%d(0x%08X) != r%d(0x%08X) at PC=0x%08X\n",
                    rs1, r[rs1], rs2, r[rs2], e->pc);
            e->halted = true;
        }
        break;

    /* ---- Jump instructions (0x40-0x41) ---- */
    case 0x40: /* JAL */
        r[rd] = e->pc + 4;
        next_pc = e->pc + imm_j;
        break;
    case 0x41: /* JALR */
        r[rd] = e->pc + 4;
        next_pc = (r[rs1] + imm_i) & ~1u;
        break;

    /* ---- Branch instructions (0x48-0x4D) — PC+4 base ---- */
    case 0x48: if (r[rs1] == r[rs2]) next_pc += imm_b; break;               /* BEQ  */
    case 0x49: if (r[rs1] != r[rs2]) next_pc += imm_b; break;               /* BNE  */
    case 0x4A: if ((int32_t)r[rs1] < (int32_t)r[rs2]) next_pc += imm_b; break;  /* BLT  */
    case 0x4B: if ((int32_t)r[rs1] >= (int32_t)r[rs2]) next_pc += imm_b; break; /* BGE  */
    case 0x4C: if (r[rs1] < r[rs2]) next_pc += imm_b; break;                /* BLTU */
    case 0x4D: if (r[rs1] >= r[rs2]) next_pc += imm_b; break;               /* BGEU */

    /* ---- System instructions ---- */
    case 0x50: break;  /* NOP */
    case 0x51: /* YIELD — process MMIO ring buffers */
        mmio_process(e);
        break;
    case 0x52: /* DEBUG — output character */
        putchar(r[rs1] & 0xFF);
        fflush(stdout);
        break;

    case 0x7F: /* HALT */
        mmio_process(e);  /* Flush pending MMIO before halt */
        fprintf(stderr, "HALT at PC=0x%08X\n", e->pc);
        e->halted = true;
        break;

    default:
        fprintf(stderr, "Unknown opcode 0x%02X at PC=0x%08X\n", op, e->pc);
        e->halted = true;
        break;
    }

    r[0] = 0;       /* r0 is always zero */
    e->pc = next_pc;

    #undef CHECK_MEM
    #undef MEM_FAULT
}

/* ======================================================================
 * Main
 * ====================================================================== */

int main(int argc, char **argv) {
    if (argc < 2) {
        fprintf(stderr, "Usage: %s <program.s32x> [args...]\n", argv[0]);
        return 1;
    }

    emu_t e;
    emu_init(&e);

    if (!load_s32x(&e, argv[1])) {
        emu_destroy(&e);
        return 1;
    }

    /* Stage guest arguments (argv[1] onward = guest argv[0] onward) */
    stage_args(&e, argc - 1, argv + 1);

    /* Run */
    fprintf(stderr, "s32-emu: starting %s (code_limit=0x%X mmio=0x%X)\n",
            argv[1], e.code_limit, e.mmio_base);

    while (!e.halted)
        step(&e);

    int exit_code = (int)e.r[1];
    emu_destroy(&e);
    return exit_code;
}
