#include "qemu/osdep.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <poll.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "exec/cpu-common.h"
#include "hw/core/cpu.h"
#include "qemu/bswap.h"
#include "qemu/cutils.h"
#include "qemu/log.h"
#include "qemu/units.h"
#include "system/runstate.h"

#include "cpu.h"
#include "mmio.h"

#define S32_MMIO_REQ_HEAD_OFFSET    0x0000u
#define S32_MMIO_REQ_TAIL_OFFSET    0x0004u
#define S32_MMIO_REQ_RING_OFFSET    0x1000u
#define S32_MMIO_RESP_HEAD_OFFSET   0x2000u
#define S32_MMIO_RESP_TAIL_OFFSET   0x2004u
#define S32_MMIO_RESP_RING_OFFSET   0x3000u
#define S32_MMIO_DATA_BUFFER_OFFSET 0x4000u

#define S32_MMIO_RING_ENTRIES 256u
#define S32_MMIO_DESC_WORDS   4u
#define S32_MMIO_DESC_BYTES   (S32_MMIO_DESC_WORDS * sizeof(uint32_t))
#define S32_MMIO_DATA_CAPACITY (48u * 1024u)
#define S32_MMIO_PAGE_SIZE    0x1000u
#define S32_MMIO_DEFAULT_HEAP_SIZE (16 * MiB)

#define S32_MMIO_STATUS_OK    0u
#define S32_MMIO_STATUS_EOF   0xFFFFFFFDu
#define S32_MMIO_STATUS_EINTR 0xFFFFFFFEu
#define S32_MMIO_STATUS_ERR   0xFFFFFFFFu

#define S32_MMIO_OP_NOP       0x00
#define S32_MMIO_OP_PUTCHAR   0x01
#define S32_MMIO_OP_GETCHAR   0x02
#define S32_MMIO_OP_WRITE     0x03
#define S32_MMIO_OP_READ      0x04
#define S32_MMIO_OP_OPEN      0x05
#define S32_MMIO_OP_CLOSE     0x06
#define S32_MMIO_OP_SEEK      0x07
/* 0x08 reserved (was BRK, removed — heap is statically allocated by linker) */
#define S32_MMIO_OP_EXIT      0x09
#define S32_MMIO_OP_STAT      0x0A
#define S32_MMIO_OP_FLUSH     0x0B
#define S32_MMIO_OP_READ_DIRECT 0x0C
#define S32_MMIO_OP_FTRUNCATE   0x0D

#define S32_MMIO_OP_UNLINK    0x20
#define S32_MMIO_OP_RENAME    0x21
#define S32_MMIO_OP_MKDIR     0x22
#define S32_MMIO_OP_RMDIR     0x23
#define S32_MMIO_OP_LSTAT     0x24
#define S32_MMIO_OP_ACCESS    0x25
#define S32_MMIO_OP_CHDIR     0x26
#define S32_MMIO_OP_GETCWD    0x27
#define S32_MMIO_OP_OPENDIR   0x28
#define S32_MMIO_OP_READDIR   0x29
#define S32_MMIO_OP_CLOSEDIR  0x2A

#define S32_MMIO_OP_GETTIME   0x30
#define S32_MMIO_OP_SLEEP     0x31

#define S32_MMIO_OP_ARGS_INFO 0x60
#define S32_MMIO_OP_ARGS_DATA 0x61
#define S32_MMIO_OP_ENVP_INFO 0x62
#define S32_MMIO_OP_ENVP_DATA 0x63
#define S32_MMIO_OP_GETENV    0x64

/* Service negotiation opcodes (0xF0-0xF4) */
#define S32_MMIO_OP_SVC_REQUEST  0xF0
#define S32_MMIO_OP_SVC_RELEASE  0xF1
#define S32_MMIO_OP_SVC_QUERY    0xF2
#define S32_MMIO_OP_SVC_LIST     0xF3
#define S32_MMIO_OP_SVC_VERSION  0xF4

/* Service negotiation result codes */
#define S32_SVC_OK          0x00
#define S32_SVC_DENIED      0x01
#define S32_SVC_UNKNOWN     0x02
#define S32_SVC_CONFLICT    0x03
#define S32_SVC_LIMIT       0x04
#define S32_SVC_VERSION_ERR 0x05

#define S32_SVC_PROTOCOL_VERSION 1
#define S32_SVC_MAX_NAME_LEN     32
#define S32_MAX_SERVICES         16
#define S32_MAX_SVC_NAME         32

/* Term service opcode offsets (relative to negotiated base) */
#define S32_TERM_SET_MODE     0
#define S32_TERM_GET_SIZE     1
#define S32_TERM_MOVE_CURSOR  2
#define S32_TERM_CLEAR        3
#define S32_TERM_SET_ATTR     4
#define S32_TERM_READ_KEY     5
#define S32_TERM_KEY_AVAIL    6
#define S32_TERM_SET_COLOR    7
#define S32_TERM_PUTC         8
#define S32_TERM_PUTS         9
#define S32_TERM_SAVE_SCREEN  10
#define S32_TERM_RESTORE_SCREEN 11
#define S32_TERM_OPCODE_COUNT 12

typedef struct Slow32MMIODesc {
    uint32_t opcode;
    uint32_t length;
    uint32_t offset;
    uint32_t status;
} Slow32MMIODesc;

typedef struct s32_mmio_timepair64 {
    uint32_t seconds_lo;
    uint32_t seconds_hi;
    uint32_t nanoseconds;
    uint32_t reserved;
} s32_mmio_timepair64_t;

typedef struct s32_mmio_args_info {
    uint32_t argc;
    uint32_t total_bytes;
    uint32_t flags;
    uint32_t reserved;
} s32_mmio_args_info_t;

typedef struct s32_mmio_envp_info {
    uint32_t envc;
    uint32_t total_bytes;
    uint32_t flags;
    uint32_t reserved;
} s32_mmio_envp_info_t;

typedef struct QEMU_PACKED s32_mmio_stat_result {
    uint64_t st_dev;
    uint64_t st_ino;
    uint32_t st_mode;
    uint32_t st_nlink;
    uint32_t st_uid;
    uint32_t st_gid;
    uint64_t st_rdev;
    uint64_t st_size;
    uint64_t st_blksize;
    uint64_t st_blocks;
    uint64_t st_atime_sec;
    uint32_t st_atime_nsec;
    uint32_t _pad0;
    uint64_t st_mtime_sec;
    uint32_t st_mtime_nsec;
    uint32_t _pad1;
    uint64_t st_ctime_sec;
    uint32_t st_ctime_nsec;
    uint32_t _pad2;
} s32_mmio_stat_result_t;

#define S32_MMIO_MAX_FDS 128
#define S32_MMIO_STAT_PATH_SENTINEL 0xFFFFFFFFu

typedef enum {
    S32_FD_TYPE_FILE = 0,
    S32_FD_TYPE_DIR  = 1,
} Slow32FdType;

#define S32_DT_UNKNOWN 0
#define S32_DT_FIFO    1
#define S32_DT_CHR     2
#define S32_DT_DIR     4
#define S32_DT_BLK     6
#define S32_DT_REG     8
#define S32_DT_LNK     10
#define S32_DT_SOCK    12

typedef struct QEMU_PACKED s32_mmio_dirent {
    uint64_t d_ino;
    uint32_t d_type;
    uint32_t d_namlen;
    char d_name[256];
} s32_mmio_dirent_t;

#define S32_MMIO_DIRENT_SIZE sizeof(s32_mmio_dirent_t)

/* Forward declaration for service session handler */
typedef struct Slow32MMIOContext Slow32MMIOCtx;

/* Service session (one per granted service) */
typedef struct {
    bool active;
    char name[S32_MAX_SVC_NAME];
    uint32_t base_opcode;
    uint32_t opcode_count;
    uint32_t version;
    void *state;
    void (*cleanup)(void *state);
    void (*handle)(void *state, Slow32MMIOCtx *ctx,
                   const CPUSlow32State *env,
                   uint32_t sub_opcode, const Slow32MMIODesc *req,
                   Slow32MMIODesc *resp);
} Slow32SvcSession;

/* Policy engine */
typedef struct {
    bool default_allow;
    char allow_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int allow_count;
    char deny_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int deny_count;
} Slow32SvcPolicy;

struct Slow32MMIOContext {
    bool enabled;
    uint32_t req_tail;
    uint32_t resp_head;
    uint32_t args_argc;
    uint32_t args_total_bytes;
    GByteArray *args_blob;
    uint32_t envp_envc;
    uint32_t envp_total_bytes;
    GByteArray *envp_blob;
    int host_fds[S32_MMIO_MAX_FDS];
    bool host_fd_owned[S32_MMIO_MAX_FDS];
    Slow32FdType fd_types[S32_MMIO_MAX_FDS];
    DIR *host_dirs[S32_MMIO_MAX_FDS];
    uint8_t scratch[S32_MMIO_DATA_CAPACITY];

    /* Service negotiation */
    Slow32SvcSession services[S32_MAX_SERVICES];
    int num_services;
    Slow32SvcPolicy policy;
    uint32_t next_dynamic_opcode;
};

static inline bool slow32_mmio_is_enabled(const CPUSlow32State *env)
{
    return env->mmio_base != 0;
}

static inline hwaddr slow32_mmio_addr(const CPUSlow32State *env,
                                      uint32_t offset)
{
    return (hwaddr)env->mmio_base + offset;
}

static uint32_t slow32_mmio_readl(const CPUSlow32State *env, uint32_t offset)
{
    uint32_t value = 0;

    cpu_physical_memory_read(slow32_mmio_addr(env, offset), &value,
                             sizeof(value));
    return le32_to_cpu(value);
}

static void slow32_mmio_writel(const CPUSlow32State *env, uint32_t offset,
                               uint32_t value)
{
    uint32_t tmp = cpu_to_le32(value);

    cpu_physical_memory_write(slow32_mmio_addr(env, offset), &tmp,
                              sizeof(tmp));
}

static uint32_t slow32_mmio_ring_next(uint32_t index)
{
    return (index + 1u) % S32_MMIO_RING_ENTRIES;
}

static void slow32_mmio_clear_window(const CPUSlow32State *env)
{
    if (!slow32_mmio_is_enabled(env)) {
        return;
    }

    uint8_t zero[256] = {0};
    hwaddr base = slow32_mmio_addr(env, 0);

    for (uint32_t offset = 0; offset < S32_MMIO_WINDOW_SIZE;
         offset += sizeof(zero)) {
        cpu_physical_memory_write(base + offset, zero,
                                  MIN(sizeof(zero),
                                      S32_MMIO_WINDOW_SIZE - offset));
    }
}

static void slow32_mmio_reset_fd_table(Slow32MMIOContext *ctx)
{
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        /* Close any open directory handles */
        if (ctx->host_dirs[i]) {
            closedir(ctx->host_dirs[i]);
        }
        ctx->host_fds[i] = -1;
        ctx->host_fd_owned[i] = false;
        ctx->fd_types[i] = S32_FD_TYPE_FILE;
        ctx->host_dirs[i] = NULL;
    }
    /* Map guest fds 0, 1, 2 to host stdin, stdout, stderr */
    ctx->host_fds[0] = STDIN_FILENO;
    ctx->host_fds[1] = STDOUT_FILENO;
    ctx->host_fds[2] = STDERR_FILENO;
}

static int slow32_mmio_alloc_guest_fd(Slow32MMIOContext *ctx, int host_fd,
                                       bool owned)
{
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        if (ctx->host_fds[i] == -1 && ctx->host_dirs[i] == NULL) {
            ctx->host_fds[i] = host_fd;
            ctx->host_fd_owned[i] = owned;
            ctx->fd_types[i] = S32_FD_TYPE_FILE;
            ctx->host_dirs[i] = NULL;
            return (int)i;
        }
    }
    return -1;
}

static int slow32_mmio_alloc_guest_dir_fd(Slow32MMIOContext *ctx, DIR *host_dir)
{
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        if (ctx->host_fds[i] == -1 && ctx->host_dirs[i] == NULL) {
            ctx->host_fds[i] = -1;
            ctx->host_fd_owned[i] = true;
            ctx->fd_types[i] = S32_FD_TYPE_DIR;
            ctx->host_dirs[i] = host_dir;
            return (int)i;
        }
    }
    return -1;
}

static DIR *slow32_mmio_host_dir_for_guest(Slow32MMIOContext *ctx,
                                            uint32_t guest_fd)
{
    if (guest_fd >= S32_MMIO_MAX_FDS) {
        return NULL;
    }
    if (ctx->fd_types[guest_fd] != S32_FD_TYPE_DIR) {
        return NULL;
    }
    return ctx->host_dirs[guest_fd];
}

static int slow32_mmio_host_fd_for_guest(Slow32MMIOContext *ctx,
                                          uint32_t guest_fd)
{
    if (guest_fd >= S32_MMIO_MAX_FDS) {
        return -1;
    }
    return ctx->host_fds[guest_fd];
}

static int slow32_mmio_translate_open_flags(uint32_t guest_flags,
                                             bool *needs_mode)
{
    const uint32_t flag_read = 0x01u;
    const uint32_t flag_write = 0x02u;
    const uint32_t flag_append = 0x04u;
    const uint32_t flag_create = 0x08u;
    const uint32_t flag_trunc = 0x10u;
    const uint32_t known = flag_read | flag_write | flag_append |
                           flag_create | flag_trunc;

    if ((guest_flags & ~known) == 0u) {
        int flags = (guest_flags & flag_write)
                        ? ((guest_flags & flag_read) ? O_RDWR : O_WRONLY)
                        : O_RDONLY;
        if (guest_flags & flag_append) {
            flags |= O_APPEND;
        }
        if (guest_flags & flag_create) {
            flags |= O_CREAT;
        }
        if (guest_flags & flag_trunc) {
            flags |= O_TRUNC;
        }
        *needs_mode = (flags & O_CREAT) != 0;
        return flags;
    }

    *needs_mode = (guest_flags & O_CREAT) != 0;
    return (int)guest_flags;
}

/* Forward declarations for copy helpers used by service handlers */
static void slow32_mmio_copy_from_guest(const CPUSlow32State *env,
                                        uint32_t offset, uint8_t *dst,
                                        uint32_t length);
static void slow32_mmio_copy_to_guest(const CPUSlow32State *env,
                                      uint32_t offset, const uint8_t *src,
                                      uint32_t length);

/* ========== Policy engine ========== */

static bool slow32_mmio_policy_allows(Slow32MMIOContext *ctx, const char *name)
{
    /* Check deny list first */
    for (int i = 0; i < ctx->policy.deny_count; i++) {
        if (strcmp(ctx->policy.deny_list[i], name) == 0) {
            return false;
        }
    }
    /* If there's an explicit allow list, only allow listed services */
    if (ctx->policy.allow_count > 0) {
        for (int i = 0; i < ctx->policy.allow_count; i++) {
            if (strcmp(ctx->policy.allow_list[i], name) == 0) {
                return true;
            }
        }
        return false;
    }
    return ctx->policy.default_allow;
}

static void slow32_mmio_cleanup_services(Slow32MMIOContext *ctx)
{
    for (int i = 0; i < ctx->num_services; i++) {
        Slow32SvcSession *svc = &ctx->services[i];
        if (svc->active && svc->cleanup && svc->state) {
            svc->cleanup(svc->state);
        }
        svc->active = false;
        svc->state = NULL;
    }
    ctx->num_services = 0;
}

/* ========== Term service implementation ========== */

#define TERM_MAX_ROWS 256
#define TERM_MAX_COLS 256
#define TERM_MAX_SAVE_DEPTH 8

typedef struct {
    uint8_t ch;
    uint8_t attr;   /* 0=normal, 1=bold, 7=reverse, etc. */
    uint8_t fg;     /* ANSI 0-7 */
    uint8_t bg;     /* ANSI 0-7 */
} Slow32TermCell;

typedef struct {
    Slow32TermCell *cells;    /* rows * cols cells */
    int rows, cols;
    int cur_row, cur_col;     /* 0-based */
    int cur_attr;
    int cur_fg, cur_bg;
} Slow32TermScreenSave;

typedef struct {
    struct termios saved_termios;
    bool raw_mode;
    bool termios_saved;
    /* Shadow screen buffer */
    int rows, cols;
    int cur_row, cur_col;     /* 0-based */
    int cur_attr;
    int cur_fg, cur_bg;
    Slow32TermCell *cells;    /* rows * cols, heap-allocated */
    /* Save stack */
    Slow32TermScreenSave save_stack[TERM_MAX_SAVE_DEPTH];
    int save_depth;
} Slow32TermState;

static void *slow32_term_create(void)
{
    Slow32TermState *ts = g_new0(Slow32TermState, 1);
    if (isatty(STDIN_FILENO) &&
        tcgetattr(STDIN_FILENO, &ts->saved_termios) == 0) {
        ts->termios_saved = true;
    }
    /* Initialize shadow screen buffer */
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) {
        ts->rows = 24;
        ts->cols = 80;
    } else {
        ts->rows = (ws.ws_row > TERM_MAX_ROWS) ? TERM_MAX_ROWS : ws.ws_row;
        ts->cols = (ws.ws_col > TERM_MAX_COLS) ? TERM_MAX_COLS : ws.ws_col;
    }
    if (ts->rows < 1) {
        ts->rows = 24;
    }
    if (ts->cols < 1) {
        ts->cols = 80;
    }
    ts->cells = g_new0(Slow32TermCell, (size_t)ts->rows * ts->cols);
    for (int i = 0; i < ts->rows * ts->cols; i++) {
        ts->cells[i].ch = ' ';
        ts->cells[i].fg = 7;  /* default white */
    }
    ts->cur_fg = 7;
    ts->cur_bg = 0;
    ts->save_depth = 0;
    return ts;
}

static void slow32_term_cleanup(void *state)
{
    Slow32TermState *ts = state;
    if (!ts) {
        return;
    }
    if (ts->raw_mode && ts->termios_saved) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &ts->saved_termios);
    }
    for (int i = 0; i < ts->save_depth; i++) {
        g_free(ts->save_stack[i].cells);
    }
    g_free(ts->cells);
    g_free(ts);
}

/* Shadow buffer helpers */

static inline Slow32TermCell *slow32_term_cell_at(Slow32TermState *ts,
                                                   int row, int col)
{
    if (!ts->cells || row < 0 || row >= ts->rows ||
        col < 0 || col >= ts->cols) {
        return NULL;
    }
    return &ts->cells[row * ts->cols + col];
}

static void slow32_term_shadow_putc(Slow32TermState *ts, int ch)
{
    if (!ts->cells) {
        return;
    }
    if (ch == '\n') {
        ts->cur_row++;
        ts->cur_col = 0;
        return;
    }
    if (ch == '\r') {
        ts->cur_col = 0;
        return;
    }
    if (ch == '\t') {
        ts->cur_col = (ts->cur_col + 8) & ~7;
        if (ts->cur_col >= ts->cols) {
            ts->cur_col = 0;
            ts->cur_row++;
        }
        return;
    }
    if (ch < 0x20) {
        return;  /* skip other control chars */
    }
    Slow32TermCell *c = slow32_term_cell_at(ts, ts->cur_row, ts->cur_col);
    if (c) {
        c->ch = (uint8_t)ch;
        c->attr = (uint8_t)ts->cur_attr;
        c->fg = (uint8_t)ts->cur_fg;
        c->bg = (uint8_t)ts->cur_bg;
    }
    ts->cur_col++;
    if (ts->cur_col >= ts->cols) {
        ts->cur_col = 0;
        ts->cur_row++;
    }
}

static void slow32_term_shadow_clear(Slow32TermState *ts, int mode)
{
    if (!ts->cells) {
        return;
    }
    int start, end;
    switch (mode) {
    case 0: /* full screen */
        start = 0;
        end = ts->rows * ts->cols;
        ts->cur_row = 0;
        ts->cur_col = 0;
        break;
    case 1: /* to end of line */
        start = ts->cur_row * ts->cols + ts->cur_col;
        end = (ts->cur_row + 1) * ts->cols;
        break;
    case 2: /* to end of screen */
        start = ts->cur_row * ts->cols + ts->cur_col;
        end = ts->rows * ts->cols;
        break;
    default:
        start = 0;
        end = ts->rows * ts->cols;
        ts->cur_row = 0;
        ts->cur_col = 0;
        break;
    }
    if (start < 0) {
        start = 0;
    }
    if (end > ts->rows * ts->cols) {
        end = ts->rows * ts->cols;
    }
    for (int i = start; i < end; i++) {
        ts->cells[i].ch = ' ';
        ts->cells[i].attr = 0;
        ts->cells[i].fg = 7;
        ts->cells[i].bg = 0;
    }
}

static void slow32_term_handle(void *state, Slow32MMIOCtx *ctx,
                                const CPUSlow32State *env,
                                uint32_t sub_opcode,
                                const Slow32MMIODesc *req,
                                Slow32MMIODesc *resp)
{
    Slow32TermState *ts = state;

    switch (sub_opcode) {
    case S32_TERM_SET_MODE: {
        if (!isatty(STDIN_FILENO)) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }
        if (req->status) {
            /* Enter raw mode */
            struct termios raw;
            if (ts->termios_saved) {
                raw = ts->saved_termios;
            } else {
                tcgetattr(STDIN_FILENO, &raw);
            }
            raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
            raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
            raw.c_oflag &= ~(OPOST);
            raw.c_cc[VMIN] = 1;
            raw.c_cc[VTIME] = 0;
            tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
            ts->raw_mode = true;
        } else {
            /* Restore cooked mode */
            if (ts->termios_saved) {
                tcsetattr(STDIN_FILENO, TCSAFLUSH, &ts->saved_termios);
            }
            ts->raw_mode = false;
        }
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_GET_SIZE: {
        struct winsize ws;
        if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) {
            ws.ws_row = 24;
            ws.ws_col = 80;
        }
        uint32_t buf[2];
        buf[0] = ws.ws_row;
        buf[1] = ws.ws_col;
        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)buf, sizeof(buf));
        resp->length = 8;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_MOVE_CURSOR: {
        uint32_t row = (req->status >> 16) & 0xFFFF;
        uint32_t col = req->status & 0xFFFF;
        fprintf(stdout, "\033[%u;%uH", row, col);
        fflush(stdout);
        ts->cur_row = (int)row - 1;  /* shadow: 0-based */
        ts->cur_col = (int)col - 1;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_CLEAR: {
        switch (req->status) {
        case 0:
            fprintf(stdout, "\033[2J\033[H");
            break;
        case 1:
            fprintf(stdout, "\033[K");
            break;
        case 2:
            fprintf(stdout, "\033[J");
            break;
        default:
            fprintf(stdout, "\033[2J\033[H");
            break;
        }
        fflush(stdout);
        slow32_term_shadow_clear(ts, (int)req->status);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_SET_ATTR:
        fprintf(stdout, "\033[%um", req->status);
        fflush(stdout);
        ts->cur_attr = (int)req->status;
        resp->status = S32_MMIO_STATUS_OK;
        break;

    case S32_TERM_READ_KEY: {
        unsigned char ch;
        ssize_t n = read(STDIN_FILENO, &ch, 1);
        if (n == 1) {
            slow32_mmio_copy_to_guest(env, req->offset, &ch, 1);
            resp->length = 1;
            resp->status = (uint32_t)ch;
        } else {
            resp->status = S32_MMIO_STATUS_EOF;
            resp->length = 0;
        }
        break;
    }

    case S32_TERM_KEY_AVAIL: {
        struct pollfd pfd = { .fd = STDIN_FILENO, .events = POLLIN };
        int ret = poll(&pfd, 1, 0);
        resp->status = (ret > 0 && (pfd.revents & POLLIN)) ? 1 : 0;
        break;
    }

    case S32_TERM_SET_COLOR: {
        uint32_t fg = (req->status >> 8) & 0xFF;
        uint32_t bg = req->status & 0xFF;
        fprintf(stdout, "\033[3%u;4%um", fg, bg);
        fflush(stdout);
        ts->cur_fg = (int)fg;
        ts->cur_bg = (int)bg;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_PUTC: {
        int ch = (int)(req->status & 0xFF);
        fputc(ch, stdout);
        fflush(stdout);
        slow32_term_shadow_putc(ts, ch);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_PUTS: {
        uint32_t len = req->length;
        if (len > S32_MMIO_DATA_CAPACITY) {
            len = S32_MMIO_DATA_CAPACITY;
        }
        if (len > 0) {
            slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, len);
            fwrite(ctx->scratch, 1, len, stdout);
            for (uint32_t i = 0; i < len; i++) {
                slow32_term_shadow_putc(ts, ctx->scratch[i]);
            }
        }
        fflush(stdout);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_SAVE_SCREEN: {
        if (!ts->cells || ts->save_depth >= TERM_MAX_SAVE_DEPTH) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }
        size_t ncells = (size_t)ts->rows * ts->cols;
        Slow32TermCell *snap = g_memdup2(ts->cells,
                                          ncells * sizeof(Slow32TermCell));
        Slow32TermScreenSave *s = &ts->save_stack[ts->save_depth++];
        s->cells = snap;
        s->rows = ts->rows;
        s->cols = ts->cols;
        s->cur_row = ts->cur_row;
        s->cur_col = ts->cur_col;
        s->cur_attr = ts->cur_attr;
        s->cur_fg = ts->cur_fg;
        s->cur_bg = ts->cur_bg;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_TERM_RESTORE_SCREEN: {
        if (ts->save_depth <= 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }
        Slow32TermScreenSave *s = &ts->save_stack[--ts->save_depth];
        /* Repaint: clear screen, then redraw all cells */
        fprintf(stdout, "\033[0m\033[2J\033[H");
        int prev_attr = 0, prev_fg = 7, prev_bg = 0;
        int paint_rows = (s->rows < ts->rows) ? s->rows : ts->rows;
        int paint_cols = (s->cols < ts->cols) ? s->cols : ts->cols;
        for (int r = 0; r < paint_rows; r++) {
            fprintf(stdout, "\033[%d;1H", r + 1);
            int last_written_col = -1;
            for (int c = 0; c < paint_cols; c++) {
                Slow32TermCell *cell = &s->cells[r * s->cols + c];
                /* Skip trailing spaces with default attributes */
                if (cell->ch == ' ' && cell->attr == 0 &&
                    cell->fg == 7 && cell->bg == 0) {
                    continue;
                }
                /* Position cursor if we skipped columns */
                if (c != last_written_col + 1) {
                    fprintf(stdout, "\033[%d;%dH", r + 1, c + 1);
                }
                /* Set attributes if changed */
                if (cell->attr != prev_attr) {
                    fprintf(stdout, "\033[%um", (unsigned)cell->attr);
                    prev_attr = cell->attr;
                }
                if (cell->fg != prev_fg || cell->bg != prev_bg) {
                    fprintf(stdout, "\033[3%u;4%um",
                            (unsigned)cell->fg, (unsigned)cell->bg);
                    prev_fg = cell->fg;
                    prev_bg = cell->bg;
                }
                fputc(cell->ch, stdout);
                last_written_col = c;
            }
        }
        /* Restore shadow buffer from saved state */
        size_t ncells = (size_t)ts->rows * ts->cols;
        if (s->rows == ts->rows && s->cols == ts->cols) {
            memcpy(ts->cells, s->cells, ncells * sizeof(Slow32TermCell));
        } else {
            /* Dimension mismatch: clear and copy what fits */
            for (size_t i = 0; i < ncells; i++) {
                ts->cells[i].ch = ' ';
                ts->cells[i].attr = 0;
                ts->cells[i].fg = 7;
                ts->cells[i].bg = 0;
            }
            for (int r = 0; r < paint_rows; r++) {
                memcpy(&ts->cells[r * ts->cols],
                       &s->cells[r * s->cols],
                       (size_t)paint_cols * sizeof(Slow32TermCell));
            }
        }
        ts->cur_row = s->cur_row;
        ts->cur_col = s->cur_col;
        ts->cur_attr = s->cur_attr;
        ts->cur_fg = s->cur_fg;
        ts->cur_bg = s->cur_bg;
        /* Restore cursor position and attributes on terminal */
        fprintf(stdout, "\033[%um", (unsigned)ts->cur_attr);
        fprintf(stdout, "\033[3%u;4%um",
                (unsigned)ts->cur_fg, (unsigned)ts->cur_bg);
        fprintf(stdout, "\033[%d;%dH",
                ts->cur_row + 1, ts->cur_col + 1);
        fflush(stdout);
        g_free(s->cells);
        s->cells = NULL;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    default:
        resp->status = S32_MMIO_STATUS_ERR;
        break;
    }
}

/* ========== Built-in service table ========== */

typedef struct {
    const char *name;
    uint32_t opcode_count;
    uint32_t version;
    void *(*create)(void);
    void (*cleanup)(void *state);
    void (*handle)(void *state, Slow32MMIOCtx *ctx,
                   const CPUSlow32State *env,
                   uint32_t sub_opcode, const Slow32MMIODesc *req,
                   Slow32MMIODesc *resp);
} Slow32BuiltinService;

static const Slow32BuiltinService builtin_services[] = {
    {
        .name = "term",
        .opcode_count = S32_TERM_OPCODE_COUNT,
        .version = 1,
        .create = slow32_term_create,
        .cleanup = slow32_term_cleanup,
        .handle = slow32_term_handle,
    },
};

#define NUM_BUILTIN_SERVICES ARRAY_SIZE(builtin_services)

static const Slow32BuiltinService *slow32_find_builtin_service(const char *name)
{
    for (size_t i = 0; i < NUM_BUILTIN_SERVICES; i++) {
        if (strcmp(builtin_services[i].name, name) == 0) {
            return &builtin_services[i];
        }
    }
    return NULL;
}

static void slow32_mmio_init_default_args(Slow32MMIOContext *ctx)
{
    ctx->args_argc = 1;
    ctx->args_total_bytes = 1;
    g_byte_array_set_size(ctx->args_blob, 0);
    uint8_t zero = 0;
    g_byte_array_append(ctx->args_blob, &zero, 1);
}

void slow32_mmio_set_args(Slow32CPU *cpu, int argc, char **argv)
{
    Slow32MMIOContext *ctx = cpu->mmio;

    if (!ctx) {
        return;
    }

    if (argc <= 0 || !argv) {
        slow32_mmio_init_default_args(ctx);
        return;
    }

    g_byte_array_set_size(ctx->args_blob, 0);
    uint32_t total_bytes = 0;

    for (int i = 0; i < argc; ++i) {
        const char *arg = argv[i] ? argv[i] : "";
        size_t len = strlen(arg) + 1;
        g_byte_array_append(ctx->args_blob, (const guint8 *)arg, len);
        total_bytes += len;
    }

    ctx->args_argc = (uint32_t)argc;
    ctx->args_total_bytes = total_bytes;
}

static void slow32_mmio_init_default_envp(Slow32MMIOContext *ctx)
{
    ctx->envp_envc = 0;
    ctx->envp_total_bytes = 0;
    g_byte_array_set_size(ctx->envp_blob, 0);

    /* Populate from host environment (environ is declared in unistd.h) */
    if (!environ) {
        return;
    }

    /* Count environment variables and total bytes needed */
    uint32_t envc = 0;
    uint64_t total_bytes = 0;
    for (char **p = environ; *p != NULL; ++p) {
        size_t len = strlen(*p) + 1;
        total_bytes += len;
        envc++;
        /* Safety cap to avoid excessive memory usage */
        if (total_bytes > (128u * 1024u)) {
            break;
        }
    }

    if (envc == 0 || total_bytes == 0) {
        return;
    }

    /* Build the blob of NUL-terminated KEY=VALUE strings */
    g_byte_array_set_size(ctx->envp_blob, 0);
    uint32_t count = 0;
    for (char **p = environ; *p != NULL && count < envc; ++p, ++count) {
        size_t len = strlen(*p) + 1;
        g_byte_array_append(ctx->envp_blob, (const guint8 *)*p, len);
    }

    ctx->envp_envc = envc;
    ctx->envp_total_bytes = (uint32_t)ctx->envp_blob->len;
}

void slow32_mmio_context_init(Slow32CPU *cpu)
{
    g_assert(cpu->mmio == NULL);
    cpu->mmio = g_new0(Slow32MMIOContext, 1);
    cpu->mmio->args_blob = g_byte_array_sized_new(16);
    cpu->mmio->envp_blob = g_byte_array_sized_new(16);
    slow32_mmio_init_default_args(cpu->mmio);
    slow32_mmio_init_default_envp(cpu->mmio);

    /* Service negotiation defaults */
    cpu->mmio->num_services = 0;
    cpu->mmio->next_dynamic_opcode = 0x80;
    cpu->mmio->policy.default_allow = true;
    cpu->mmio->policy.allow_count = 0;
    cpu->mmio->policy.deny_count = 0;
}

void slow32_mmio_context_destroy(Slow32CPU *cpu)
{
    if (!cpu->mmio) {
        return;
    }
    slow32_mmio_cleanup_services(cpu->mmio);
    if (cpu->mmio->args_blob) {
        g_byte_array_unref(cpu->mmio->args_blob);
        cpu->mmio->args_blob = NULL;
    }
    if (cpu->mmio->envp_blob) {
        g_byte_array_unref(cpu->mmio->envp_blob);
        cpu->mmio->envp_blob = NULL;
    }
    g_free(cpu->mmio);
    cpu->mmio = NULL;
}

static void slow32_mmio_apply_reset(Slow32CPU *cpu, bool clear_window_first)
{
    Slow32MMIOContext *ctx = cpu->mmio;
    CPUSlow32State *env = &cpu->env;

    if (!ctx) {
        return;
    }

    ctx->enabled = slow32_mmio_is_enabled(env);
    if (!ctx->enabled) {
        ctx->req_tail = 0;
        ctx->resp_head = 0;
        return;
    }

    if (clear_window_first) {
        slow32_mmio_clear_window(env);
    }

    ctx->req_tail = 0;
    ctx->resp_head = 0;
    slow32_mmio_reset_fd_table(ctx);
    slow32_mmio_cleanup_services(ctx);
    ctx->next_dynamic_opcode = 0x80;

    slow32_mmio_writel(env, S32_MMIO_REQ_HEAD_OFFSET, 0);
    slow32_mmio_writel(env, S32_MMIO_REQ_TAIL_OFFSET, 0);
    slow32_mmio_writel(env, S32_MMIO_RESP_HEAD_OFFSET, 0);
    slow32_mmio_writel(env, S32_MMIO_RESP_TAIL_OFFSET, 0);
}

void slow32_mmio_reset(Slow32CPU *cpu)
{
    slow32_mmio_apply_reset(cpu, false);
}

void slow32_mmio_reconfigure(Slow32CPU *cpu)
{
    slow32_mmio_apply_reset(cpu, true);
}

static void slow32_mmio_read_desc(const CPUSlow32State *env, uint32_t index,
                                  uint32_t ring_offset,
                                  Slow32MMIODesc *out)
{
    hwaddr addr = slow32_mmio_addr(env, ring_offset) +
                  (hwaddr)index * S32_MMIO_DESC_BYTES;
    uint32_t words[S32_MMIO_DESC_WORDS] = {0};

    cpu_physical_memory_read(addr, words, sizeof(words));
    out->opcode = le32_to_cpu(words[0]);
    out->length = le32_to_cpu(words[1]);
    out->offset = le32_to_cpu(words[2]);
    out->status = le32_to_cpu(words[3]);
}

static void slow32_mmio_write_desc(const CPUSlow32State *env, uint32_t index,
                                   uint32_t ring_offset,
                                   const Slow32MMIODesc *desc)
{
    hwaddr addr = slow32_mmio_addr(env, ring_offset) +
                  (hwaddr)index * S32_MMIO_DESC_BYTES;
    uint32_t words[S32_MMIO_DESC_WORDS];

    words[0] = cpu_to_le32(desc->opcode);
    words[1] = cpu_to_le32(desc->length);
    words[2] = cpu_to_le32(desc->offset);
    words[3] = cpu_to_le32(desc->status);
    cpu_physical_memory_write(addr, words, sizeof(words));
}

static void slow32_mmio_copy_from_guest(const CPUSlow32State *env,
                                        uint32_t offset, uint8_t *dst,
                                        uint32_t length)
{
    if (!length) {
        return;
    }
    uint32_t capacity = S32_MMIO_DATA_CAPACITY;
    hwaddr base = slow32_mmio_addr(env, S32_MMIO_DATA_BUFFER_OFFSET);
    uint32_t cursor = offset % capacity;
    uint32_t remaining = length;

    while (remaining) {
        uint32_t chunk = MIN(remaining, capacity - cursor);
        cpu_physical_memory_read(base + cursor, dst, chunk);
        dst += chunk;
        remaining -= chunk;
        cursor = 0;
    }
}

static void slow32_mmio_copy_to_guest(const CPUSlow32State *env,
                                      uint32_t offset, const uint8_t *src,
                                      uint32_t length)
{
    if (!length) {
        return;
    }
    uint32_t capacity = S32_MMIO_DATA_CAPACITY;
    hwaddr base = slow32_mmio_addr(env, S32_MMIO_DATA_BUFFER_OFFSET);
    uint32_t cursor = offset % capacity;
    uint32_t remaining = length;

    while (remaining) {
        uint32_t chunk = MIN(remaining, capacity - cursor);
        cpu_physical_memory_write(base + cursor, src, chunk);
        src += chunk;
        remaining -= chunk;
        cursor = 0;
    }
}

static void slow32_mmio_request_exit(Slow32CPU *cpu)
{
    CPUState *cs = CPU(cpu);

    cpu->env.halted = 1;
    slow32_cpu_complete_halt(cpu);
    cpu_exit(cs);
}

static void slow32_mmio_handle_args_info(Slow32MMIOContext *ctx,
                                         const CPUSlow32State *env,
                                         const Slow32MMIODesc *req,
                                         Slow32MMIODesc *resp)
{
    if (req->length < sizeof(s32_mmio_args_info_t)) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    s32_mmio_args_info_t info = {
        .argc = ctx->args_argc,
        .total_bytes = ctx->args_total_bytes,
        .flags = 0,
        .reserved = 0,
    };

    slow32_mmio_copy_to_guest(env, req->offset,
                              (const uint8_t *)&info, sizeof(info));
    resp->length = sizeof(info);
    resp->status = S32_MMIO_STATUS_OK;
}

static void slow32_mmio_handle_args_data(Slow32MMIOContext *ctx,
                                         const CPUSlow32State *env,
                                         const Slow32MMIODesc *req,
                                         Slow32MMIODesc *resp)
{
    if (!ctx->args_blob) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    if (req->length == 0) {
        resp->status = S32_MMIO_STATUS_OK;
        resp->length = 0;
        return;
    }

    if (req->length > S32_MMIO_DATA_CAPACITY) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    if (req->status > ctx->args_total_bytes) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    uint32_t remaining = ctx->args_total_bytes - req->status;
    uint32_t to_copy = MIN(req->length, remaining);

    if (to_copy) {
        slow32_mmio_copy_to_guest(env, req->offset,
                                  ctx->args_blob->data + req->status,
                                  to_copy);
    }

    resp->length = to_copy;
    resp->status = S32_MMIO_STATUS_OK;
}

static void slow32_mmio_handle_envp_info(Slow32MMIOContext *ctx,
                                         const CPUSlow32State *env,
                                         const Slow32MMIODesc *req,
                                         Slow32MMIODesc *resp)
{
    if (req->length < sizeof(s32_mmio_envp_info_t)) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    s32_mmio_envp_info_t info = {
        .envc = ctx->envp_envc,
        .total_bytes = ctx->envp_total_bytes,
        .flags = 0,
        .reserved = 0,
    };

    slow32_mmio_copy_to_guest(env, req->offset,
                              (const uint8_t *)&info, sizeof(info));
    resp->length = sizeof(info);
    resp->status = S32_MMIO_STATUS_OK;
}

static void slow32_mmio_handle_envp_data(Slow32MMIOContext *ctx,
                                         const CPUSlow32State *env,
                                         const Slow32MMIODesc *req,
                                         Slow32MMIODesc *resp)
{
    if (!ctx->envp_blob) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    if (req->length == 0) {
        resp->status = S32_MMIO_STATUS_OK;
        resp->length = 0;
        return;
    }

    if (req->length > S32_MMIO_DATA_CAPACITY) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    if (req->status > ctx->envp_total_bytes) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    uint32_t remaining = ctx->envp_total_bytes - req->status;
    uint32_t to_copy = MIN(req->length, remaining);

    if (to_copy) {
        slow32_mmio_copy_to_guest(env, req->offset,
                                  ctx->envp_blob->data + req->status,
                                  to_copy);
    }

    resp->length = to_copy;
    resp->status = S32_MMIO_STATUS_OK;
}

static void slow32_mmio_handle_getenv(Slow32MMIOContext *ctx,
                                      const CPUSlow32State *env,
                                      const Slow32MMIODesc *req,
                                      Slow32MMIODesc *resp)
{
    if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    /* Read the name from guest memory. Clamp the copy length so we always
     * have room for a terminator even when the guest supplies the maximum
     * request size.
     */
    uint32_t copy_len = MIN(req->length, S32_MMIO_DATA_CAPACITY - 1u);
    slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, copy_len);
    ctx->scratch[copy_len] = '\0';

    /* Look up in host environment */
    const char *value = getenv((const char *)ctx->scratch);
    if (!value) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    size_t value_len = strlen(value);
    if (value_len > S32_MMIO_DATA_CAPACITY) {
        value_len = S32_MMIO_DATA_CAPACITY;
    }

    slow32_mmio_copy_to_guest(env, req->offset,
                              (const uint8_t *)value, value_len);
    resp->length = (uint32_t)value_len;
    resp->status = (uint32_t)value_len;
}

static void slow32_mmio_handle_gettime(const CPUSlow32State *env,
                                       const Slow32MMIODesc *req,
                                       Slow32MMIODesc *resp)
{
    if (req->length < sizeof(s32_mmio_timepair64_t)) {
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        return;
    }

    struct timespec ts = {0};
    clock_gettime(CLOCK_REALTIME, &ts);
    uint64_t secs = ts.tv_sec;
    s32_mmio_timepair64_t payload = {
        .seconds_lo = (uint32_t)(secs & 0xFFFFFFFFu),
        .seconds_hi = (uint32_t)(secs >> 32),
        .nanoseconds = ts.tv_nsec,
        .reserved = 0,
    };

    slow32_mmio_copy_to_guest(env, req->offset,
                              (const uint8_t *)&payload, sizeof(payload));
    resp->length = sizeof(payload);
    resp->status = S32_MMIO_STATUS_OK;
}

static void slow32_mmio_handle_sleep(const CPUSlow32State *env,
                                     const Slow32MMIODesc *req,
                                     Slow32MMIODesc *resp)
{
    s32_mmio_timepair64_t remainder = {0};
    slow32_mmio_copy_to_guest(env, req->offset,
                              (const uint8_t *)&remainder, sizeof(remainder));
    resp->length = sizeof(remainder);
    resp->status = S32_MMIO_STATUS_EINTR;
}

static void slow32_mmio_dispatch(Slow32MMIOContext *ctx, Slow32CPU *cpu,
                                 const Slow32MMIODesc *req,
                                 Slow32MMIODesc *resp)
{
    CPUSlow32State *env = &cpu->env;

    switch (req->opcode) {
    case S32_MMIO_OP_NOP:
        resp->status = S32_MMIO_STATUS_OK;
        break;

    case S32_MMIO_OP_PUTCHAR:
        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, 1);
        putchar(ctx->scratch[0]);
        fflush(stdout);
        resp->status = S32_MMIO_STATUS_OK;
        resp->length = 1;
        break;

    case S32_MMIO_OP_GETCHAR: {
        int ch = fgetc(stdin);
        if (ch != EOF) {
            ctx->scratch[0] = (uint8_t)ch;
            slow32_mmio_copy_to_guest(env, req->offset, ctx->scratch, 1);
            resp->length = 1;
            resp->status = S32_MMIO_STATUS_OK;
        } else {
            resp->length = 0;
            resp->status = (uint32_t)EOF;
        }
        break;
    }

    case S32_MMIO_OP_WRITE: {
        int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
        uint32_t to_write = MIN(req->length, (uint32_t)S32_MMIO_DATA_CAPACITY);

        if (host_fd < 0 || to_write == 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, to_write);
        ssize_t written = write(host_fd, ctx->scratch, to_write);
        if (written < 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
        } else {
            resp->length = (uint32_t)written;
            resp->status = (uint32_t)written;
        }
        break;
    }

    case S32_MMIO_OP_READ: {
        int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
        uint32_t to_read = MIN(req->length, (uint32_t)S32_MMIO_DATA_CAPACITY);

        if (host_fd < 0 || to_read == 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        ssize_t nread = read(host_fd, ctx->scratch, to_read);
        if (nread < 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
        } else {
            if (nread > 0) {
                slow32_mmio_copy_to_guest(env, req->offset,
                                          ctx->scratch, (uint32_t)nread);
            }
            resp->length = (uint32_t)nread;
            resp->status = (uint32_t)nread;
        }
        break;
    }

    case S32_MMIO_OP_OPEN: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        bool needs_mode = false;
        int flags = slow32_mmio_translate_open_flags(req->status, &needs_mode);
        int host_fd = needs_mode ? open((char *)ctx->scratch, flags, 0644)
                                 : open((char *)ctx->scratch, flags);

        if (host_fd < 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        int guest_fd = slow32_mmio_alloc_guest_fd(ctx, host_fd, true);
        if (guest_fd < 0) {
            close(host_fd);
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        resp->status = (uint32_t)guest_fd;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_CLOSE: {
        uint32_t guest_fd = req->status;
        if (guest_fd >= S32_MMIO_MAX_FDS || ctx->host_fds[guest_fd] < 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        int host_fd = ctx->host_fds[guest_fd];
        int rc = 0;
        if (ctx->host_fd_owned[guest_fd]) {
            rc = close(host_fd);
        }

        ctx->host_fds[guest_fd] = -1;
        ctx->host_fd_owned[guest_fd] = false;

        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_SEEK: {
        int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
        if (host_fd < 0 || req->length < 8u) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, 8);
        uint8_t whence_raw = ctx->scratch[0];
        int32_t distance = 0;
        memcpy(&distance, ctx->scratch + 4, sizeof(int32_t));

        off_t new_pos = lseek(host_fd, (off_t)distance, (int)whence_raw);
        if (new_pos == (off_t)-1) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
        } else {
            resp->status = (uint32_t)new_pos;
            resp->length = 0;
        }
        break;
    }

    case S32_MMIO_OP_STAT: {
        struct stat host_stat;
        int rc = -1;

        if (req->status == S32_MMIO_STAT_PATH_SENTINEL) {
            /* stat by path */
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp->status = S32_MMIO_STATUS_ERR;
                resp->length = 0;
                break;
            }
            slow32_mmio_copy_from_guest(env, req->offset,
                                        ctx->scratch, req->length);
            ctx->scratch[req->length - 1] = '\0';
            rc = stat((char *)ctx->scratch, &host_stat);
        } else {
            /* fstat by guest fd */
            int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
            if (host_fd < 0) {
                resp->status = S32_MMIO_STATUS_ERR;
                resp->length = 0;
                break;
            }
            rc = fstat(host_fd, &host_stat);
        }

        if (rc != 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        s32_mmio_stat_result_t result = {
            .st_dev = (uint64_t)host_stat.st_dev,
            .st_ino = (uint64_t)host_stat.st_ino,
            .st_mode = (uint32_t)host_stat.st_mode,
            .st_nlink = (uint32_t)host_stat.st_nlink,
            .st_uid = (uint32_t)host_stat.st_uid,
            .st_gid = (uint32_t)host_stat.st_gid,
            .st_rdev = (uint64_t)host_stat.st_rdev,
            .st_size = (uint64_t)host_stat.st_size,
            .st_blksize = (uint64_t)host_stat.st_blksize,
            .st_blocks = (uint64_t)host_stat.st_blocks,
#if defined(__APPLE__)
            .st_atime_sec = (uint64_t)host_stat.st_atimespec.tv_sec,
            .st_atime_nsec = (uint32_t)host_stat.st_atimespec.tv_nsec,
            .st_mtime_sec = (uint64_t)host_stat.st_mtimespec.tv_sec,
            .st_mtime_nsec = (uint32_t)host_stat.st_mtimespec.tv_nsec,
            .st_ctime_sec = (uint64_t)host_stat.st_ctimespec.tv_sec,
            .st_ctime_nsec = (uint32_t)host_stat.st_ctimespec.tv_nsec,
#else
            .st_atime_sec = (uint64_t)host_stat.st_atim.tv_sec,
            .st_atime_nsec = (uint32_t)host_stat.st_atim.tv_nsec,
            .st_mtime_sec = (uint64_t)host_stat.st_mtim.tv_sec,
            .st_mtime_nsec = (uint32_t)host_stat.st_mtim.tv_nsec,
            .st_ctime_sec = (uint64_t)host_stat.st_ctim.tv_sec,
            .st_ctime_nsec = (uint32_t)host_stat.st_ctim.tv_nsec,
#endif
        };

        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)&result, sizeof(result));
        resp->length = sizeof(result);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_FLUSH:
        fflush(stdout);
        fflush(stderr);
        resp->status = S32_MMIO_STATUS_OK;
        resp->length = 0;
        break;

    case S32_MMIO_OP_READ_DIRECT: {
        /*
         * Direct read into guest memory (zero-copy path).
         * Request: status=guest_fd, offset=guest_addr, length=count
         * Response: status=bytes_read or ERR
         */
        if (req->length == 0) {
            resp->length = 0;
            resp->status = 0;
            break;
        }

        int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
        if (host_fd < 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        uint32_t guest_addr = req->offset;
        uint32_t count = req->length;

        /* Validate guest address range */
        if (env->mem_size == 0 ||
            guest_addr >= env->mem_size ||
            (uint64_t)guest_addr + count > env->mem_size) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        /*
         * Read in chunks through the scratch buffer, writing each
         * chunk directly into guest physical memory.
         */
        uint32_t total_read = 0;
        bool read_error = false;

        while (total_read < count) {
            uint32_t chunk = MIN(count - total_read,
                                 (uint32_t)S32_MMIO_DATA_CAPACITY);
            ssize_t nread = read(host_fd, ctx->scratch, chunk);
            if (nread < 0) {
                read_error = true;
                break;
            }
            if (nread == 0) {
                break; /* EOF */
            }
            cpu_physical_memory_write((hwaddr)(guest_addr + total_read),
                                      ctx->scratch, (uint32_t)nread);
            total_read += (uint32_t)nread;
            if ((uint32_t)nread < chunk) {
                break; /* Short read */
            }
        }

        if (read_error && total_read == 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
        } else {
            resp->status = total_read;
            resp->length = total_read;
        }
        break;
    }

    case S32_MMIO_OP_FTRUNCATE: {
        int host_fd = slow32_mmio_host_fd_for_guest(ctx, req->status);
        if (host_fd < 0 || req->length < 4u) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, 4);
        uint32_t new_length = 0;
        memcpy(&new_length, ctx->scratch, sizeof(uint32_t));

        int rc = ftruncate(host_fd, (off_t)new_length);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_ARGS_INFO:
        slow32_mmio_handle_args_info(ctx, env, req, resp);
        break;

    case S32_MMIO_OP_ARGS_DATA:
        slow32_mmio_handle_args_data(ctx, env, req, resp);
        break;

    case S32_MMIO_OP_EXIT:
        resp->status = req->status;
        slow32_mmio_request_exit(cpu);
        break;

    case S32_MMIO_OP_GETTIME:
        slow32_mmio_handle_gettime(env, req, resp);
        break;

    case S32_MMIO_OP_SLEEP:
        slow32_mmio_handle_sleep(env, req, resp);
        break;

    case S32_MMIO_OP_ENVP_INFO:
        slow32_mmio_handle_envp_info(ctx, env, req, resp);
        break;

    case S32_MMIO_OP_ENVP_DATA:
        slow32_mmio_handle_envp_data(ctx, env, req, resp);
        break;

    case S32_MMIO_OP_GETENV:
        slow32_mmio_handle_getenv(ctx, env, req, resp);
        break;

    /* ========== Filesystem metadata operations (0x20-0x2A) ========== */

    case S32_MMIO_OP_UNLINK: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        int rc = unlink((char *)ctx->scratch);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_RENAME: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        uint32_t old_len = req->status;
        if (old_len == 0 || old_len >= req->length) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[old_len] = '\0';
        ctx->scratch[req->length - 1] = '\0';

        const char *oldpath = (char *)ctx->scratch;
        const char *newpath = (char *)ctx->scratch + old_len;

        int rc = rename(oldpath, newpath);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_MKDIR: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        mode_t mode = (mode_t)req->status;
        if (mode == 0) {
            mode = 0755;
        }

        int rc = mkdir((char *)ctx->scratch, mode);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_RMDIR: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        int rc = rmdir((char *)ctx->scratch);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_LSTAT: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        struct stat host_stat;
        memset(&host_stat, 0, sizeof(host_stat));
        int rc = lstat((char *)ctx->scratch, &host_stat);

        if (rc != 0) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        s32_mmio_stat_result_t result = {
            .st_dev = (uint64_t)host_stat.st_dev,
            .st_ino = (uint64_t)host_stat.st_ino,
            .st_mode = (uint32_t)host_stat.st_mode,
            .st_nlink = (uint32_t)host_stat.st_nlink,
            .st_uid = (uint32_t)host_stat.st_uid,
            .st_gid = (uint32_t)host_stat.st_gid,
            .st_rdev = (uint64_t)host_stat.st_rdev,
            .st_size = (uint64_t)host_stat.st_size,
            .st_blksize = (uint64_t)host_stat.st_blksize,
            .st_blocks = (uint64_t)host_stat.st_blocks,
#if defined(__APPLE__)
            .st_atime_sec = (uint64_t)host_stat.st_atimespec.tv_sec,
            .st_atime_nsec = (uint32_t)host_stat.st_atimespec.tv_nsec,
            .st_mtime_sec = (uint64_t)host_stat.st_mtimespec.tv_sec,
            .st_mtime_nsec = (uint32_t)host_stat.st_mtimespec.tv_nsec,
            .st_ctime_sec = (uint64_t)host_stat.st_ctimespec.tv_sec,
            .st_ctime_nsec = (uint32_t)host_stat.st_ctimespec.tv_nsec,
#else
            .st_atime_sec = (uint64_t)host_stat.st_atim.tv_sec,
            .st_atime_nsec = (uint32_t)host_stat.st_atim.tv_nsec,
            .st_mtime_sec = (uint64_t)host_stat.st_mtim.tv_sec,
            .st_mtime_nsec = (uint32_t)host_stat.st_mtim.tv_nsec,
            .st_ctime_sec = (uint64_t)host_stat.st_ctim.tv_sec,
            .st_ctime_nsec = (uint32_t)host_stat.st_ctim.tv_nsec,
#endif
        };

        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)&result, sizeof(result));
        resp->length = sizeof(result);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_ACCESS: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        int mode = (int)req->status;
        int rc = access((char *)ctx->scratch, mode);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_CHDIR: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        int rc = chdir((char *)ctx->scratch);
        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_GETCWD: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        uint32_t max_len = MIN(req->length, S32_MMIO_DATA_CAPACITY);
        char *cwd = getcwd((char *)ctx->scratch, max_len);
        if (!cwd) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        size_t len = strlen(cwd) + 1;
        slow32_mmio_copy_to_guest(env, req->offset, ctx->scratch, len);
        resp->length = (uint32_t)len;
        resp->status = (uint32_t)len;
        break;
    }

    case S32_MMIO_OP_OPENDIR: {
        if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';

        DIR *host_dir = opendir((char *)ctx->scratch);
        if (!host_dir) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        int guest_fd = slow32_mmio_alloc_guest_dir_fd(ctx, host_dir);
        if (guest_fd < 0) {
            closedir(host_dir);
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        resp->status = (uint32_t)guest_fd;
        resp->length = 0;
        break;
    }

    case S32_MMIO_OP_READDIR: {
        uint32_t guest_fd = req->status;
        DIR *host_dir = slow32_mmio_host_dir_for_guest(ctx, guest_fd);

        if (!host_dir) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        errno = 0;
        struct dirent *entry = readdir(host_dir);
        if (!entry) {
            if (errno == 0) {
                resp->status = S32_MMIO_STATUS_EOF;
                resp->length = 0;
            } else {
                resp->status = S32_MMIO_STATUS_ERR;
                resp->length = 0;
            }
            break;
        }

        s32_mmio_dirent_t result;
        memset(&result, 0, sizeof(result));
        result.d_ino = (uint64_t)entry->d_ino;
        result.d_type = (uint32_t)entry->d_type;
        size_t namelen = strlen(entry->d_name);
        if (namelen > 255) {
            namelen = 255;
        }
        result.d_namlen = (uint32_t)namelen;
        memcpy(result.d_name, entry->d_name, namelen);
        result.d_name[namelen] = '\0';

        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)&result, sizeof(result));
        resp->length = sizeof(s32_mmio_dirent_t);
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_CLOSEDIR: {
        uint32_t guest_fd = req->status;

        if (guest_fd >= S32_MMIO_MAX_FDS) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        if (ctx->fd_types[guest_fd] != S32_FD_TYPE_DIR || !ctx->host_dirs[guest_fd]) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
            break;
        }

        int rc = closedir(ctx->host_dirs[guest_fd]);
        ctx->host_dirs[guest_fd] = NULL;
        ctx->host_fds[guest_fd] = -1;
        ctx->host_fd_owned[guest_fd] = false;
        ctx->fd_types[guest_fd] = S32_FD_TYPE_FILE;

        resp->status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }

    /* ========== Service negotiation opcodes (0xF0-0xF4) ========== */

    case S32_MMIO_OP_SVC_REQUEST: {
        if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';
        const char *svc_name = (const char *)ctx->scratch;

        /* Policy check */
        if (!slow32_mmio_policy_allows(ctx, svc_name)) {
            uint32_t svc_result = S32_SVC_DENIED;
            slow32_mmio_copy_to_guest(env, req->offset,
                                      (const uint8_t *)&svc_result, 4);
            resp->length = 4;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }

        /* Check if already active */
        for (int i = 0; i < ctx->num_services; i++) {
            if (ctx->services[i].active &&
                strcmp(ctx->services[i].name, svc_name) == 0) {
                uint32_t svc_result = S32_SVC_CONFLICT;
                slow32_mmio_copy_to_guest(env, req->offset,
                                          (const uint8_t *)&svc_result, 4);
                resp->length = 4;
                resp->status = S32_MMIO_STATUS_OK;
                goto done;
            }
        }

        /* Find builtin service */
        const Slow32BuiltinService *builtin =
            slow32_find_builtin_service(svc_name);
        if (!builtin) {
            uint32_t svc_result = S32_SVC_UNKNOWN;
            slow32_mmio_copy_to_guest(env, req->offset,
                                      (const uint8_t *)&svc_result, 4);
            resp->length = 4;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }

        /* Check session limit */
        if (ctx->num_services >= S32_MAX_SERVICES) {
            uint32_t svc_result = S32_SVC_LIMIT;
            slow32_mmio_copy_to_guest(env, req->offset,
                                      (const uint8_t *)&svc_result, 4);
            resp->length = 4;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }

        /* Allocate opcode range */
        uint32_t base = ctx->next_dynamic_opcode;
        if (base + builtin->opcode_count > 0xF0) {
            uint32_t svc_result = S32_SVC_LIMIT;
            slow32_mmio_copy_to_guest(env, req->offset,
                                      (const uint8_t *)&svc_result, 4);
            resp->length = 4;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }

        /* Create service state */
        void *svc_state = builtin->create ? builtin->create() : NULL;

        /* Register session */
        Slow32SvcSession *session = &ctx->services[ctx->num_services++];
        session->active = true;
        pstrcpy(session->name, S32_MAX_SVC_NAME, svc_name);
        session->base_opcode = base;
        session->opcode_count = builtin->opcode_count;
        session->version = builtin->version;
        session->state = svc_state;
        session->cleanup = builtin->cleanup;
        session->handle = builtin->handle;

        ctx->next_dynamic_opcode = base + builtin->opcode_count;

        /* Write response: [0]=OK, [4]=base, [8]=count, [12]=version */
        uint32_t reply[4];
        reply[0] = S32_SVC_OK;
        reply[1] = base;
        reply[2] = builtin->opcode_count;
        reply[3] = builtin->version;
        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)reply, sizeof(reply));
        resp->length = 16;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_SVC_RELEASE: {
        if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';
        const char *svc_name = (const char *)ctx->scratch;

        bool found = false;
        for (int i = 0; i < ctx->num_services; i++) {
            Slow32SvcSession *svc = &ctx->services[i];
            if (svc->active && strcmp(svc->name, svc_name) == 0) {
                if (svc->cleanup && svc->state) {
                    svc->cleanup(svc->state);
                }
                svc->active = false;
                svc->state = NULL;
                found = true;
                break;
            }
        }
        resp->status = found ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
        break;
    }

    case S32_MMIO_OP_SVC_QUERY: {
        if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
            resp->status = S32_MMIO_STATUS_ERR;
            break;
        }

        slow32_mmio_copy_from_guest(env, req->offset, ctx->scratch, req->length);
        ctx->scratch[req->length - 1] = '\0';
        const char *svc_name = (const char *)ctx->scratch;

        uint32_t svc_result;
        const Slow32BuiltinService *builtin =
            slow32_find_builtin_service(svc_name);
        if (!builtin) {
            svc_result = S32_SVC_UNKNOWN;
        } else if (!slow32_mmio_policy_allows(ctx, svc_name)) {
            svc_result = S32_SVC_DENIED;
        } else {
            svc_result = S32_SVC_OK;
        }
        slow32_mmio_copy_to_guest(env, req->offset,
                                  (const uint8_t *)&svc_result, 4);
        resp->length = 4;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_SVC_LIST: {
        /* NUL-separated list of available service names */
        uint32_t pos = 0;
        for (size_t i = 0; i < NUM_BUILTIN_SERVICES; i++) {
            size_t len = strlen(builtin_services[i].name) + 1;
            if (pos + len > S32_MMIO_DATA_CAPACITY) {
                break;
            }
            memcpy(ctx->scratch + pos, builtin_services[i].name, len);
            pos += len;
        }
        if (pos > 0) {
            slow32_mmio_copy_to_guest(env, req->offset, ctx->scratch, pos);
        }
        resp->length = pos;
        resp->status = S32_MMIO_STATUS_OK;
        break;
    }

    case S32_MMIO_OP_SVC_VERSION:
        resp->status = S32_SVC_PROTOCOL_VERSION;
        break;

    default: {
        /* Check if opcode falls in a registered service range */
        bool handled = false;
        for (int i = 0; i < ctx->num_services; i++) {
            Slow32SvcSession *svc = &ctx->services[i];
            if (svc->active &&
                req->opcode >= svc->base_opcode &&
                req->opcode < svc->base_opcode + svc->opcode_count) {
                uint32_t sub = req->opcode - svc->base_opcode;
                svc->handle(svc->state, ctx, env, sub, req, resp);
                handled = true;
                break;
            }
        }
        if (!handled) {
            resp->status = S32_MMIO_STATUS_ERR;
            resp->length = 0;
        }
        break;
    }
    }

done:
    ; /* label at end of function for early-exit from nested loops */
}

static bool slow32_mmio_write_response(Slow32MMIOContext *ctx,
                                       CPUSlow32State *env,
                                       const Slow32MMIODesc *resp)
{
    uint32_t resp_tail = slow32_mmio_readl(env, S32_MMIO_RESP_TAIL_OFFSET);
    uint32_t next = slow32_mmio_ring_next(ctx->resp_head);

    if (next == resp_tail) {
        qemu_log_mask(LOG_GUEST_ERROR,
                      "slow32-mmio: response ring full, dropping opcode 0x%x\n",
                      resp->opcode);
        return false;
    }

    slow32_mmio_write_desc(env, ctx->resp_head, S32_MMIO_RESP_RING_OFFSET,
                           resp);
    ctx->resp_head = next;
    slow32_mmio_writel(env, S32_MMIO_RESP_HEAD_OFFSET, ctx->resp_head);
    return true;
}

void slow32_mmio_process(Slow32CPU *cpu)
{
    Slow32MMIOContext *ctx = cpu->mmio;
    CPUSlow32State *env = &cpu->env;

    if (!ctx || !ctx->enabled) {
        return;
    }

    uint32_t req_head = slow32_mmio_readl(env, S32_MMIO_REQ_HEAD_OFFSET);
    uint32_t req_tail = ctx->req_tail;

    while (req_tail != req_head) {
        Slow32MMIODesc req = {};
        Slow32MMIODesc resp = {};

        slow32_mmio_read_desc(env, req_tail, S32_MMIO_REQ_RING_OFFSET, &req);
        resp.opcode = req.opcode;
        resp.offset = req.offset;

        slow32_mmio_dispatch(ctx, cpu, &req, &resp);

        if (!slow32_mmio_write_response(ctx, env, &resp)) {
            break;
        }

        req_tail = slow32_mmio_ring_next(req_tail);
        ctx->req_tail = req_tail;
        slow32_mmio_writel(env, S32_MMIO_REQ_TAIL_OFFSET, req_tail);
        req_head = slow32_mmio_readl(env, S32_MMIO_REQ_HEAD_OFFSET);
    }
}
