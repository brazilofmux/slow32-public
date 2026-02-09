#include "qemu/osdep.h"

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>
#include <stdio.h>
#include <sys/stat.h>
#include <time.h>
#include <unistd.h>

#include "exec/cpu-common.h"
#include "hw/core/cpu.h"
#include "qemu/bswap.h"
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
}

void slow32_mmio_context_destroy(Slow32CPU *cpu)
{
    if (!cpu->mmio) {
        return;
    }
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

static uint32_t slow32_mmio_initial_brk(const CPUSlow32State *env)
{
    if (env->heap_base) {
        return env->heap_base;
    }
    uint32_t limit = env->data_limit ? env->data_limit : env->stack_top;
    return QEMU_ALIGN_UP(limit, S32_MMIO_PAGE_SIZE);
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

    default:
        resp->status = S32_MMIO_STATUS_ERR;
        resp->length = 0;
        break;
    }
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
