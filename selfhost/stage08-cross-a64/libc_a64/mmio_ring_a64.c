/* mmio_ring_a64.c — minimal AArch64 stub of the SLOW-32 MMIO ring buffer.
 *
 * The full implementation in tools/emulator/mmio_ring.c is ~2400 lines and
 * pulls in errno/limits/fcntl/sys/stat/sys/ioctl/termios/poll/sys/mman.
 * That's more than we want to port for initial bring-up.  This file gives
 * tools/dbt enough to link AND exercise the common path:
 *
 *   - PUTCHAR / WRITE / FLUSH      → host stdout/stderr/fd
 *   - GETCHAR / READ               → host fd
 *   - OPEN / CLOSE                 → openat/close (paths only — no flags translation)
 *   - SEEK                         → lseek
 *   - STAT                         → newfstatat / fstat (path-stat or fd-stat)
 *   - EXIT                         → halts the guest
 *   - ARGS_INFO/DATA, ENVP_INFO/DATA, GETENV
 *
 * Everything else (dirent, time, sockets, services) responds with
 * S32_MMIO_STATUS_ERR.  The struct layout matches mmio_ring.h exactly so
 * dbt's direct field access (req_ring/resp_ring/data_buffer/base_addr)
 * keeps working.
 */

#include <stdint.h>
#include <stdbool.h>

#include "../../../common/mmio_ring_layout.h"

/* ---- Mirror of the public types from tools/emulator/mmio_ring.h ----
 * Must stay byte-for-byte compatible since dbt allocates these. */

#define S32_MMIO_MAX_FDS 128
#define S32_MAX_SERVICES 16
#define S32_MAX_SVC_NAME 32

typedef enum { S32_FD_TYPE_FILE = 0, S32_FD_TYPE_DIR = 1 } s32_fd_type_t;

typedef struct {
    uint32_t opcode;
    uint32_t length;
    uint32_t offset;
    uint32_t status;
} io_descriptor_t;

typedef struct __dir_stream DIR;
struct mmio_ring_state;
typedef struct mmio_ring_state mmio_ring_state_t;

typedef struct {
    bool active;
    char name[S32_MAX_SVC_NAME];
    uint32_t base_opcode;
    uint32_t opcode_count;
    uint32_t version;
    void *state;
    void (*cleanup)(void *state);
    void (*handle)(void *state, mmio_ring_state_t *mmio,
                   uint32_t sub_opcode, io_descriptor_t *req,
                   io_descriptor_t *resp);
} svc_session_t;

typedef struct {
    bool default_allow;
    char allow_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int allow_count;
    char deny_list[S32_MAX_SERVICES][S32_MAX_SVC_NAME];
    int deny_count;
} svc_policy_t;

struct mmio_ring_state {
    uint32_t req_head;
    uint32_t req_tail;
    uint32_t resp_head;
    uint32_t resp_tail;
    uint32_t base_addr;
    void *guest_mem_base;
    uint32_t guest_mem_size;
    io_descriptor_t *req_ring;
    io_descriptor_t *resp_ring;
    uint8_t *data_buffer;
    uint64_t total_requests;
    uint64_t total_responses;
    uint32_t args_argc;
    uint32_t args_total_bytes;
    uint8_t *args_blob;
    uint32_t envp_envc;
    uint32_t envp_total_bytes;
    uint8_t *envp_blob;
    int host_fds[S32_MMIO_MAX_FDS];
    bool host_fd_owned[S32_MMIO_MAX_FDS];
    s32_fd_type_t fd_types[S32_MMIO_MAX_FDS];
    DIR *host_dirs[S32_MMIO_MAX_FDS];
    svc_session_t services[S32_MAX_SERVICES];
    int num_services;
    svc_policy_t policy;
    uint32_t next_dynamic_opcode;
};

typedef struct {
    bool *halted;
    uint32_t *exit_status;
} mmio_cpu_iface_t;

/* ---- libc/syscall imports ---- */

int   write(int fd, char *buf, int len);
int   read(int fd, char *buf, int len);
int   open(char *path, int flags, int mode);
int   close(int fd);
int   strlen(char *s);
int   strcmp(char *a, char *b);
int   strncmp(char *a, char *b, int n);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
char *malloc(int size);
void  free(char *p);

#define AT_FDCWD (-100)

/* ============================================================================
 * Initialization
 * ============================================================================ */

void mmio_ring_init(mmio_ring_state_t *mmio) {
    int i;
    memset((char *)mmio, 0, sizeof(*mmio));
    /* fd 0/1/2 reserved for stdin/stdout/stderr — slot 0..2 map identity. */
    for (i = 0; i < S32_MMIO_MAX_FDS; i = i + 1) {
        mmio->host_fds[i] = -1;
        mmio->host_fd_owned[i] = false;
        mmio->fd_types[i] = S32_FD_TYPE_FILE;
        mmio->host_dirs[i] = 0;
    }
    mmio->host_fds[0] = 0;
    mmio->host_fds[1] = 1;
    mmio->host_fds[2] = 2;
    mmio->next_dynamic_opcode = 0x100;
    mmio->policy.default_allow = true;
}

/* ============================================================================
 * fd table helpers
 * ============================================================================ */

static int alloc_guest_fd(mmio_ring_state_t *mmio, int host_fd) {
    int i;
    for (i = 3; i < S32_MMIO_MAX_FDS; i = i + 1) {
        if (mmio->host_fds[i] < 0) {
            mmio->host_fds[i] = host_fd;
            mmio->host_fd_owned[i] = true;
            mmio->fd_types[i] = S32_FD_TYPE_FILE;
            return i;
        }
    }
    return -1;
}

static int host_fd_for(mmio_ring_state_t *mmio, uint32_t guest_fd) {
    if (guest_fd >= S32_MMIO_MAX_FDS) return -1;
    return mmio->host_fds[guest_fd];
}

/* ============================================================================
 * Args / envp blob construction
 * ============================================================================ */

static void free_blob(uint8_t **blob_p) {
    if (*blob_p) { free((char *)*blob_p); *blob_p = 0; }
}

int mmio_ring_set_args(mmio_ring_state_t *mmio, uint32_t argc, char **argv) {
    uint32_t total;
    uint32_t i;
    uint8_t *p;
    int slen;

    free_blob(&mmio->args_blob);
    mmio->args_argc = 0;
    mmio->args_total_bytes = 0;
    if (argc == 0 || argv == 0) return 0;

    total = 0;
    for (i = 0; i < argc; i = i + 1) {
        if (argv[i] == 0) break;
        total = total + (uint32_t)strlen(argv[i]) + 1u;
    }
    if (total == 0) return 0;
    if (total > S32_MMIO_ARGS_MAX_BYTES) return -1;

    p = (uint8_t *)malloc((int)total);
    if (!p) return -1;
    mmio->args_blob = p;

    for (i = 0; i < argc; i = i + 1) {
        if (argv[i] == 0) break;
        slen = strlen(argv[i]) + 1;
        memcpy((char *)p, argv[i], slen);
        p = p + slen;
    }
    mmio->args_argc = argc;
    mmio->args_total_bytes = total;
    return 0;
}

void mmio_ring_clear_args(mmio_ring_state_t *mmio) {
    free_blob(&mmio->args_blob);
    mmio->args_argc = 0;
    mmio->args_total_bytes = 0;
}

int mmio_ring_set_envp(mmio_ring_state_t *mmio, char **envp) {
    uint32_t total;
    uint32_t count;
    uint32_t i;
    uint8_t *p;
    int slen;

    free_blob(&mmio->envp_blob);
    mmio->envp_envc = 0;
    mmio->envp_total_bytes = 0;
    if (envp == 0) return 0;

    count = 0;
    total = 0;
    while (envp[count]) {
        total = total + (uint32_t)strlen(envp[count]) + 1u;
        count = count + 1;
    }
    if (total == 0) return 0;
    if (total > S32_MMIO_ENVP_MAX_BYTES) return -1;

    p = (uint8_t *)malloc((int)total);
    if (!p) return -1;
    mmio->envp_blob = p;

    for (i = 0; i < count; i = i + 1) {
        slen = strlen(envp[i]) + 1;
        memcpy((char *)p, envp[i], slen);
        p = p + slen;
    }
    mmio->envp_envc = count;
    mmio->envp_total_bytes = total;
    return 0;
}

void mmio_ring_clear_envp(mmio_ring_state_t *mmio) {
    free_blob(&mmio->envp_blob);
    mmio->envp_envc = 0;
    mmio->envp_total_bytes = 0;
}

/* ============================================================================
 * Policy
 * ============================================================================ */

void mmio_set_policy(mmio_ring_state_t *mmio, svc_policy_t *policy) {
    if (policy) mmio->policy = *policy;
}

/* ============================================================================
 * Request dispatch
 * ============================================================================ */

static void emit_response(mmio_ring_state_t *mmio, io_descriptor_t *resp) {
    uint32_t next;
    next = (mmio->resp_head + 1u) % S32_MMIO_RING_ENTRIES;
    if (next == mmio->resp_tail) return;  /* response ring full — drop */
    mmio->resp_ring[mmio->resp_head] = *resp;
    mmio->resp_head = next;
    mmio->total_responses = mmio->total_responses + 1;
}

static void process_request(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu,
                            io_descriptor_t *req) {
    io_descriptor_t resp;
    uint32_t off;
    uint32_t len;
    int host_fd;
    int guest_fd;
    int rc;
    uint32_t i;
    uint8_t *p;
    uint8_t *src;
    uint8_t *dst;
    int sl;
    char namebuf[256];
    char *envp_p;
    uint32_t consumed;
    /* AArch64 Linux struct stat is the asm-generic 128-byte layout. */
    char kbuf[128];
    int rc_stat;
    s32_mmio_stat_result_t stat_result;

    resp.opcode = req->opcode;
    resp.length = 0;
    resp.offset = req->offset;
    resp.status = S32_MMIO_STATUS_ERR;

    off = req->offset % S32_MMIO_DATA_CAPACITY;
    len = req->length;

    if (req->opcode == S32_MMIO_OP_NOP) {
        resp.status = S32_MMIO_STATUS_OK;
    } else if (req->opcode == S32_MMIO_OP_PUTCHAR) {
        write(1, (char *)(mmio->data_buffer + off), 1);
        resp.status = S32_MMIO_STATUS_OK;
    } else if (req->opcode == S32_MMIO_OP_WRITE) {
        host_fd = host_fd_for(mmio, req->status);
        if (host_fd >= 0 && len > 0 && len <= S32_MMIO_DATA_CAPACITY) {
            if (off + len > S32_MMIO_DATA_CAPACITY) len = S32_MMIO_DATA_CAPACITY - off;
            rc = write(host_fd, (char *)(mmio->data_buffer + off), (int)len);
            if (rc >= 0) {
                resp.length = (uint32_t)rc;
                resp.status = (uint32_t)rc;
            }
        }
    } else if (req->opcode == S32_MMIO_OP_READ) {
        host_fd = host_fd_for(mmio, req->status);
        if (host_fd >= 0 && len > 0 && len <= S32_MMIO_DATA_CAPACITY) {
            if (off + len > S32_MMIO_DATA_CAPACITY) len = S32_MMIO_DATA_CAPACITY - off;
            rc = read(host_fd, (char *)(mmio->data_buffer + off), (int)len);
            if (rc >= 0) {
                resp.length = (uint32_t)rc;
                resp.status = (uint32_t)rc;
            }
        }
    } else if (req->opcode == S32_MMIO_OP_GETCHAR) {
        rc = read(0, (char *)(mmio->data_buffer + off), 1);
        if (rc == 1) { resp.length = 1; resp.status = S32_MMIO_STATUS_OK; }
        else if (rc == 0) resp.status = S32_MMIO_STATUS_EOF;
    } else if (req->opcode == S32_MMIO_OP_OPEN) {
        /* Path is a NUL-terminated string at data_buffer+off; req->status is
         * SLOW-32 open flags (0x01 read, 0x02 write, 0x04 append, 0x08 create,
         * 0x10 trunc) — not host flags. Translate to Linux open flags for
         * openat(AT_FDCWD, ...). Mode hardcoded to 0644. */
        if (len <= 256) {
            int s32_flags = (int)req->status;
            int linux_flags = 0;
            if ((s32_flags & 0x02) != 0) {
                if ((s32_flags & 0x01) != 0) {
                    linux_flags = 2;  /* O_RDWR */
                } else {
                    linux_flags = 1;  /* O_WRONLY */
                }
            } /* else O_RDONLY == 0 */
            if ((s32_flags & 0x04) != 0) linux_flags = linux_flags | 0x400;  /* O_APPEND */
            if ((s32_flags & 0x08) != 0) linux_flags = linux_flags | 0x40;   /* O_CREAT */
            if ((s32_flags & 0x10) != 0) linux_flags = linux_flags | 0x200;  /* O_TRUNC */
            for (i = 0; i < len && i < 255; i = i + 1) {
                namebuf[i] = (char)mmio->data_buffer[(off + i) % S32_MMIO_DATA_CAPACITY];
            }
            namebuf[i] = 0;
            host_fd = open(namebuf, linux_flags, 0644);
            if (host_fd >= 0) {
                guest_fd = alloc_guest_fd(mmio, host_fd);
                if (guest_fd >= 0) {
                    resp.length = (uint32_t)guest_fd;
                    resp.status = (uint32_t)guest_fd;
                } else {
                    close(host_fd);
                }
            }
        }
    } else if (req->opcode == S32_MMIO_OP_CLOSE) {
        guest_fd = (int)req->status;
        if (guest_fd >= 3 && guest_fd < S32_MMIO_MAX_FDS) {
            host_fd = mmio->host_fds[guest_fd];
            if (host_fd >= 0 && mmio->host_fd_owned[guest_fd]) {
                close(host_fd);
                mmio->host_fds[guest_fd] = -1;
                mmio->host_fd_owned[guest_fd] = false;
            }
            resp.status = S32_MMIO_STATUS_OK;
        } else if (guest_fd >= 0 && guest_fd < 3) {
            /* don't actually close stdin/stdout/stderr */
            resp.status = S32_MMIO_STATUS_OK;
        }
    } else if (req->opcode == S32_MMIO_OP_SEEK) {
        /* req->status is guest fd. data_buffer at off carries:
         *   byte 0: whence (0=SET, 1=CUR, 2=END — matches Linux constants)
         *   bytes 4..7: int32 distance
         * Response status: new file position (or ERR on failure). */
        if (len >= 8 && off + 8 <= S32_MMIO_DATA_CAPACITY) {
            host_fd = host_fd_for(mmio, req->status);
            if (host_fd >= 0) {
                int whence = (int)mmio->data_buffer[off];
                int distance = 0;
                memcpy((char *)&distance, (char *)(mmio->data_buffer + off + 4), 4);
                int new_pos = lseek(host_fd, distance, whence);
                if (new_pos >= 0) {
                    resp.status = (uint32_t)new_pos;
                }
            }
        }
    } else if (req->opcode == S32_MMIO_OP_STAT) {
        /* req->status == 0xFFFFFFFF (S32_MMIO_STAT_PATH_SENTINEL) means
         * path-stat: NUL-terminated path is in data_buffer+off, length len.
         * Otherwise req->status is a guest fd to fstat. */
        rc_stat = -1;
        if (req->status == S32_MMIO_STAT_PATH_SENTINEL) {
            if (len > 0 && len <= 256) {
                for (i = 0; i < len && i < 255; i = i + 1) {
                    namebuf[i] = (char)mmio->data_buffer[(off + i) % S32_MMIO_DATA_CAPACITY];
                }
                namebuf[i] = 0;
                rc_stat = stat(namebuf, kbuf);
            }
        } else {
            host_fd = host_fd_for(mmio, req->status);
            if (host_fd >= 0) {
                rc_stat = fstat(host_fd, kbuf);
            }
        }
        if (rc_stat == 0 && off + sizeof(stat_result) <= S32_MMIO_DATA_CAPACITY) {
            /* Repack AArch64 asm-generic struct stat into s32_mmio_stat_result_t.
             * Field offsets (kernel struct stat, 128 bytes):
             *   0   st_dev (u64)        8   st_ino (u64)
             *  16   st_mode (u32)      20   st_nlink (u32)
             *  24   st_uid (u32)       28   st_gid (u32)
             *  32   st_rdev (u64)      40   __pad1 (u64)
             *  48   st_size (s64)      56   st_blksize (u32)  60   __pad2
             *  64   st_blocks (s64)
             *  72   st_atime_sec (s64) 80   st_atime_nsec (u64)
             *  88   st_mtime_sec (s64) 96   st_mtime_nsec (u64)
             * 104   st_ctime_sec (s64) 112  st_ctime_nsec (u64)
             */
            memset((char *)&stat_result, 0, sizeof(stat_result));
            memcpy((char *)&stat_result.st_dev,        kbuf + 0,   8);
            memcpy((char *)&stat_result.st_ino,        kbuf + 8,   8);
            memcpy((char *)&stat_result.st_mode,       kbuf + 16,  4);
            memcpy((char *)&stat_result.st_nlink,      kbuf + 20,  4);
            memcpy((char *)&stat_result.st_uid,        kbuf + 24,  4);
            memcpy((char *)&stat_result.st_gid,        kbuf + 28,  4);
            memcpy((char *)&stat_result.st_rdev,       kbuf + 32,  8);
            memcpy((char *)&stat_result.st_size,       kbuf + 48,  8);
            memcpy((char *)&stat_result.st_blksize,    kbuf + 56,  4);
            memcpy((char *)&stat_result.st_blocks,     kbuf + 64,  8);
            memcpy((char *)&stat_result.st_atime_sec,  kbuf + 72,  8);
            memcpy((char *)&stat_result.st_atime_nsec, kbuf + 80,  4);
            memcpy((char *)&stat_result.st_mtime_sec,  kbuf + 88,  8);
            memcpy((char *)&stat_result.st_mtime_nsec, kbuf + 96,  4);
            memcpy((char *)&stat_result.st_ctime_sec,  kbuf + 104, 8);
            memcpy((char *)&stat_result.st_ctime_nsec, kbuf + 112, 4);
            memcpy((char *)(mmio->data_buffer + off), (char *)&stat_result,
                   sizeof(stat_result));
            resp.length = sizeof(stat_result);
            resp.status = S32_MMIO_STATUS_OK;
        }
    } else if (req->opcode == S32_MMIO_OP_FLUSH) {
        resp.status = S32_MMIO_STATUS_OK;  /* unbuffered writes — nothing to do */
    } else if (req->opcode == S32_MMIO_OP_EXIT) {
        if (cpu && cpu->halted) *cpu->halted = true;
        if (cpu && cpu->exit_status) *cpu->exit_status = req->status;
        resp.status = S32_MMIO_STATUS_OK;
    } else if (req->opcode == S32_MMIO_OP_ARGS_INFO) {
        /* Write s32_mmio_args_info_t to data_buffer+off. */
        p = mmio->data_buffer + off;
        ((uint32_t *)p)[0] = mmio->args_argc;
        ((uint32_t *)p)[1] = mmio->args_total_bytes;
        ((uint32_t *)p)[2] = 0;
        ((uint32_t *)p)[3] = 0;
        resp.length = 16;
        resp.status = S32_MMIO_STATUS_OK;
    } else if (req->opcode == S32_MMIO_OP_ARGS_DATA) {
        if (mmio->args_blob && len >= mmio->args_total_bytes &&
            off + mmio->args_total_bytes <= S32_MMIO_DATA_CAPACITY) {
            memcpy((char *)(mmio->data_buffer + off),
                   (char *)mmio->args_blob, (int)mmio->args_total_bytes);
            resp.length = mmio->args_total_bytes;
            resp.status = S32_MMIO_STATUS_OK;
        }
    } else if (req->opcode == S32_MMIO_OP_ENVP_INFO) {
        p = mmio->data_buffer + off;
        ((uint32_t *)p)[0] = mmio->envp_envc;
        ((uint32_t *)p)[1] = mmio->envp_total_bytes;
        ((uint32_t *)p)[2] = 0;
        ((uint32_t *)p)[3] = 0;
        resp.length = 16;
        resp.status = S32_MMIO_STATUS_OK;
    } else if (req->opcode == S32_MMIO_OP_ENVP_DATA) {
        if (mmio->envp_blob && len >= mmio->envp_total_bytes &&
            off + mmio->envp_total_bytes <= S32_MMIO_DATA_CAPACITY) {
            memcpy((char *)(mmio->data_buffer + off),
                   (char *)mmio->envp_blob, (int)mmio->envp_total_bytes);
            resp.length = mmio->envp_total_bytes;
            resp.status = S32_MMIO_STATUS_OK;
        }
    } else if (req->opcode == S32_MMIO_OP_GETENV) {
        /* Lookup envp_blob for "NAME=VALUE" matching the NUL-terminated
         * name in data_buffer+off; copy VALUE back to the same offset. */
        if (mmio->envp_blob && len < 256) {
            for (i = 0; i < len; i = i + 1) {
                namebuf[i] = (char)mmio->data_buffer[(off + i) % S32_MMIO_DATA_CAPACITY];
            }
            namebuf[i] = 0;
            sl = strlen(namebuf);
            envp_p = (char *)mmio->envp_blob;
            consumed = 0;
            while (consumed < mmio->envp_total_bytes) {
                int entry_len = strlen(envp_p);
                if (strncmp(envp_p, namebuf, sl) == 0 && envp_p[sl] == '=') {
                    int vlen = entry_len - sl - 1;
                    if ((uint32_t)vlen < S32_MMIO_DATA_CAPACITY) {
                        memcpy((char *)(mmio->data_buffer + off),
                               envp_p + sl + 1, vlen);
                        mmio->data_buffer[off + (uint32_t)vlen] = 0;
                        resp.length = (uint32_t)vlen;
                        resp.status = S32_MMIO_STATUS_OK;
                    }
                    break;
                }
                envp_p = envp_p + entry_len + 1;
                consumed = consumed + (uint32_t)entry_len + 1u;
            }
        }
    }
    /* All other opcodes leave resp.status = ERR. */

    emit_response(mmio, &resp);
}

void mmio_ring_process(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu) {
    while (mmio->req_head != mmio->req_tail) {
        io_descriptor_t *req = &mmio->req_ring[mmio->req_tail];
        process_request(mmio, cpu, req);
        mmio->req_tail = (mmio->req_tail + 1u) % S32_MMIO_RING_ENTRIES;
        mmio->total_requests = mmio->total_requests + 1;
    }
}
