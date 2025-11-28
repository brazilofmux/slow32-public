// SLOW-32 Ring Buffer MMIO Implementation
#include "mmio_ring.h"
#include "slow32.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <time.h>

#if defined(__APPLE__)
#define STAT_ATIME_SEC(st)  ((st).st_atimespec.tv_sec)
#define STAT_ATIME_NSEC(st) ((st).st_atimespec.tv_nsec)
#define STAT_MTIME_SEC(st)  ((st).st_mtimespec.tv_sec)
#define STAT_MTIME_NSEC(st) ((st).st_mtimespec.tv_nsec)
#define STAT_CTIME_SEC(st)  ((st).st_ctimespec.tv_sec)
#define STAT_CTIME_NSEC(st) ((st).st_ctimespec.tv_nsec)
#else
#define STAT_ATIME_SEC(st)  ((st).st_atim.tv_sec)
#define STAT_ATIME_NSEC(st) ((st).st_atim.tv_nsec)
#define STAT_MTIME_SEC(st)  ((st).st_mtim.tv_sec)
#define STAT_MTIME_NSEC(st) ((st).st_mtim.tv_nsec)
#define STAT_CTIME_SEC(st)  ((st).st_ctim.tv_sec)
#define STAT_CTIME_NSEC(st) ((st).st_ctim.tv_nsec)
#endif

static void reset_fd_table(mmio_ring_state_t *mmio) {
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        mmio->host_fds[i] = -1;
        mmio->host_fd_owned[i] = false;
    }

    mmio->host_fds[0] = STDIN_FILENO;
    mmio->host_fds[1] = STDOUT_FILENO;
    mmio->host_fds[2] = STDERR_FILENO;
}

static int alloc_guest_fd(mmio_ring_state_t *mmio, int host_fd, bool owned) {
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        if (mmio->host_fds[i] == -1) {
            mmio->host_fds[i] = host_fd;
            mmio->host_fd_owned[i] = owned;
            return (int)i;
        }
    }
    return -1;
}

static int host_fd_for_guest(mmio_ring_state_t *mmio, uint32_t guest_fd) {
    if (guest_fd >= S32_MMIO_MAX_FDS) {
        return -1;
    }
    return mmio->host_fds[guest_fd];
}

static int translate_open_flags(uint32_t guest_flags, bool *needs_mode) {
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
        if (guest_flags & flag_append) flags |= O_APPEND;
        if (guest_flags & flag_create) flags |= O_CREAT;
        if (guest_flags & flag_trunc) flags |= O_TRUNC;
        *needs_mode = (flags & O_CREAT) != 0;
        return flags;
    }

    *needs_mode = (guest_flags & O_CREAT) != 0;
    return (int)guest_flags;
}

// Debug tracing for argument MMIO operations (enabled via env var)
static bool trace_args_enabled = false;
static bool trace_io_enabled = false;
static inline void maybe_init_trace_flag(void) {
    static bool initialized = false;
    if (initialized) return;
    initialized = true;
    if (getenv("S32_MMIO_TRACE")) {
        trace_args_enabled = true;
        trace_io_enabled = true;
    }
}

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio, uint32_t heap_base, uint32_t heap_size) {
    maybe_init_trace_flag();
    memset(mmio, 0, sizeof(mmio_ring_state_t));
    
    // Initialize indices
    mmio->req_head = 0;
    mmio->req_tail = 0;
    mmio->resp_head = 0;
    mmio->resp_tail = 0;
    mmio->base_addr = 0;
    
    // Initialize heap
    mmio->brk_current = heap_base;
    mmio->brk_max = heap_base + heap_size;

    mmio->args_blob = NULL;
    mmio->args_argc = 0;
    mmio->args_total_bytes = 0;

    mmio->envp_blob = NULL;
    mmio->envp_envc = 0;
    mmio->envp_total_bytes = 0;

    reset_fd_table(mmio);
}

// Map MMIO memory region (returns host pointer to MMIO base)
void* mmio_ring_map(mmio_ring_state_t *mmio) {
    // Allocate 64KB for entire MMIO region
    size_t window_size = S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY;
    void *mmio_mem = mmap(NULL, window_size, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (mmio_mem == MAP_FAILED) {
        perror("Failed to map MMIO memory");
        return NULL;
    }
    
    // Set up pointers to ring buffers and data area
    uint8_t *base = (uint8_t*)mmio_mem;
    
    // Skip head/tail registers at offset 0
    mmio->req_ring = (io_descriptor_t*)(base + S32_MMIO_REQ_RING_OFFSET);
    mmio->resp_ring = (io_descriptor_t*)(base + S32_MMIO_RESP_RING_OFFSET);
    mmio->data_buffer = base + S32_MMIO_DATA_BUFFER_OFFSET;
    
    // Clear rings
    memset(mmio->req_ring, 0, S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES);
    memset(mmio->resp_ring, 0, S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES);
    
    return mmio_mem;
}

void mmio_ring_clear_args(mmio_ring_state_t *mmio) {
    if (!mmio) {
        return;
    }
    free(mmio->args_blob);
    mmio->args_blob = NULL;
    mmio->args_argc = 0;
    mmio->args_total_bytes = 0;
}

int mmio_ring_set_args(mmio_ring_state_t *mmio,
                       uint32_t argc,
                       char *const *argv) {
    if (!mmio) {
        return -1;
    }

    mmio_ring_clear_args(mmio);

    if (argc == 0 || argv == NULL) {
        return 0;
    }

    uint64_t total_bytes = 0;
    for (uint32_t i = 0; i < argc; ++i) {
        const char *arg = argv[i] ? argv[i] : "";
        size_t len = strlen(arg) + 1u;
        total_bytes += len;
        if (total_bytes > S32_MMIO_ARGS_MAX_BYTES || total_bytes > UINT32_MAX) {
            mmio_ring_clear_args(mmio);
            return -1;
        }
    }

    if (total_bytes == 0) {
        mmio->args_argc = argc;
        mmio->args_total_bytes = 0;
        return 0;
    }

    uint8_t *blob = (uint8_t *)malloc((size_t)total_bytes);
    if (!blob) {
        mmio_ring_clear_args(mmio);
        return -1;
    }

    size_t offset = 0;
    for (uint32_t i = 0; i < argc; ++i) {
        const char *arg = argv[i] ? argv[i] : "";
        size_t len = strlen(arg) + 1u;
        memcpy(blob + offset, arg, len);
        offset += len;
    }

    mmio->args_blob = blob;
    mmio->args_argc = argc;
    mmio->args_total_bytes = (uint32_t)total_bytes;
    return 0;
}

void mmio_ring_clear_envp(mmio_ring_state_t *mmio) {
    if (!mmio) {
        return;
    }
    free(mmio->envp_blob);
    mmio->envp_blob = NULL;
    mmio->envp_envc = 0;
    mmio->envp_total_bytes = 0;
}

int mmio_ring_set_envp(mmio_ring_state_t *mmio,
                       char *const *envp) {
    if (!mmio) {
        return -1;
    }

    mmio_ring_clear_envp(mmio);

    if (envp == NULL) {
        return 0;
    }

    // Count environment variables and total bytes
    uint32_t envc = 0;
    uint64_t total_bytes = 0;
    for (char *const *p = envp; *p != NULL; ++p) {
        size_t len = strlen(*p) + 1u;
        total_bytes += len;
        envc++;
        if (total_bytes > S32_MMIO_ENVP_MAX_BYTES || total_bytes > UINT32_MAX) {
            mmio_ring_clear_envp(mmio);
            return -1;
        }
    }

    if (envc == 0 || total_bytes == 0) {
        mmio->envp_envc = 0;
        mmio->envp_total_bytes = 0;
        return 0;
    }

    uint8_t *blob = (uint8_t *)malloc((size_t)total_bytes);
    if (!blob) {
        mmio_ring_clear_envp(mmio);
        return -1;
    }

    size_t offset = 0;
    for (char *const *p = envp; *p != NULL; ++p) {
        size_t len = strlen(*p) + 1u;
        memcpy(blob + offset, *p, len);
        offset += len;
    }

    mmio->envp_blob = blob;
    mmio->envp_envc = envc;
    mmio->envp_total_bytes = (uint32_t)total_bytes;
    return 0;
}

// MMIO read handler
uint32_t mmio_ring_read(mmio_ring_state_t *mmio, uint32_t addr, int size) {
    if (size != 4) return 0;  // Only 32-bit reads

    if (addr < mmio->base_addr) {
        return 0;
    }

    uint32_t rel = addr - mmio->base_addr;
    if (rel >= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
        return 0;
    }

    switch (rel) {
        case S32_MMIO_REQ_HEAD_OFFSET:
            return mmio->req_head;
        case S32_MMIO_REQ_TAIL_OFFSET:
            return mmio->req_tail;
        case S32_MMIO_RESP_HEAD_OFFSET:
            return mmio->resp_head;
        case S32_MMIO_RESP_TAIL_OFFSET:
            return mmio->resp_tail;
        default:
            // Reading from rings or data buffer
            if (rel >= S32_MMIO_REQ_RING_OFFSET &&
                rel < S32_MMIO_REQ_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_REQ_RING_OFFSET) / 4;
                return ((uint32_t*)mmio->req_ring)[offset];
            }
            if (rel >= S32_MMIO_RESP_RING_OFFSET &&
                rel < S32_MMIO_RESP_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_RESP_RING_OFFSET) / 4;
                return ((uint32_t*)mmio->resp_ring)[offset];
            }
            if (rel >= S32_MMIO_DATA_BUFFER_OFFSET &&
                rel < S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
                uint32_t offset = rel - S32_MMIO_DATA_BUFFER_OFFSET;
                uint32_t value = 0;
                memcpy(&value, mmio->data_buffer + offset, 4);
                return value;
            }
            return 0;
    }
}

// MMIO write handler
void mmio_ring_write(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu, uint32_t addr, uint32_t value, int size) {
    (void)cpu;  // Reserved for future use (e.g., trapping on special writes)

    if (size != 4) return;  // Only 32-bit writes

    if (addr < mmio->base_addr) {
        return;
    }

    uint32_t rel = addr - mmio->base_addr;
    if (rel >= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
        return;
    }

    switch (rel) {
        case S32_MMIO_REQ_HEAD_OFFSET:
            mmio->req_head = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_REQ_TAIL_OFFSET:
            mmio->req_tail = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_RESP_HEAD_OFFSET:
            mmio->resp_head = value % S32_MMIO_RING_ENTRIES;
            break;
        case S32_MMIO_RESP_TAIL_OFFSET:
            mmio->resp_tail = value % S32_MMIO_RING_ENTRIES;
            break;
        default:
            // Writing to rings or data buffer
            if (rel >= S32_MMIO_REQ_RING_OFFSET &&
                rel < S32_MMIO_REQ_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_REQ_RING_OFFSET) / 4;
                ((uint32_t*)mmio->req_ring)[offset] = value;
            }
            else if (rel >= S32_MMIO_RESP_RING_OFFSET &&
                     rel < S32_MMIO_RESP_RING_OFFSET + S32_MMIO_RING_ENTRIES * S32_MMIO_DESC_BYTES) {
                uint32_t offset = (rel - S32_MMIO_RESP_RING_OFFSET) / 4;
                ((uint32_t*)mmio->resp_ring)[offset] = value;
            }
            else if (rel >= S32_MMIO_DATA_BUFFER_OFFSET &&
                     rel < S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
                uint32_t offset = rel - S32_MMIO_DATA_BUFFER_OFFSET;
                memcpy(mmio->data_buffer + offset, &value, 4);
            }
            break;
    }
}

// Process a single request
static void process_request(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu, io_descriptor_t *req) {
    io_descriptor_t resp = {0};
    resp.opcode = req->opcode;
    resp.offset = req->offset;
    
    switch (req->opcode) {
        case S32_MMIO_OP_NOP:
            resp.status = 0;
            break;
            
        case S32_MMIO_OP_PUTCHAR: {
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint8_t ch = mmio->data_buffer[offset];
            fputc(ch, stdout);
            fflush(stdout);
            resp.status = 0;
            break;
        }
        
        case S32_MMIO_OP_WRITE: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;
            uint32_t to_write = req->length;

            if (host_fd < 0 || to_write == 0 || to_write > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            if (to_write > max_bytes) {
                to_write = max_bytes;
            }

            ssize_t written = write(host_fd, mmio->data_buffer + offset, to_write);
            if (written < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            resp.length = (uint32_t)written;
            resp.status = (uint32_t)written;
            break;
        }

        case S32_MMIO_OP_READ: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;
            uint32_t to_read = req->length;

            if (host_fd < 0 || to_read == 0 || to_read > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] READ invalid fd=%d len=%u\n",
                            host_fd, to_read);
                }
                break;
            }

            if (to_read > max_bytes) {
                to_read = max_bytes;
            }

            ssize_t read_count = read(host_fd, mmio->data_buffer + offset, to_read);
            if (read_count < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] READ error fd=%d len=%u errno=%d\n",
                            host_fd, to_read, errno);
                }
                break;
            }

            resp.length = (uint32_t)read_count;
            resp.status = (uint32_t)read_count;
            if (trace_io_enabled) {
                fprintf(stderr, "[MMIO] READ fd=%d len=%u -> %zd\n",
                        host_fd, to_read, read_count);
            }
            break;
        }

        case S32_MMIO_OP_OPEN: {
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            char *path = (char *)malloc(req->length + 1u);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';
            path[req->length - 1u] = '\0';

            bool needs_mode = false;
            int flags = translate_open_flags(req->status, &needs_mode);
            int host_fd = needs_mode ? open(path, flags, 0644) : open(path, flags);
            free(path);

            if (host_fd < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] OPEN failed (flags=0x%x errno=%d)\n",
                            req->status, errno);
                }
                break;
            }

            int guest_fd = alloc_guest_fd(mmio, host_fd, true);
            if (guest_fd < 0) {
                close(host_fd);
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] OPEN no free guest fd (host_fd=%d)\n",
                            host_fd);
                }
                break;
            }

            if (trace_io_enabled) {
                fprintf(stderr, "[MMIO] OPEN guest_fd=%d host_fd=%d len=%u\n",
                        guest_fd, host_fd, req->length);
            }
            resp.status = (uint32_t)guest_fd;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_CLOSE: {
            uint32_t guest_fd = req->status;
            if (guest_fd >= S32_MMIO_MAX_FDS || mmio->host_fds[guest_fd] < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            int host_fd = mmio->host_fds[guest_fd];
            int rc = 0;
            if (mmio->host_fd_owned[guest_fd]) {
                rc = close(host_fd);
            }

            mmio->host_fds[guest_fd] = -1;
            mmio->host_fd_owned[guest_fd] = false;

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_SEEK: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0 || req->length < 8u) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - 8u)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint8_t whence_raw = mmio->data_buffer[offset];
            int32_t distance = 0;
            memcpy(&distance, mmio->data_buffer + offset + 4u, sizeof(int32_t));

            off_t new_pos = lseek(host_fd, (off_t)distance, (int)whence_raw);
            if (new_pos == (off_t)-1) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            resp.status = (uint32_t)new_pos;
            resp.length = 0;
            break;
        }
        
        case S32_MMIO_OP_GETCHAR: {
            int ch = fgetc(stdin);
            if (ch != EOF) {
                mmio->data_buffer[req->offset % S32_MMIO_DATA_CAPACITY] = (uint8_t)ch;
                resp.length = 1;
                resp.status = 0;
            } else {
                resp.length = 0;
                resp.status = EOF;
            }
            break;
        }
        
        case S32_MMIO_OP_BRK: {
            uint32_t requested = req->status;  // New break value in status
            
            if (requested == 0) {
                // Return current break
                resp.status = mmio->brk_current;
                resp.length = 0;
            } else if (requested >= mmio->brk_current && requested <= mmio->brk_max) {
                // Expand heap
                mmio->brk_current = requested;
                resp.status = requested;
                resp.length = 0;
            } else {
                // Failed to expand
                resp.status = mmio->brk_current;
                resp.length = ENOMEM;
            }
            break;
        }
        
        case S32_MMIO_OP_EXIT:
            if (cpu) {
                if (cpu->halted) {
                    *(cpu->halted) = true;
                }
                if (cpu->exit_status) {
                    *(cpu->exit_status) = req->status;
                }
            }
            resp.status = req->status;  // Exit code
            break;
            
        case S32_MMIO_OP_FLUSH:
            fflush(stdout);
            fflush(stderr);
            resp.status = 0;
            break;

        case S32_MMIO_OP_STAT: {
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;

            if (max_bytes < sizeof(s32_mmio_stat_result_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            struct stat host_stat;
            memset(&host_stat, 0, sizeof(host_stat));

            int rc = -1;
            if (req->status == S32_MMIO_STAT_PATH_SENTINEL) {
                if (req->length == 0 || req->length > max_bytes) {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                    break;
                }

                char *path = (char *)malloc(req->length);
                if (!path) {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                    break;
                }

                memcpy(path, mmio->data_buffer + offset, req->length);
                path[req->length - 1u] = '\0';
                rc = stat(path, &host_stat);
                free(path);
            } else {
                rc = fstat((int)req->status, &host_stat);
            }

            if (rc != 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
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
                .st_size = (uint64_t)((host_stat.st_size < 0) ? 0 : host_stat.st_size),
                .st_blksize = (uint64_t)((host_stat.st_blksize < 0) ? 0 : host_stat.st_blksize),
                .st_blocks = (uint64_t)((host_stat.st_blocks < 0) ? 0 : host_stat.st_blocks),
                .st_atime_sec = (uint64_t)STAT_ATIME_SEC(host_stat),
                .st_atime_nsec = (uint32_t)STAT_ATIME_NSEC(host_stat),
                .st_mtime_sec = (uint64_t)STAT_MTIME_SEC(host_stat),
                .st_mtime_nsec = (uint32_t)STAT_MTIME_NSEC(host_stat),
                .st_ctime_sec = (uint64_t)STAT_CTIME_SEC(host_stat),
                .st_ctime_nsec = (uint32_t)STAT_CTIME_NSEC(host_stat),
            };

            memcpy(mmio->data_buffer + offset, &result, sizeof(result));
            resp.length = sizeof(result);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_GETTIME: {
            struct timespec ts;
            if (clock_gettime(CLOCK_REALTIME, &ts) != 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            if (req->length < sizeof(s32_mmio_timepair64_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_timepair64_t))) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint64_t seconds = (ts.tv_sec < 0) ? 0ull : (uint64_t)ts.tv_sec;
            s32_mmio_timepair64_t pair = {
                .seconds_lo = (uint32_t)(seconds & 0xFFFFFFFFu),
                .seconds_hi = (uint32_t)(seconds >> 32),
                .nanoseconds = (uint32_t)ts.tv_nsec,
                .reserved = 0u,
            };
            memcpy(mmio->data_buffer + offset, &pair, sizeof(pair));
            resp.length = sizeof(s32_mmio_timepair64_t);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_SLEEP: {
            if (req->length < sizeof(s32_mmio_timepair64_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_timepair64_t))) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            s32_mmio_timepair64_t interval = {0u, 0u, 0u, 0u};
            memcpy(&interval, mmio->data_buffer + offset, sizeof(interval));

            if (interval.nanoseconds >= 1000000000u) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint64_t seconds = ((uint64_t)interval.seconds_hi << 32) | interval.seconds_lo;

            struct timespec req_ts = {
                .tv_sec = (time_t)seconds,
                .tv_nsec = (long)interval.nanoseconds,
            };
            struct timespec rem_ts = {0, 0};

            int rc = nanosleep(&req_ts, &rem_ts);
            if (rc == -1) {
                if (errno == EINTR) {
                    uint64_t rem_secs = (rem_ts.tv_sec < 0) ? 0ull : (uint64_t)rem_ts.tv_sec;
                    s32_mmio_timepair64_t remainder = {
                        .seconds_lo = (uint32_t)(rem_secs & 0xFFFFFFFFu),
                        .seconds_hi = (uint32_t)(rem_secs >> 32),
                        .nanoseconds = (uint32_t)rem_ts.tv_nsec,
                        .reserved = 0u,
                    };
                    memcpy(mmio->data_buffer + offset, &remainder, sizeof(remainder));
                    resp.length = sizeof(s32_mmio_timepair64_t);
                    resp.status = S32_MMIO_STATUS_EINTR;
                } else {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                }
                break;
            }

            s32_mmio_timepair64_t remainder = {0u, 0u, 0u, 0u};
            memcpy(mmio->data_buffer + offset, &remainder, sizeof(remainder));
            resp.length = sizeof(s32_mmio_timepair64_t);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_ARGS_INFO: {
            if (req->length < sizeof(s32_mmio_args_info_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_args_info_t))) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            s32_mmio_args_info_t info = {
                .argc = mmio->args_argc,
                .total_bytes = mmio->args_total_bytes,
                .flags = (mmio->args_total_bytes > 0) ? 0u : 0u,
                .reserved = 0u,
            };

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] ARGS_INFO argc=%u total=%u offset=%u\n",
                        info.argc, info.total_bytes, offset);
            }

            memcpy(mmio->data_buffer + offset, &info, sizeof(info));
            resp.length = sizeof(info);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_ARGS_DATA: {
            if (req->length == 0) {
                resp.length = 0;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            if (req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t dest = req->offset % S32_MMIO_DATA_CAPACITY;
            if (dest > (S32_MMIO_DATA_CAPACITY - req->length)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t source_offset = req->status;
            if (source_offset > mmio->args_total_bytes) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t remaining = mmio->args_total_bytes - source_offset;
            uint32_t to_copy = req->length;
            if (to_copy > remaining) {
                to_copy = remaining;
            }

            if (to_copy > 0) {
                if (!mmio->args_blob) {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                    break;
                }
                memcpy(mmio->data_buffer + dest,
                       mmio->args_blob + source_offset,
                       to_copy);
            }

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] ARGS_DATA req_len=%u dest=0x%X src_off=%u copied=%u\n",
                        req->length, dest, source_offset, to_copy);
                if (mmio->args_blob && to_copy > 0) {
                    uint32_t preview = (to_copy < 16u) ? to_copy : 16u;
                    fprintf(stderr, "              args_blob[0..%u]:", preview);
                    for (uint32_t i = 0; i < preview; ++i) {
                        fprintf(stderr, " %02x", mmio->args_blob[i]);
                    }
                    fprintf(stderr, "\n");
                    fprintf(stderr, "              data_buffer[0..%u]:", preview);
                    for (uint32_t i = 0; i < preview; ++i) {
                        fprintf(stderr, " %02x", mmio->data_buffer[i]);
                    }
                    fprintf(stderr, "\n");
                }
            }

            resp.length = to_copy;
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_ENVP_INFO: {
            if (req->length < sizeof(s32_mmio_envp_info_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_envp_info_t))) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            s32_mmio_envp_info_t info = {
                .envc = mmio->envp_envc,
                .total_bytes = mmio->envp_total_bytes,
                .flags = 0u,
                .reserved = 0u,
            };

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] ENVP_INFO envc=%u total=%u offset=%u\n",
                        info.envc, info.total_bytes, offset);
            }

            memcpy(mmio->data_buffer + offset, &info, sizeof(info));
            resp.length = sizeof(info);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_ENVP_DATA: {
            if (req->length == 0) {
                resp.length = 0;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            if (req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t dest = req->offset % S32_MMIO_DATA_CAPACITY;
            if (dest > (S32_MMIO_DATA_CAPACITY - req->length)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t source_offset = req->status;
            if (source_offset > mmio->envp_total_bytes) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t remaining = mmio->envp_total_bytes - source_offset;
            uint32_t to_copy = req->length;
            if (to_copy > remaining) {
                to_copy = remaining;
            }

            if (to_copy > 0) {
                if (!mmio->envp_blob) {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                    break;
                }
                memcpy(mmio->data_buffer + dest,
                       mmio->envp_blob + source_offset,
                       to_copy);
            }

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] ENVP_DATA req_len=%u dest=0x%X src_off=%u copied=%u\n",
                        req->length, dest, source_offset, to_copy);
            }

            resp.length = to_copy;
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_GETENV: {
            // Request: name in data buffer, length = name length (including NUL)
            // Response: value in data buffer, status = value length (0 if not found)
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            // Extract the name from data buffer
            char *name = (char *)malloc(req->length + 1);
            if (!name) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(name, mmio->data_buffer + offset, req->length);
            name[req->length] = '\0';

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] GETENV request for '%s'\n", name);
            }

            // Look up in host environment
            const char *value = getenv(name);
            free(name);

            if (!value) {
                if (trace_args_enabled) {
                    fprintf(stderr, "[MMIO TRACE] GETENV not found\n");
                }
                // Not found - return error status
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            size_t value_len = strlen(value);

            // Safety check: ensure we don't write past end of buffer
            // offset is where we write (req->offset % CAPACITY)
            if (offset + value_len > S32_MMIO_DATA_CAPACITY) {
                value_len = S32_MMIO_DATA_CAPACITY - offset;
            }

            // Copy value to data buffer
            memcpy(mmio->data_buffer + offset, value, value_len);
            resp.length = (uint32_t)value_len;
            resp.status = (uint32_t)value_len;

            if (trace_args_enabled) {
                fprintf(stderr, "[MMIO TRACE] GETENV found value len=%zu\n", value_len);
            }
            break;
        }

        default:
            resp.status = ENOSYS;  // Not implemented
            break;
    }
    
    // Write response
    if (!ring_full(mmio->resp_head, mmio->resp_tail)) {
        if (trace_io_enabled) {
            fprintf(stderr, "[MMIO] RESP head=%u tail=%u opcode=0x%X status=%u len=%u\n",
                    mmio->resp_head, mmio->resp_tail, resp.opcode,
                    resp.status, resp.length);
        }
        mmio->resp_ring[mmio->resp_head] = resp;
        mmio->resp_head = ring_next(mmio->resp_head);
        mmio->total_responses++;
    }
}

// Process pending requests
void mmio_ring_process(mmio_ring_state_t *mmio, mmio_cpu_iface_t *cpu) {
    while (!ring_empty(mmio->req_head, mmio->req_tail)) {
        io_descriptor_t *req = &mmio->req_ring[mmio->req_tail];
        process_request(mmio, cpu, req);
        mmio->req_tail = ring_next(mmio->req_tail);
        mmio->total_requests++;
    }
}
