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
#include <sys/ioctl.h>
#include <termios.h>
#include <poll.h>
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
        mmio->fd_types[i] = S32_FD_TYPE_FILE;
        mmio->host_dirs[i] = NULL;
    }

    mmio->host_fds[0] = STDIN_FILENO;
    mmio->host_fds[1] = STDOUT_FILENO;
    mmio->host_fds[2] = STDERR_FILENO;
}

static int alloc_guest_fd(mmio_ring_state_t *mmio, int host_fd, bool owned) {
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        if (mmio->host_fds[i] == -1 && mmio->host_dirs[i] == NULL) {
            mmio->host_fds[i] = host_fd;
            mmio->host_fd_owned[i] = owned;
            mmio->fd_types[i] = S32_FD_TYPE_FILE;
            mmio->host_dirs[i] = NULL;
            return (int)i;
        }
    }
    return -1;
}

static int alloc_guest_dir_fd(mmio_ring_state_t *mmio, DIR *host_dir) {
    for (uint32_t i = 0; i < S32_MMIO_MAX_FDS; ++i) {
        if (mmio->host_fds[i] == -1 && mmio->host_dirs[i] == NULL) {
            mmio->host_fds[i] = -1;  // No underlying file descriptor
            mmio->host_fd_owned[i] = true;
            mmio->fd_types[i] = S32_FD_TYPE_DIR;
            mmio->host_dirs[i] = host_dir;
            return (int)i;
        }
    }
    return -1;
}

static DIR *host_dir_for_guest(mmio_ring_state_t *mmio, uint32_t guest_fd) {
    if (guest_fd >= S32_MMIO_MAX_FDS) {
        return NULL;
    }
    if (mmio->fd_types[guest_fd] != S32_FD_TYPE_DIR) {
        return NULL;
    }
    return mmio->host_dirs[guest_fd];
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

// ========== Service negotiation infrastructure ==========

void mmio_set_policy(mmio_ring_state_t *mmio, const svc_policy_t *policy) {
    memcpy(&mmio->policy, policy, sizeof(svc_policy_t));
}

bool mmio_policy_allows(mmio_ring_state_t *mmio, const char *name) {
    // Check deny list first
    for (int i = 0; i < mmio->policy.deny_count; i++) {
        if (strcmp(mmio->policy.deny_list[i], name) == 0) return false;
    }
    // If there's an explicit allow list, only allow listed services
    if (mmio->policy.allow_count > 0) {
        for (int i = 0; i < mmio->policy.allow_count; i++) {
            if (strcmp(mmio->policy.allow_list[i], name) == 0) return true;
        }
        return false;
    }
    return mmio->policy.default_allow;
}

void mmio_cleanup_services(mmio_ring_state_t *mmio) {
    for (int i = 0; i < mmio->num_services; i++) {
        svc_session_t *svc = &mmio->services[i];
        if (svc->active && svc->cleanup && svc->state) {
            svc->cleanup(svc->state);
        }
        svc->active = false;
        svc->state = NULL;
    }
    mmio->num_services = 0;
}

// ========== Term service implementation ==========

typedef struct {
    struct termios saved_termios;
    bool raw_mode;
    bool termios_saved;
} term_state_t;

static void *term_create(void) {
    term_state_t *ts = calloc(1, sizeof(term_state_t));
    if (!ts) return NULL;
    if (isatty(STDIN_FILENO) && tcgetattr(STDIN_FILENO, &ts->saved_termios) == 0) {
        ts->termios_saved = true;
    }
    return ts;
}

static void term_cleanup(void *state) {
    term_state_t *ts = (term_state_t *)state;
    if (!ts) return;
    if (ts->raw_mode && ts->termios_saved) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &ts->saved_termios);
    }
    free(ts);
}

static void term_handle(void *state, mmio_ring_state_t *mmio,
                         uint32_t sub_opcode, io_descriptor_t *req,
                         io_descriptor_t *resp) {
    term_state_t *ts = (term_state_t *)state;
    uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;

    switch (sub_opcode) {
        case S32_TERM_SET_MODE: {
            // status field: 1 = raw, 0 = cooked
            if (!isatty(STDIN_FILENO)) {
                resp->status = S32_MMIO_STATUS_ERR;
                break;
            }
            if (req->status) {
                // Enter raw mode
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
                // Restore cooked mode
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
                // Default fallback
                ws.ws_row = 24;
                ws.ws_col = 80;
            }
            // Write rows and cols to data buffer as two uint32_t
            if (offset + 8 > S32_MMIO_DATA_CAPACITY) {
                resp->status = S32_MMIO_STATUS_ERR;
                break;
            }
            uint32_t rows = ws.ws_row;
            uint32_t cols = ws.ws_col;
            memcpy(mmio->data_buffer + offset, &rows, 4);
            memcpy(mmio->data_buffer + offset + 4, &cols, 4);
            resp->length = 8;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_MOVE_CURSOR: {
            // status = (row << 16) | col  (1-based)
            uint32_t row = (req->status >> 16) & 0xFFFF;
            uint32_t col = req->status & 0xFFFF;
            fprintf(stdout, "\033[%u;%uH", row, col);
            fflush(stdout);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_CLEAR: {
            // status: 0 = full screen, 1 = to end of line, 2 = to end of screen
            switch (req->status) {
                case 0: fprintf(stdout, "\033[2J\033[H"); break;
                case 1: fprintf(stdout, "\033[K"); break;
                case 2: fprintf(stdout, "\033[J"); break;
                default: fprintf(stdout, "\033[2J\033[H"); break;
            }
            fflush(stdout);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_SET_ATTR: {
            // status: 0 = normal, 1 = bold, 7 = reverse
            fprintf(stdout, "\033[%um", req->status);
            fflush(stdout);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_READ_KEY: {
            // Blocking read of one byte
            unsigned char ch;
            ssize_t n = read(STDIN_FILENO, &ch, 1);
            if (n == 1) {
                mmio->data_buffer[offset] = ch;
                resp->length = 1;
                resp->status = (uint32_t)ch;
            } else {
                resp->status = S32_MMIO_STATUS_EOF;
                resp->length = 0;
            }
            break;
        }
        case S32_TERM_KEY_AVAIL: {
            // Non-blocking poll: returns 1 if key available, 0 if not
            struct pollfd pfd = { .fd = STDIN_FILENO, .events = POLLIN };
            int ret = poll(&pfd, 1, 0);
            resp->status = (ret > 0 && (pfd.revents & POLLIN)) ? 1 : 0;
            break;
        }
        case S32_TERM_SET_COLOR: {
            // status = (fg << 8) | bg  (ANSI color 0-7)
            uint32_t fg = (req->status >> 8) & 0xFF;
            uint32_t bg = req->status & 0xFF;
            fprintf(stdout, "\033[3%u;4%um", fg, bg);
            fflush(stdout);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        default:
            resp->status = S32_MMIO_STATUS_ERR;
            break;
    }
}

// ========== Built-in service table ==========

typedef struct {
    const char *name;
    uint32_t opcode_count;
    uint32_t version;
    void *(*create)(void);
    void (*cleanup)(void *state);
    void (*handle)(void *state, mmio_ring_state_t *mmio,
                   uint32_t sub_opcode, io_descriptor_t *req,
                   io_descriptor_t *resp);
} builtin_service_t;

static const builtin_service_t builtin_services[] = {
    {
        .name = "term",
        .opcode_count = S32_TERM_OPCODE_COUNT,
        .version = 1,
        .create = term_create,
        .cleanup = term_cleanup,
        .handle = term_handle,
    },
};

#define NUM_BUILTIN_SERVICES (sizeof(builtin_services) / sizeof(builtin_services[0]))

static const builtin_service_t *find_builtin_service(const char *name) {
    for (size_t i = 0; i < NUM_BUILTIN_SERVICES; i++) {
        if (strcmp(builtin_services[i].name, name) == 0) {
            return &builtin_services[i];
        }
    }
    return NULL;
}

// Map legacy opcodes to service names for policy enforcement
static const char *legacy_opcode_service(uint32_t opcode) {
    if (opcode >= 0x03 && opcode <= 0x07) return "fs";  // WRITE..SEEK
    if (opcode == 0x0A) return "fs";   // STAT
    if (opcode == 0x0B) return "fs";   // FLUSH (file flush)
    if (opcode == 0x0C) return "fs";   // READ_DIRECT
    if (opcode >= 0x20 && opcode <= 0x2A) return "fs";  // FS metadata
    if (opcode >= 0x30 && opcode <= 0x3F) return "time";
    if (opcode >= 0x40 && opcode <= 0x4F) return "net";
    if (opcode >= 0x60 && opcode <= 0x6F) return "env";
    // 0x01 (PUTCHAR), 0x02 (GETCHAR), 0x08 (BRK), 0x09 (EXIT) always allowed
    return NULL;
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
    
    mmio->guest_mem_base = NULL;
    mmio->guest_mem_size = 0;

    // Initialize heap
    mmio->brk_current = heap_base;
    mmio->brk_max = heap_base + heap_size;

    mmio->args_blob = NULL;
    mmio->args_argc = 0;
    mmio->args_total_bytes = 0;

    mmio->envp_blob = NULL;
    mmio->envp_envc = 0;
    mmio->envp_total_bytes = 0;

    // Service negotiation defaults
    mmio->num_services = 0;
    mmio->next_dynamic_opcode = 0x80;  // Start dynamic services at 0x80
    memset(mmio->services, 0, sizeof(mmio->services));
    mmio->policy.default_allow = true;
    mmio->policy.allow_count = 0;
    mmio->policy.deny_count = 0;

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

    // Policy gate: check legacy opcode against policy
    const char *legacy_svc = legacy_opcode_service(req->opcode);
    if (legacy_svc && !mmio_policy_allows(mmio, legacy_svc)) {
        resp.status = S32_MMIO_STATUS_ERR;
        goto write_response;
    }

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

        case S32_MMIO_OP_READ_DIRECT: {
            // Request: status=fd, offset=guest_addr, length=count
            // Response: status=read_count or ERR
            
            if (req->length == 0) {
                resp.length = 0;
                resp.status = 0;
                break;
            }

            // Must have guest memory configured
            if (!mmio->guest_mem_base) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                     fprintf(stderr, "[MMIO] READ_DIRECT failed: guest memory not configured\n");
                }
                break;
            }

            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t guest_addr = req->offset;
            uint32_t count = req->length;

            // Validate guest address range
            if (guest_addr >= mmio->guest_mem_size || 
                (uint64_t)guest_addr + count > mmio->guest_mem_size) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] READ_DIRECT bounds check failed: addr=0x%08X len=%u size=0x%08X\n",
                            guest_addr, count, mmio->guest_mem_size);
                }
                break;
            }

            void *dest = (uint8_t *)mmio->guest_mem_base + guest_addr;
            
            ssize_t read_count = read(host_fd, dest, count);
            
            if (read_count < 0) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] READ_DIRECT error fd=%d addr=0x%08X len=%u errno=%d\n",
                            host_fd, guest_addr, count, errno);
                }
                break;
            }
            
            resp.status = (uint32_t)read_count;
            resp.length = (uint32_t)read_count;

            if (trace_io_enabled) {
                fprintf(stderr, "[MMIO] READ_DIRECT fd=%d addr=0x%08X len=%u -> %zd\n",
                        host_fd, guest_addr, count, read_count);
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

        // ========== Filesystem metadata operations (0x20-0x2A) ==========

        case S32_MMIO_OP_UNLINK: {
            // Request: path in data buffer
            // Response: OK or ERR
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = unlink(path);
            free(path);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_RENAME: {
            // Request: oldpath + newpath in data buffer, status = old_len
            // Response: OK or ERR
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t old_len = req->status;
            if (old_len == 0 || old_len >= req->length) {
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

            char *buffer = (char *)malloc(req->length + 2);
            if (!buffer) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(buffer, mmio->data_buffer + offset, req->length);
            buffer[old_len] = '\0';
            buffer[req->length] = '\0';

            const char *oldpath = buffer;
            const char *newpath = buffer + old_len;

            int rc = rename(oldpath, newpath);
            free(buffer);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_MKDIR: {
            // Request: path in data buffer, status = mode
            // Response: OK or ERR
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            mode_t mode = (mode_t)req->status;
            if (mode == 0) mode = 0755;  // Default mode

            int rc = mkdir(path, mode);
            free(path);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_RMDIR: {
            // Request: path in data buffer
            // Response: OK or ERR
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = rmdir(path);
            free(path);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_LSTAT: {
            // Request: path in data buffer (like STAT but no symlink follow)
            // Response: stat result in data buffer
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;

            if (max_bytes < sizeof(s32_mmio_stat_result_t)) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            struct stat host_stat;
            memset(&host_stat, 0, sizeof(host_stat));
            int rc = lstat(path, &host_stat);
            free(path);

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

        case S32_MMIO_OP_ACCESS: {
            // Request: path in data buffer, status = mode (F_OK|R_OK|W_OK|X_OK)
            // Response: OK if accessible, ERR if not
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            // Map guest access modes to host (they match POSIX values)
            int mode = (int)req->status;
            int rc = access(path, mode);
            free(path);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_CHDIR: {
            // Request: path in data buffer
            // Response: OK or ERR
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = chdir(path);
            free(path);

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_GETCWD: {
            // Request: length = max buffer size
            // Response: path in data buffer, status = actual length (including NUL)
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_len = S32_MMIO_DATA_CAPACITY - offset;
            if (req->length < max_len) max_len = req->length;

            char *cwd = getcwd((char *)(mmio->data_buffer + offset), max_len);
            if (!cwd) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            size_t len = strlen(cwd) + 1;  // Include NUL terminator
            resp.length = (uint32_t)len;
            resp.status = (uint32_t)len;
            break;
        }

        case S32_MMIO_OP_OPENDIR: {
            // Request: path in data buffer
            // Response: directory descriptor in status, or ERR
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

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            DIR *host_dir = opendir(path);
            free(path);

            if (!host_dir) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            int guest_fd = alloc_guest_dir_fd(mmio, host_dir);
            if (guest_fd < 0) {
                closedir(host_dir);
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            resp.status = (uint32_t)guest_fd;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_READDIR: {
            // Request: status = directory descriptor
            // Response: dirent in data buffer, status = OK/EOF/ERR
            uint32_t guest_fd = req->status;
            DIR *host_dir = host_dir_for_guest(mmio, guest_fd);

            if (!host_dir) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_dirent_t))) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            errno = 0;
            struct dirent *entry = readdir(host_dir);
            if (!entry) {
                if (errno == 0) {
                    // End of directory
                    resp.status = S32_MMIO_STATUS_EOF;
                    resp.length = 0;
                } else {
                    resp.status = S32_MMIO_STATUS_ERR;
                    resp.length = 0;
                }
                break;
            }

            s32_mmio_dirent_t result;
            memset(&result, 0, sizeof(result));
            result.d_ino = (uint64_t)entry->d_ino;
            result.d_type = (uint32_t)entry->d_type;
            size_t namelen = strlen(entry->d_name);
            if (namelen > 255) namelen = 255;
            result.d_namlen = (uint32_t)namelen;
            memcpy(result.d_name, entry->d_name, namelen);
            result.d_name[namelen] = '\0';

            memcpy(mmio->data_buffer + offset, &result, sizeof(result));
            resp.length = sizeof(s32_mmio_dirent_t);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_CLOSEDIR: {
            // Request: status = directory descriptor
            // Response: OK or ERR
            uint32_t guest_fd = req->status;

            if (guest_fd >= S32_MMIO_MAX_FDS) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            if (mmio->fd_types[guest_fd] != S32_FD_TYPE_DIR || !mmio->host_dirs[guest_fd]) {
                resp.status = S32_MMIO_STATUS_ERR;
                resp.length = 0;
                break;
            }

            int rc = closedir(mmio->host_dirs[guest_fd]);
            mmio->host_dirs[guest_fd] = NULL;
            mmio->host_fds[guest_fd] = -1;
            mmio->host_fd_owned[guest_fd] = false;
            mmio->fd_types[guest_fd] = S32_FD_TYPE_FILE;

            resp.status = (rc == 0) ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            resp.length = 0;
            break;
        }

        // ========== Service negotiation opcodes (0xF0-0xF4) ==========

        case S32_MMIO_OP_SVC_REQUEST: {
            // Request: service name in data buffer, length = name len (incl NUL)
            // Response: status in data buffer [0]=result, [4]=base_opcode, [8]=count, [12]=version
            if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            char svc_name[S32_SVC_MAX_NAME_LEN];
            memcpy(svc_name, mmio->data_buffer + offset, req->length);
            svc_name[req->length - 1] = '\0';

            // Policy check
            if (!mmio_policy_allows(mmio, svc_name)) {
                uint32_t svc_result = S32_SVC_DENIED;
                memcpy(mmio->data_buffer + offset, &svc_result, 4);
                resp.length = 4;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            // Check if already active
            for (int i = 0; i < mmio->num_services; i++) {
                if (mmio->services[i].active && strcmp(mmio->services[i].name, svc_name) == 0) {
                    uint32_t svc_result = S32_SVC_CONFLICT;
                    memcpy(mmio->data_buffer + offset, &svc_result, 4);
                    resp.length = 4;
                    resp.status = S32_MMIO_STATUS_OK;
                    break;
                }
            }

            // Find builtin service
            const builtin_service_t *builtin = find_builtin_service(svc_name);
            if (!builtin) {
                uint32_t svc_result = S32_SVC_UNKNOWN;
                memcpy(mmio->data_buffer + offset, &svc_result, 4);
                resp.length = 4;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            // Check session limit
            if (mmio->num_services >= S32_MAX_SERVICES) {
                uint32_t svc_result = S32_SVC_LIMIT;
                memcpy(mmio->data_buffer + offset, &svc_result, 4);
                resp.length = 4;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            // Allocate opcode range
            uint32_t base = mmio->next_dynamic_opcode;
            if (base + builtin->opcode_count > 0xF0) {
                uint32_t svc_result = S32_SVC_LIMIT;
                memcpy(mmio->data_buffer + offset, &svc_result, 4);
                resp.length = 4;
                resp.status = S32_MMIO_STATUS_OK;
                break;
            }

            // Create service state
            void *svc_state = builtin->create ? builtin->create() : NULL;

            // Register session
            svc_session_t *session = &mmio->services[mmio->num_services++];
            session->active = true;
            strncpy(session->name, svc_name, S32_MAX_SVC_NAME - 1);
            session->name[S32_MAX_SVC_NAME - 1] = '\0';
            session->base_opcode = base;
            session->opcode_count = builtin->opcode_count;
            session->version = builtin->version;
            session->state = svc_state;
            session->cleanup = builtin->cleanup;
            session->handle = builtin->handle;

            mmio->next_dynamic_opcode = base + builtin->opcode_count;

            // Write response: [0]=OK, [4]=base, [8]=count, [12]=version
            uint32_t reply[4];
            reply[0] = S32_SVC_OK;
            reply[1] = base;
            reply[2] = builtin->opcode_count;
            reply[3] = builtin->version;
            if (offset + 16 <= S32_MMIO_DATA_CAPACITY) {
                memcpy(mmio->data_buffer + offset, reply, 16);
                resp.length = 16;
            }
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_SVC_RELEASE: {
            // Request: service name in data buffer
            if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            char svc_name[S32_SVC_MAX_NAME_LEN];
            memcpy(svc_name, mmio->data_buffer + offset, req->length);
            svc_name[req->length - 1] = '\0';

            bool found = false;
            for (int i = 0; i < mmio->num_services; i++) {
                svc_session_t *svc = &mmio->services[i];
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
            resp.status = found ? S32_MMIO_STATUS_OK : S32_MMIO_STATUS_ERR;
            break;
        }

        case S32_MMIO_OP_SVC_QUERY: {
            // Request: service name in data buffer
            // Response: [0]=result code (OK if available, DENIED if policy blocks, UNKNOWN)
            if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                resp.status = S32_MMIO_STATUS_ERR;
                break;
            }
            char svc_name[S32_SVC_MAX_NAME_LEN];
            memcpy(svc_name, mmio->data_buffer + offset, req->length);
            svc_name[req->length - 1] = '\0';

            uint32_t svc_result;
            const builtin_service_t *builtin = find_builtin_service(svc_name);
            if (!builtin) {
                svc_result = S32_SVC_UNKNOWN;
            } else if (!mmio_policy_allows(mmio, svc_name)) {
                svc_result = S32_SVC_DENIED;
            } else {
                svc_result = S32_SVC_OK;
            }
            if (offset + 4 <= S32_MMIO_DATA_CAPACITY) {
                memcpy(mmio->data_buffer + offset, &svc_result, 4);
                resp.length = 4;
            }
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_SVC_LIST: {
            // Response: NUL-separated list of available service names
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t pos = 0;
            for (size_t i = 0; i < NUM_BUILTIN_SERVICES; i++) {
                size_t len = strlen(builtin_services[i].name) + 1;
                if (offset + pos + len > S32_MMIO_DATA_CAPACITY) break;
                memcpy(mmio->data_buffer + offset + pos, builtin_services[i].name, len);
                pos += len;
            }
            resp.length = pos;
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_SVC_VERSION: {
            // Response: protocol version in status field
            resp.status = S32_SVC_PROTOCOL_VERSION;
            break;
        }

        default: {
            // Check if opcode falls in a registered service range
            bool handled = false;
            for (int i = 0; i < mmio->num_services; i++) {
                svc_session_t *svc = &mmio->services[i];
                if (svc->active &&
                    req->opcode >= svc->base_opcode &&
                    req->opcode < svc->base_opcode + svc->opcode_count) {
                    uint32_t sub = req->opcode - svc->base_opcode;
                    svc->handle(svc->state, mmio, sub, req, &resp);
                    handled = true;
                    break;
                }
            }
            if (!handled) {
                resp.status = S32_MMIO_STATUS_ERR;
            }
            break;
        }
    }

write_response:
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
