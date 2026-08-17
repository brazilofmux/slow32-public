// SLOW-32 Ring Buffer MMIO Implementation
#include "mmio_ring.h"
#include "slow32.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
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
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>

#ifndef MSG_NOSIGNAL
#define MSG_NOSIGNAL 0
#endif
#include <sys/wait.h>
#include <limits.h>

static char g_emu_path[4096];

void mmio_ring_set_emulator(const char *argv0) {
    char resolved[4096];
    if (!argv0 || !argv0[0]) {
        return;
    }
    if (realpath(argv0, resolved) != NULL) {
        strncpy(g_emu_path, resolved, sizeof(g_emu_path) - 1);
        g_emu_path[sizeof(g_emu_path) - 1] = '\0';
        return;
    }
    strncpy(g_emu_path, argv0, sizeof(g_emu_path) - 1);
    g_emu_path[sizeof(g_emu_path) - 1] = '\0';
}

static int ends_with_ci(const char *s, const char *suf) {
    size_t n, m;
    const char *a, *b;
    if (!s || !suf) {
        return 0;
    }
    n = strlen(s);
    m = strlen(suf);
    if (n < m) {
        return 0;
    }
    a = s + n - m;
    b = suf;
    while (*b) {
        if (toupper((unsigned char)*a) != toupper((unsigned char)*b)) {
            return 0;
        }
        a++;
        b++;
    }
    return 1;
}

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


// Fail a request: status=ERR, length=positive errno for the guest.
static void mmio_fail(io_descriptor_t *resp, int err)
{
    resp->status = S32_MMIO_STATUS_ERR;
    if (err <= 0 || err >= 4096) {
        err = EIO;
    }
    resp->length = (uint32_t)err;
}

// Snapshot walk of guest RAM. Prefer the sparse callback; fall back to a
// flat map. Reads of the execute-only code window fail when a limit is set.
static int mmio_guest_read(mmio_ring_state_t *mmio, uint32_t addr,
                           void *dest, size_t size) {
    if (size == 0) {
        return 0;
    }
    if ((uint64_t)addr + size < addr) {
        return -1;
    }
    if (mmio->guest_code_limit != 0 && addr < mmio->guest_code_limit) {
        return -1;
    }
    if (mmio->guest_read) {
        return mmio->guest_read(mmio->guest_read_ctx, addr, dest, size);
    }
    if (mmio->guest_mem_base) {
        if ((uint64_t)addr + size > mmio->guest_mem_size) {
            return -1;
        }
        memcpy(dest, (uint8_t *)mmio->guest_mem_base + addr, size);
        return 0;
    }
    return -1;
}

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

static int parse_guest_sockaddr_in(mmio_ring_state_t *mmio,
                                   const io_descriptor_t *req,
                                   struct sockaddr_in *out) {
    if (req->length < sizeof(s32_mmio_sockaddr_in_t)) {
        return EINVAL;
    }
    uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
    if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_sockaddr_in_t))) {
        return EINVAL;
    }

    s32_mmio_sockaddr_in_t g;
    memcpy(&g, mmio->data_buffer + offset, sizeof(g));
    if (g.family != S32_AF_INET) {
        return EAFNOSUPPORT;
    }

    memset(out, 0, sizeof(*out));
    out->sin_family = AF_INET;
    out->sin_port = htons(g.port);
    out->sin_addr.s_addr = htonl(g.addr);
    return 0;
}

static void write_guest_sockaddr_in(mmio_ring_state_t *mmio, uint32_t offset,
                                    const struct sockaddr_in *in) {
    if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_sockaddr_in_t))) {
        return;
    }
    s32_mmio_sockaddr_in_t g;
    g.addr = ntohl(in->sin_addr.s_addr);
    g.port = ntohs(in->sin_port);
    g.family = S32_AF_INET;
    memcpy(mmio->data_buffer + offset, &g, sizeof(g));
}

static int guest_socket_fd(mmio_ring_state_t *mmio, int host_fd) {
    int guest_fd = alloc_guest_fd(mmio, host_fd, true);
    if (guest_fd < 0) {
        close(host_fd);
        return -1;
    }
    mmio->fd_types[guest_fd] = S32_FD_TYPE_SOCK;
    return guest_fd;
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

#define TERM_MAX_ROWS 256
#define TERM_MAX_COLS 256
#define TERM_MAX_SAVE_DEPTH 8

typedef struct {
    uint8_t ch;
    uint8_t attr;   // 0=normal, 1=bold, 7=reverse, etc.
    uint8_t fg;     // ANSI 0-7
    uint8_t bg;     // ANSI 0-7
} term_cell_t;

typedef struct {
    term_cell_t *cells;    // rows * cols cells
    int rows, cols;
    int cur_row, cur_col;  // 0-based
    int cur_attr;
    int cur_fg, cur_bg;
} term_screen_save_t;

typedef struct {
    struct termios saved_termios;
    bool raw_mode;
    bool termios_saved;
    // Shadow screen buffer
    int rows, cols;
    int cur_row, cur_col;  // 0-based
    int cur_attr;
    int cur_fg, cur_bg;
    term_cell_t *cells;    // rows * cols, heap-allocated
    // Save stack
    term_screen_save_t save_stack[TERM_MAX_SAVE_DEPTH];
    int save_depth;
    // Buffered update state (for begin/end update diffing)
    bool in_update;
    term_cell_t *prev_cells;   // snapshot taken at begin_update
    int prev_cur_row, prev_cur_col;
    int prev_cur_attr, prev_cur_fg, prev_cur_bg;
} term_state_t;

static void *term_create(void) {
    term_state_t *ts = calloc(1, sizeof(term_state_t));
    if (!ts) return NULL;
    if (isatty(STDIN_FILENO) && tcgetattr(STDIN_FILENO, &ts->saved_termios) == 0) {
        ts->termios_saved = true;
    }
    // Initialize shadow screen buffer
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1) {
        ts->rows = 24;
        ts->cols = 80;
    } else {
        ts->rows = (ws.ws_row > TERM_MAX_ROWS) ? TERM_MAX_ROWS : ws.ws_row;
        ts->cols = (ws.ws_col > TERM_MAX_COLS) ? TERM_MAX_COLS : ws.ws_col;
    }
    if (ts->rows < 1) ts->rows = 24;
    if (ts->cols < 1) ts->cols = 80;
    ts->cells = calloc((size_t)ts->rows * ts->cols, sizeof(term_cell_t));
    if (ts->cells) {
        // Fill with spaces
        for (int i = 0; i < ts->rows * ts->cols; i++) {
            ts->cells[i].ch = ' ';
            ts->cells[i].fg = 7;  // default white
        }
    }
    ts->cur_fg = 7;
    ts->cur_bg = 0;
    ts->save_depth = 0;
    return ts;
}

static void term_cleanup(void *state) {
    term_state_t *ts = (term_state_t *)state;
    if (!ts) return;
    if (ts->raw_mode && ts->termios_saved) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &ts->saved_termios);
    }
    for (int i = 0; i < ts->save_depth; i++) {
        free(ts->save_stack[i].cells);
    }
    free(ts->prev_cells);
    free(ts->cells);
    free(ts);
}

// Shadow buffer helpers
static inline term_cell_t *term_cell_at(term_state_t *ts, int row, int col) {
    if (!ts->cells || row < 0 || row >= ts->rows || col < 0 || col >= ts->cols)
        return NULL;
    return &ts->cells[row * ts->cols + col];
}

static void term_shadow_putc(term_state_t *ts, int ch) {
    if (!ts->cells) return;
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
    if (ch < 0x20) return;  // skip other control chars
    term_cell_t *c = term_cell_at(ts, ts->cur_row, ts->cur_col);
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

static void term_shadow_clear(term_state_t *ts, int mode) {
    if (!ts->cells) return;
    int start, end;
    switch (mode) {
        case 0: // full screen
            start = 0;
            end = ts->rows * ts->cols;
            ts->cur_row = 0;
            ts->cur_col = 0;
            break;
        case 1: // to end of line
            start = ts->cur_row * ts->cols + ts->cur_col;
            end = (ts->cur_row + 1) * ts->cols;
            break;
        case 2: // to end of screen
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
    if (start < 0) start = 0;
    if (end > ts->rows * ts->cols) end = ts->rows * ts->cols;
    for (int i = start; i < end; i++) {
        ts->cells[i].ch = ' ';
        ts->cells[i].attr = 0;
        ts->cells[i].fg = 7;
        ts->cells[i].bg = 0;
    }
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
                mmio_fail(resp, EINVAL);
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
                mmio_fail(resp, EINVAL);
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
            if (!ts->in_update) {
                fprintf(stdout, "\033[%u;%uH", row, col);
                fflush(stdout);
            }
            ts->cur_row = (int)row - 1;  // shadow: 0-based
            ts->cur_col = (int)col - 1;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_CLEAR: {
            // status: 0 = full screen, 1 = to end of line, 2 = to end of screen
            if (!ts->in_update) {
                switch (req->status) {
                    case 0: fprintf(stdout, "\033[2J\033[H"); break;
                    case 1: fprintf(stdout, "\033[K"); break;
                    case 2: fprintf(stdout, "\033[J"); break;
                    default: fprintf(stdout, "\033[2J\033[H"); break;
                }
                fflush(stdout);
            }
            term_shadow_clear(ts, (int)req->status);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_SET_ATTR: {
            // status: 0 = normal, 1 = bold, 7 = reverse
            if (!ts->in_update) {
                fprintf(stdout, "\033[%um", req->status);
                fflush(stdout);
            }
            ts->cur_attr = (int)req->status;
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
            if (!ts->in_update) {
                fprintf(stdout, "\033[3%u;4%um", fg, bg);
                fflush(stdout);
            }
            ts->cur_fg = (int)fg;
            ts->cur_bg = (int)bg;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_PUTC: {
            int ch = (int)(req->status & 0xFF);
            if (!ts->in_update) {
                fputc(ch, stdout);
                fflush(stdout);
            }
            term_shadow_putc(ts, ch);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_PUTS: {
            uint32_t off = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t len = req->length;
            if (len > S32_MMIO_DATA_CAPACITY - off)
                len = S32_MMIO_DATA_CAPACITY - off;
            if (len > 0) {
                if (!ts->in_update)
                    fwrite(mmio->data_buffer + off, 1, len, stdout);
                for (uint32_t i = 0; i < len; i++)
                    term_shadow_putc(ts, mmio->data_buffer[off + i]);
            }
            if (!ts->in_update)
                fflush(stdout);
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_SAVE_SCREEN: {
            if (!ts->cells || ts->save_depth >= TERM_MAX_SAVE_DEPTH) {
                mmio_fail(resp, EINVAL);
                break;
            }
            size_t ncells = (size_t)ts->rows * ts->cols;
            term_cell_t *snap = malloc(ncells * sizeof(term_cell_t));
            if (!snap) {
                mmio_fail(resp, EINVAL);
                break;
            }
            memcpy(snap, ts->cells, ncells * sizeof(term_cell_t));
            term_screen_save_t *s = &ts->save_stack[ts->save_depth++];
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
                mmio_fail(resp, EINVAL);
                break;
            }
            term_screen_save_t *s = &ts->save_stack[--ts->save_depth];
            // Repaint: clear screen, then redraw all cells
            fprintf(stdout, "\033[0m\033[2J\033[H");
            int prev_attr = 0, prev_fg = 7, prev_bg = 0;
            int paint_rows = (s->rows < ts->rows) ? s->rows : ts->rows;
            int paint_cols = (s->cols < ts->cols) ? s->cols : ts->cols;
            for (int r = 0; r < paint_rows; r++) {
                // Move to start of row
                fprintf(stdout, "\033[%d;1H", r + 1);
                int last_written_col = -1;
                for (int c = 0; c < paint_cols; c++) {
                    term_cell_t *cell = &s->cells[r * s->cols + c];
                    // Skip trailing spaces with default attributes
                    if (cell->ch == ' ' && cell->attr == 0 &&
                        cell->fg == 7 && cell->bg == 0)
                        continue;
                    // Position cursor if we skipped columns
                    if (c != last_written_col + 1)
                        fprintf(stdout, "\033[%d;%dH", r + 1, c + 1);
                    // Set attributes if changed
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
            // Restore shadow buffer from saved state
            size_t ncells = (size_t)ts->rows * ts->cols;
            if (s->rows == ts->rows && s->cols == ts->cols) {
                memcpy(ts->cells, s->cells, ncells * sizeof(term_cell_t));
            } else {
                // Dimension mismatch: clear and copy what fits
                for (size_t i = 0; i < ncells; i++) {
                    ts->cells[i].ch = ' ';
                    ts->cells[i].attr = 0;
                    ts->cells[i].fg = 7;
                    ts->cells[i].bg = 0;
                }
                for (int r = 0; r < paint_rows; r++)
                    memcpy(&ts->cells[r * ts->cols],
                           &s->cells[r * s->cols],
                           (size_t)paint_cols * sizeof(term_cell_t));
            }
            ts->cur_row = s->cur_row;
            ts->cur_col = s->cur_col;
            ts->cur_attr = s->cur_attr;
            ts->cur_fg = s->cur_fg;
            ts->cur_bg = s->cur_bg;
            // Restore cursor position and attributes on terminal
            fprintf(stdout, "\033[%um", (unsigned)ts->cur_attr);
            fprintf(stdout, "\033[3%u;4%um",
                    (unsigned)ts->cur_fg, (unsigned)ts->cur_bg);
            fprintf(stdout, "\033[%d;%dH",
                    ts->cur_row + 1, ts->cur_col + 1);
            fflush(stdout);
            free(s->cells);
            s->cells = NULL;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_BEGIN_UPDATE: {
            if (ts->in_update || !ts->cells) {
                mmio_fail(resp, EINVAL);
                break;
            }
            // Snapshot current shadow buffer
            size_t ncells = (size_t)ts->rows * ts->cols;
            ts->prev_cells = malloc(ncells * sizeof(term_cell_t));
            if (!ts->prev_cells) {
                mmio_fail(resp, EINVAL);
                break;
            }
            memcpy(ts->prev_cells, ts->cells, ncells * sizeof(term_cell_t));
            ts->prev_cur_row = ts->cur_row;
            ts->prev_cur_col = ts->cur_col;
            ts->prev_cur_attr = ts->cur_attr;
            ts->prev_cur_fg = ts->cur_fg;
            ts->prev_cur_bg = ts->cur_bg;
            ts->in_update = true;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        case S32_TERM_END_UPDATE: {
            if (!ts->in_update || !ts->prev_cells) {
                mmio_fail(resp, EINVAL);
                break;
            }
            ts->in_update = false;
            // Diff prev_cells vs cells, emit minimum ANSI
            int out_attr = ts->prev_cur_attr;
            int out_fg = ts->prev_cur_fg;
            int out_bg = ts->prev_cur_bg;
            int out_row = ts->prev_cur_row;
            int out_col = ts->prev_cur_col;
            for (int r = 0; r < ts->rows; r++) {
                for (int c = 0; c < ts->cols; c++) {
                    int idx = r * ts->cols + c;
                    term_cell_t *prev = &ts->prev_cells[idx];
                    term_cell_t *cur = &ts->cells[idx];
                    if (cur->ch == prev->ch && cur->attr == prev->attr &&
                        cur->fg == prev->fg && cur->bg == prev->bg)
                        continue;
                    // Position cursor if needed
                    if (r != out_row || c != out_col) {
                        fprintf(stdout, "\033[%d;%dH", r + 1, c + 1);
                        out_row = r;
                        out_col = c;
                    }
                    // Set attributes if changed
                    if (cur->attr != out_attr) {
                        fprintf(stdout, "\033[%um", (unsigned)cur->attr);
                        out_attr = cur->attr;
                        // Reset colors after attr change (attr 0 resets everything)
                        if (cur->attr == 0) { out_fg = 7; out_bg = 0; }
                    }
                    if (cur->fg != out_fg || cur->bg != out_bg) {
                        fprintf(stdout, "\033[3%u;4%um",
                                (unsigned)cur->fg, (unsigned)cur->bg);
                        out_fg = cur->fg;
                        out_bg = cur->bg;
                    }
                    fputc(cur->ch, stdout);
                    out_col++;
                    if (out_col >= ts->cols) {
                        out_col = 0;
                        out_row++;
                    }
                }
            }
            // Restore final cursor position and attributes
            if (ts->cur_attr != out_attr) {
                fprintf(stdout, "\033[%um", (unsigned)ts->cur_attr);
            }
            if (ts->cur_fg != out_fg || ts->cur_bg != out_bg) {
                fprintf(stdout, "\033[3%u;4%um",
                        (unsigned)ts->cur_fg, (unsigned)ts->cur_bg);
            }
            fprintf(stdout, "\033[%d;%dH",
                    ts->cur_row + 1, ts->cur_col + 1);
            fflush(stdout);
            free(ts->prev_cells);
            ts->prev_cells = NULL;
            resp->status = S32_MMIO_STATUS_OK;
            break;
        }
        default:
            mmio_fail(resp, EINVAL);
            break;
    }
}

// ========== Tube service (vec only; viewer socket later) ==========

#define TUBE_KEY_QUEUE S32_TUBE_KEY_QUEUE

typedef struct {
    char kind;          /* 'M', 'D', 'P' */
    uint16_t x0, y0;    /* beam start (D) or equal to x,y (P) */
    uint16_t x, y;
    uint8_t r, g, b, i;
} tube_elem_t;

typedef struct {
    uint16_t code;
    uint8_t down;
    uint8_t reserved;
} tube_key_t;

#define TUBE_TAG_HELO 0x4F4C4548u
#define TUBE_TAG_VSEG 0x47455356u
#define TUBE_TAG_KEYE 0x4559454Bu
#define TUBE_TAG_BYE  0x00455942u
#define TUBE_RBUF     64

typedef struct {
    int mode;                   /* 0 = none, else S32_TUBE_MODE_* */
    uint32_t frames;
    uint32_t generation;
    int have_snap;
    tube_elem_t *elems;
    uint32_t nelems;
    char *dump_dir;
    int dump_full;
    uint32_t dump_index;
    tube_key_t keys[TUBE_KEY_QUEUE];
    uint32_t key_head;
    uint32_t key_count;
    int listen_fd;
    int view_fd;
    char port_path[4096];
    int port_written;
    uint8_t rbuf[TUBE_RBUF];
    size_t rfill;
} tube_state_t;

static uint64_t tube_fnv1a64(const uint8_t *p, size_t n) {
    uint64_t h = 14695981039346656037ULL;
    size_t i;
    for (i = 0; i < n; i++) {
        h ^= p[i];
        h *= 1099511628211ULL;
    }
    return h;
}

static void tube_key_push(tube_state_t *ts, tube_key_t ev) {
    if (ts->key_count == TUBE_KEY_QUEUE) {
        ts->key_head = (ts->key_head + 1u) % TUBE_KEY_QUEUE;
        ts->key_count--;
    }
    uint32_t tail = (ts->key_head + ts->key_count) % TUBE_KEY_QUEUE;
    ts->keys[tail] = ev;
    ts->key_count++;
}

static void tube_preload_keys(tube_state_t *ts) {
    const char *path = getenv("S32_TUBE_KEYS");
    FILE *f;
    tube_key_t ev;
    if (!path || !path[0]) {
        return;
    }
    f = fopen(path, "rb");
    if (!f) {
        return;
    }
    while (fread(&ev, sizeof(ev), 1, f) == 1) {
        tube_key_push(ts, ev);
    }
    fclose(f);
}

static char *tube_canonical_vec(const tube_state_t *ts, size_t *out_len) {
    size_t cap, pos, i;
    char *buf;

    if (ts->nelems == 0) {
        char *empty = (char *)malloc(1);
        if (empty) {
            empty[0] = '\0';
        }
        *out_len = 0;
        return empty;
    }

    cap = (size_t)ts->nelems * 40u + 1u;
    buf = (char *)malloc(cap);
    if (!buf) {
        return NULL;
    }
    pos = 0;
    for (i = 0; i < ts->nelems; i++) {
        const tube_elem_t *e = &ts->elems[i];
        int n;
        if (e->kind == 'M') {
            n = snprintf(buf + pos, cap - pos, "M %u %u\n", e->x, e->y);
        } else {
            n = snprintf(buf + pos, cap - pos, "%c %u %u %u %u %u %u\n",
                         e->kind, e->x, e->y, e->r, e->g, e->b, e->i);
        }
        if (n < 0 || (size_t)n >= cap - pos) {
            free(buf);
            return NULL;
        }
        pos += (size_t)n;
    }
    *out_len = pos;
    return buf;
}

static void tube_dump_vec(tube_state_t *ts) {
    size_t len = 0;
    char *text;
    uint64_t h;
    char path[4096];
    FILE *f;

    if (!ts->dump_dir) {
        return;
    }
    text = tube_canonical_vec(ts, &len);
    if (!text) {
        return;
    }
    h = tube_fnv1a64((const uint8_t *)text, len);

    snprintf(path, sizeof(path), "%s/%06u.hash", ts->dump_dir, ts->dump_index);
    f = fopen(path, "w");
    if (f) {
        fprintf(f, "%016llx\n", (unsigned long long)h);
        fclose(f);
    }
    if (ts->dump_full) {
        snprintf(path, sizeof(path), "%s/%06u.txt", ts->dump_dir, ts->dump_index);
        f = fopen(path, "w");
        if (f) {
            if (len > 0) {
                fwrite(text, 1, len, f);
            }
            fclose(f);
        }
    }
    free(text);
    ts->dump_index++;
}

static int tube_walk_vec(tube_state_t *ts, mmio_ring_state_t *mmio,
                         uint32_t base, uint32_t nwords) {
    uint32_t bx = 0, by = 0, inten = 255, color = 0xFFFFFFu;
    uint32_t i, nelems = 0;
    int saw_end = 0;
    tube_elem_t *elems;

    if (nwords == 0 || nwords > S32_TUBE_LIST_MAX_WORDS) {
        return EINVAL;
    }
    if (base & 3u) {
        return EINVAL;
    }

    elems = (tube_elem_t *)malloc((size_t)nwords * sizeof(tube_elem_t));
    if (!elems) {
        return ENOMEM;
    }

    for (i = 0; i < nwords; i++) {
        uint32_t w, op, x, y;
        if (mmio_guest_read(mmio, base + i * 4u, &w, 4) != 0) {
            free(elems);
            return EINVAL;
        }
        op = (w >> 28) & 0xFu;
        x = (w >> 16) & 0xFFFu;
        y = (w >> 4) & 0xFFFu;
        if (op == S32_TUBE_VOP_END) {
            saw_end = 1;
            break;
        }
        switch (op) {
            case S32_TUBE_VOP_MOVE:
                elems[nelems].kind = 'M';
                elems[nelems].x0 = (uint16_t)x;
                elems[nelems].y0 = (uint16_t)y;
                elems[nelems].x = (uint16_t)x;
                elems[nelems].y = (uint16_t)y;
                elems[nelems].r = elems[nelems].g = elems[nelems].b = 0;
                elems[nelems].i = 0;
                nelems++;
                bx = x;
                by = y;
                break;
            case S32_TUBE_VOP_DRAW:
                elems[nelems].kind = 'D';
                elems[nelems].x0 = (uint16_t)bx;
                elems[nelems].y0 = (uint16_t)by;
                elems[nelems].x = (uint16_t)x;
                elems[nelems].y = (uint16_t)y;
                elems[nelems].r = (uint8_t)((color >> 16) & 0xFFu);
                elems[nelems].g = (uint8_t)((color >> 8) & 0xFFu);
                elems[nelems].b = (uint8_t)(color & 0xFFu);
                elems[nelems].i = (uint8_t)inten;
                nelems++;
                bx = x;
                by = y;
                break;
            case S32_TUBE_VOP_POINT:
                elems[nelems].kind = 'P';
                elems[nelems].x0 = (uint16_t)x;
                elems[nelems].y0 = (uint16_t)y;
                elems[nelems].x = (uint16_t)x;
                elems[nelems].y = (uint16_t)y;
                elems[nelems].r = (uint8_t)((color >> 16) & 0xFFu);
                elems[nelems].g = (uint8_t)((color >> 8) & 0xFFu);
                elems[nelems].b = (uint8_t)(color & 0xFFu);
                elems[nelems].i = (uint8_t)inten;
                nelems++;
                break;
            case S32_TUBE_VOP_INTEN:
                inten = w & 0xFFu;
                break;
            case S32_TUBE_VOP_COLOR:
                color = w & 0xFFFFFFu;
                break;
            default:
                free(elems);
                return EINVAL;
        }
    }

    if (!saw_end) {
        free(elems);
        return EINVAL;
    }

    free(ts->elems);
    ts->elems = elems;
    ts->nelems = nelems;
    return 0;
}

static void tube_set_nb(int fd) {
    int flags;
    if (fd < 0) {
        return;
    }
    flags = fcntl(fd, F_GETFL, 0);
    if (flags >= 0) {
        fcntl(fd, F_SETFL, flags | O_NONBLOCK);
    }
#ifdef SO_NOSIGPIPE
    {
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof(one));
    }
#endif
    {
        int one = 1;
        setsockopt(fd, IPPROTO_TCP, TCP_NODELAY, &one, sizeof(one));
    }
}

static void tube_close_view(tube_state_t *ts) {
    if (ts->view_fd >= 0) {
        close(ts->view_fd);
        ts->view_fd = -1;
    }
    ts->rfill = 0;
}

static void tube_unlisten(tube_state_t *ts) {
    tube_close_view(ts);
    if (ts->listen_fd >= 0) {
        close(ts->listen_fd);
        ts->listen_fd = -1;
    }
    if (ts->port_written && ts->port_path[0]) {
        unlink(ts->port_path);
        ts->port_written = 0;
    }
}

static int tube_send_all(int fd, const void *buf, size_t n) {
    const uint8_t *p = (const uint8_t *)buf;
    int any = 0;
    while (n > 0) {
        ssize_t w = send(fd, p, n, MSG_NOSIGNAL);
        if (w < 0) {
            if (errno == EINTR) {
                continue;
            }
            if ((errno == EAGAIN || errno == EWOULDBLOCK) && !any) {
                return -2; /* drop whole frame */
            }
            return -1;
        }
        if (w == 0) {
            return -1;
        }
        any = 1;
        p += (size_t)w;
        n -= (size_t)w;
    }
    return 0;
}

static int tube_send_frame(int fd, uint32_t tag, const void *payload, uint32_t plen) {
    uint32_t hdr[2];
    uint8_t *pkt;
    uint32_t length = 4u + plen;
    int rc;

    pkt = (uint8_t *)malloc(8u + plen);
    if (!pkt) {
        return -1;
    }
    hdr[0] = length;
    hdr[1] = tag;
    memcpy(pkt, hdr, 8);
    if (plen && payload) {
        memcpy(pkt + 8, payload, plen);
    }
    rc = tube_send_all(fd, pkt, 8u + plen);
    free(pkt);
    return rc;
}

static void tube_send_helo(tube_state_t *ts) {
    uint32_t pl[4];
    int rc;
    if (ts->view_fd < 0) {
        return;
    }
    pl[0] = 1;
    pl[1] = (uint32_t)ts->mode;
    pl[2] = 4096;
    pl[3] = 4096;
    rc = tube_send_frame(ts->view_fd, TUBE_TAG_HELO, pl, sizeof(pl));
    if (rc == -1) {
        tube_close_view(ts);
    }
}

static void tube_send_vseg(tube_state_t *ts) {
    uint32_t count = 0, i, o;
    uint8_t *pl;
    int rc;

    if (ts->view_fd < 0 || !ts->have_snap) {
        return;
    }
    for (i = 0; i < ts->nelems; i++) {
        if (ts->elems[i].kind == 'D' || ts->elems[i].kind == 'P') {
            count++;
        }
    }
    pl = (uint8_t *)malloc(8u + count * 12u);
    if (!pl) {
        return;
    }
    memcpy(pl + 0, &ts->generation, 4);
    memcpy(pl + 4, &count, 4);
    o = 8;
    for (i = 0; i < ts->nelems; i++) {
        const tube_elem_t *e = &ts->elems[i];
        uint16_t xy[4];
        if (e->kind != 'D' && e->kind != 'P') {
            continue;
        }
        xy[0] = e->x0;
        xy[1] = e->y0;
        xy[2] = e->x;
        xy[3] = e->y;
        memcpy(pl + o, xy, 8);
        pl[o + 8] = e->r;
        pl[o + 9] = e->g;
        pl[o + 10] = e->b;
        pl[o + 11] = e->i;
        o += 12;
    }
    rc = tube_send_frame(ts->view_fd, TUBE_TAG_VSEG, pl, 8u + count * 12u);
    free(pl);
    if (rc == -1) {
        tube_close_view(ts);
    }
}

static void tube_recv(tube_state_t *ts) {
    if (ts->view_fd < 0) {
        return;
    }
    for (;;) {
        ssize_t n;
        uint32_t length, tag;

        /* Parse buffered frames before recv. EAGAIN must not
           abandon a KEYE burst that already arrived in rbuf. */
        while (ts->rfill >= 8) {
            memcpy(&length, ts->rbuf, 4);
            memcpy(&tag, ts->rbuf + 4, 4);
            if (length < 4 || length > 16) {
                tube_close_view(ts);
                return;
            }
            if (ts->rfill < 4u + length) {
                break;
            }
            if (tag == TUBE_TAG_KEYE && length >= 8) {
                tube_key_t ev;
                memcpy(&ev, ts->rbuf + 8, 4);
                tube_key_push(ts, ev);
            } else if (tag == TUBE_TAG_BYE) {
                tube_close_view(ts);
                return;
            }
            {
                size_t used = 4u + length;
                memmove(ts->rbuf, ts->rbuf + used, ts->rfill - used);
                ts->rfill -= used;
            }
        }

        if (ts->rfill >= TUBE_RBUF) {
            tube_close_view(ts);
            return;
        }
        n = recv(ts->view_fd, ts->rbuf + ts->rfill, TUBE_RBUF - ts->rfill, 0);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            if (errno == EAGAIN || errno == EWOULDBLOCK) {
                return;
            }
            tube_close_view(ts);
            return;
        }
        if (n == 0) {
            tube_close_view(ts);
            return;
        }
        ts->rfill += (size_t)n;
    }
}

static void tube_pump(tube_state_t *ts) {
    if (ts->listen_fd >= 0) {
        for (;;) {
            struct sockaddr_in addr;
            socklen_t alen = sizeof(addr);
            int cfd = accept(ts->listen_fd, (struct sockaddr *)&addr, &alen);
            if (cfd < 0) {
                break;
            }
            tube_set_nb(cfd);
            tube_close_view(ts);
            ts->view_fd = cfd;
            ts->rfill = 0;
            tube_send_helo(ts);
            tube_send_vseg(ts);
        }
    }
    tube_recv(ts);
}

static void tube_listen(tube_state_t *ts) {
    struct sockaddr_in addr;
    socklen_t alen;
    const char *path;
    FILE *f;
    int fd, port;

    if (ts->listen_fd >= 0) {
        return;
    }
    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        return;
    }
    {
        int one = 1;
        setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &one, sizeof(one));
    }
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons(0);
    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
        listen(fd, 1) < 0) {
        close(fd);
        return;
    }
    tube_set_nb(fd);
    alen = sizeof(addr);
    if (getsockname(fd, (struct sockaddr *)&addr, &alen) < 0) {
        close(fd);
        return;
    }
    port = (int)ntohs(addr.sin_port);
    path = getenv("S32_TUBE_PORT");
    if (!path || !path[0]) {
        path = "tube.port";
    }
    strncpy(ts->port_path, path, sizeof(ts->port_path) - 1);
    ts->port_path[sizeof(ts->port_path) - 1] = '\0';
    f = fopen(ts->port_path, "w");
    if (!f) {
        close(fd);
        return;
    }
    fprintf(f, "%u\n", (unsigned)port);
    fclose(f);
    ts->port_written = 1;
    ts->listen_fd = fd;
}

static void *tube_create(void) {
    tube_state_t *ts = (tube_state_t *)calloc(1, sizeof(tube_state_t));
    const char *dir;
    if (!ts) {
        return NULL;
    }
    ts->listen_fd = -1;
    ts->view_fd = -1;
    dir = getenv("S32_TUBE_DUMP");
    if (dir && dir[0]) {
        if (mkdir(dir, 0777) < 0 && errno != EEXIST) {
            /* Headless dump is best-effort; OPEN still succeeds. */
        } else {
            ts->dump_dir = strdup(dir);
        }
        if (getenv("S32_TUBE_DUMP_FULL")) {
            ts->dump_full = 1;
        }
    }
    return ts;
}

static void tube_cleanup(void *state) {
    tube_state_t *ts = (tube_state_t *)state;
    if (!ts) {
        return;
    }
    tube_unlisten(ts);
    free(ts->elems);
    free(ts->dump_dir);
    free(ts);
}

static uint32_t tube_info_status(const tube_state_t *ts) {
    uint32_t st = (1u << (S32_TUBE_MODE_VEC - 1)); /* vec implemented */
    if (ts->view_fd >= 0) {
        st |= (1u << 8);
    }
    st |= (1u << 16); /* version 1 */
    return st;
}

static void tube_handle(void *state, mmio_ring_state_t *mmio,
                        uint32_t sub_opcode, io_descriptor_t *req,
                        io_descriptor_t *resp) {
    tube_state_t *ts = (tube_state_t *)state;
    if (!ts) {
        mmio_fail(resp, EIO);
        return;
    }

    tube_pump(ts);

    switch (sub_opcode) {
        case S32_TUBE_INFO:
            resp->status = tube_info_status(ts);
            resp->length = 0;
            break;

        case S32_TUBE_OPEN: {
            uint32_t mode = req->status;
            if (ts->mode != 0) {
                mmio_fail(resp, EINVAL);
                break;
            }
            if (mode != S32_TUBE_MODE_VEC) {
                mmio_fail(resp, EINVAL);
                break;
            }
            if (req->length != 0) {
                mmio_fail(resp, EINVAL);
                break;
            }
            ts->mode = (int)mode;
            tube_preload_keys(ts);
            tube_listen(ts);
            tube_pump(ts);
            resp->status = S32_MMIO_STATUS_OK;
            resp->length = 0;
            break;
        }

        case S32_TUBE_CLOSE:
            tube_unlisten(ts);
            free(ts->elems);
            ts->elems = NULL;
            ts->nelems = 0;
            ts->have_snap = 0;
            ts->mode = 0;
            ts->key_head = 0;
            ts->key_count = 0;
            resp->status = S32_MMIO_STATUS_OK;
            resp->length = 0;
            break;

        case S32_TUBE_PRESENT: {
            int err;
            if (ts->mode != S32_TUBE_MODE_VEC) {
                mmio_fail(resp, EINVAL);
                break;
            }
            err = tube_walk_vec(ts, mmio, req->status, req->length);
            if (err != 0) {
                mmio_fail(resp, err);
                break;
            }
            tube_dump_vec(ts);
            ts->generation = req->offset;
            ts->have_snap = 1;
            ts->frames = (ts->frames + 1u) & 0xFFFFFFu;
            tube_send_vseg(ts);
            resp->status = S32_MMIO_STATUS_OK;
            resp->length = 0;
            break;
        }

        case S32_TUBE_STATUS:
            resp->status = (ts->frames & 0xFFFFFFu);
            if (ts->view_fd >= 0) {
                resp->status |= (1u << 31);
            }
            resp->length = 0;
            break;

        case S32_TUBE_KEYS: {
            uint32_t offset, nbytes, nfit, ncopy, i;
            if (req->length % 4u != 0) {
                mmio_fail(resp, EINVAL);
                break;
            }
            offset = req->offset % S32_MMIO_DATA_CAPACITY;
            nbytes = req->length;
            if (offset + nbytes > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(resp, EINVAL);
                break;
            }
            nfit = nbytes / 4u;
            ncopy = nfit < ts->key_count ? nfit : ts->key_count;
            for (i = 0; i < ncopy; i++) {
                tube_key_t ev = ts->keys[ts->key_head];
                ts->key_head = (ts->key_head + 1u) % TUBE_KEY_QUEUE;
                ts->key_count--;
                memcpy(mmio->data_buffer + offset + i * 4u, &ev, 4);
            }
            resp->status = ncopy;
            resp->length = ncopy * 4u;
            break;
        }

        default:
            mmio_fail(resp, EINVAL);
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
    {
        .name = "tube",
        .opcode_count = S32_TUBE_OPCODE_COUNT,
        .version = 1,
        .create = tube_create,
        .cleanup = tube_cleanup,
        .handle = tube_handle,
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
    if (opcode == 0x0D) return "fs";   // FTRUNCATE
    if (opcode >= 0x20 && opcode <= 0x2B) return "fs";  // FS metadata (through REWINDDIR)
    if (opcode >= 0x30 && opcode <= 0x3F) return "time";
    if (opcode == 0x10) return "exec";
    if (opcode >= 0x40 && opcode <= 0x4F) return "net";
    if (opcode >= 0x60 && opcode <= 0x6F) return "env";
    // 0x01 (PUTCHAR), 0x02 (GETCHAR), 0x09 (EXIT) always allowed
    return NULL;
}

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio) {
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
                rel + 4 <= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
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
                     rel + 4 <= S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY) {
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
        mmio_fail(&resp, EINVAL);
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
        
        case S32_MMIO_OP_SEND:
        case S32_MMIO_OP_WRITE: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;
            uint32_t to_write = req->length;

            if (host_fd < 0 || to_write == 0 || to_write > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, host_fd < 0 ? EBADF : EINVAL);
                break;
            }

            if (to_write > max_bytes) {
                to_write = max_bytes;
            }

            ssize_t written = write(host_fd, mmio->data_buffer + offset, to_write);
            if (written < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }

            resp.length = (uint32_t)written;
            resp.status = (uint32_t)written;
            break;
        }

        case S32_MMIO_OP_RECV:
        case S32_MMIO_OP_READ: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;
            uint32_t to_read = req->length;

            if (host_fd < 0 || to_read == 0 || to_read > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, host_fd < 0 ? EBADF : EINVAL);
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
                mmio_fail(&resp, errno > 0 ? errno : EIO);
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
                mmio_fail(&resp, EINVAL);
                if (trace_io_enabled) {
                     fprintf(stderr, "[MMIO] READ_DIRECT failed: guest memory not configured\n");
                }
                break;
            }

            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t guest_addr = req->offset;
            uint32_t count = req->length;

            // Validate guest address range
            if (guest_addr >= mmio->guest_mem_size || 
                (uint64_t)guest_addr + count > mmio->guest_mem_size) {
                mmio_fail(&resp, EINVAL);
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] READ_DIRECT bounds check failed: addr=0x%08X len=%u size=0x%08X\n",
                            guest_addr, count, mmio->guest_mem_size);
                }
                break;
            }

            void *dest = (uint8_t *)mmio->guest_mem_base + guest_addr;
            
            ssize_t read_count = read(host_fd, dest, count);
            
            if (read_count < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1u);
            if (!path) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                if (trace_io_enabled) {
                    fprintf(stderr, "[MMIO] OPEN failed (flags=0x%x errno=%d)\n",
                            req->status, errno);
                }
                break;
            }

            int guest_fd = alloc_guest_fd(mmio, host_fd, true);
            if (guest_fd < 0) {
                close(host_fd);
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            int host_fd = mmio->host_fds[guest_fd];
            int rc = 0;
            if (mmio->host_fd_owned[guest_fd]) {
                rc = close(host_fd);
            }

            mmio->host_fds[guest_fd] = -1;
            mmio->host_fd_owned[guest_fd] = false;

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_SEEK: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0 || req->length < 8u) {
                mmio_fail(&resp, host_fd < 0 ? EBADF : EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - 8u)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint8_t whence_raw = mmio->data_buffer[offset];
            int32_t distance = 0;
            memcpy(&distance, mmio->data_buffer + offset + 4u, sizeof(int32_t));

            off_t new_pos = lseek(host_fd, (off_t)distance, (int)whence_raw);
            if (new_pos == (off_t)-1) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            resp.status = (uint32_t)new_pos;
            resp.length = 0;
            break;
        }
        
        case S32_MMIO_OP_FTRUNCATE: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0 || req->length < 4u) {
                mmio_fail(&resp, host_fd < 0 ? EBADF : EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - 4u)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t new_length = 0;
            memcpy(&new_length, mmio->data_buffer + offset, sizeof(uint32_t));

            int rc = ftruncate(host_fd, (off_t)new_length);
            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
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

        case S32_MMIO_OP_EXEC: {
            char blob[4096];
            char *path;
            char *extra[12];
            int nextra = 0;
            char *p;
            char *end;
            const char *emu;
            pid_t pid;
            int st = 0;
            struct stat sb;
            uint32_t n;
            uint32_t off;

            if (req->length == 0 || req->length >= sizeof(blob)) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            off = req->offset % S32_MMIO_DATA_CAPACITY;
            if (off > S32_MMIO_DATA_CAPACITY - req->length) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(blob, mmio->data_buffer + off, req->length);
            blob[req->length] = '\0';
            path = blob;
            if (!path[0] || !ends_with_ci(path, ".s32x")) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            if (stat(path, &sb) != 0 || !S_ISREG(sb.st_mode)) {
                mmio_fail(&resp, errno > 0 ? errno : ENOENT);
                break;
            }
            p = path + strlen(path) + 1;
            end = blob + req->length;
            while (p < end && nextra < 11) {
                if (*p == '\0') {
                    break;
                }
                extra[nextra++] = p;
                n = (uint32_t)strlen(p);
                p += n + 1;
            }

            emu = g_emu_path[0] ? g_emu_path : getenv("S32_EMU");
            if (!emu || !emu[0]) {
                mmio_fail(&resp, ENOENT);
                break;
            }

            pid = fork();
            if (pid < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }
            if (pid == 0) {
                char *av[32];
                char allow_csv[S32_MAX_SERVICES * S32_MAX_SVC_NAME];
                char deny_csv[S32_MAX_SERVICES * S32_MAX_SVC_NAME];
                int i, a = 0;
                int inherit = -1;
                if (req->status != 0xFFFFFFFFu) {
                    inherit = host_fd_for_guest(mmio, req->status);
                    if (inherit < 0) {
                        _exit(127);
                    }
                    if (dup2(inherit, STDIN_FILENO) < 0 ||
                        dup2(inherit, STDOUT_FILENO) < 0 ||
                        dup2(inherit, STDERR_FILENO) < 0) {
                        _exit(127);
                    }
                    if (inherit > STDERR_FILENO) {
                        close(inherit);
                    }
                }
                av[a++] = (char *)emu;
                av[a++] = "-q";
                /* Propagate the parent's MMIO service policy to the child.
                 * Without this the child inits default-allow, so a guest run
                 * under --deny/--allow could escape the sandbox by exec'ing a
                 * helper that regains every service. Both list forms are
                 * passed as the child evaluates deny first, then allow. */
                if (mmio->policy.deny_count > 0) {
                    int p = 0;
                    for (i = 0; i < mmio->policy.deny_count; i++) {
                        p += snprintf(deny_csv + p, sizeof(deny_csv) - p,
                                      "%s%s", i ? "," : "",
                                      mmio->policy.deny_list[i]);
                    }
                    av[a++] = "--deny";
                    av[a++] = deny_csv;
                }
                if (mmio->policy.allow_count > 0) {
                    int p = 0;
                    for (i = 0; i < mmio->policy.allow_count; i++) {
                        p += snprintf(allow_csv + p, sizeof(allow_csv) - p,
                                      "%s%s", i ? "," : "",
                                      mmio->policy.allow_list[i]);
                    }
                    av[a++] = "--allow";
                    av[a++] = allow_csv;
                }
                av[a++] = path;
                for (i = 0; i < nextra && a < 31; i++) {
                    av[a++] = extra[i];
                }
                av[a] = NULL;
                execv(emu, av);
                _exit(127);
            }
            while (waitpid(pid, &st, 0) < 0) {
                if (errno != EINTR) {
                    mmio_fail(&resp, errno > 0 ? errno : EIO);
                    goto exec_done;
                }
            }
            if (WIFEXITED(st)) {
                resp.status = (uint32_t)WEXITSTATUS(st);
                resp.length = 0;
            } else {
                resp.status = 255;
                resp.length = 0;
            }
        exec_done:
            break;
        }
            
        case S32_MMIO_OP_FLUSH:
            fflush(stdout);
            fflush(stderr);
            resp.status = 0;
            break;

        case S32_MMIO_OP_STAT: {
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;

            if (max_bytes < sizeof(s32_mmio_stat_result_t)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            struct stat host_stat;
            memset(&host_stat, 0, sizeof(host_stat));

            int rc = -1;
            if (req->status == S32_MMIO_STAT_PATH_SENTINEL) {
                if (req->length == 0 || req->length > max_bytes) {
                    mmio_fail(&resp, EINVAL);
                    break;
                }

                char *path = (char *)malloc(req->length);
                if (!path) {
                    mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            if (req->length < sizeof(s32_mmio_timepair64_t)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_timepair64_t))) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_timepair64_t))) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            s32_mmio_timepair64_t interval = {0u, 0u, 0u, 0u};
            memcpy(&interval, mmio->data_buffer + offset, sizeof(interval));

            if (interval.nanoseconds >= 1000000000u) {
                mmio_fail(&resp, EINVAL);
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
                    mmio_fail(&resp, EINVAL);
                }
                break;
            }

            s32_mmio_timepair64_t remainder = {0u, 0u, 0u, 0u};
            memcpy(mmio->data_buffer + offset, &remainder, sizeof(remainder));
            resp.length = sizeof(s32_mmio_timepair64_t);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_GETTZ: {
            if (req->length < sizeof(s32_mmio_tzinfo_t)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_tzinfo_t))) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            s32_mmio_timepair64_t query = {0u, 0u, 0u, 0u};
            memcpy(&query, mmio->data_buffer + offset, sizeof(query));
            uint64_t qsecs = ((uint64_t)query.seconds_hi << 32) | query.seconds_lo;
            time_t when = (time_t)qsecs;

            struct tm local_tm;
            s32_mmio_tzinfo_t info = {0, 0u, {0}};
            if (localtime_r(&when, &local_tm) != NULL) {
                info.gmtoff_sec = (int32_t)local_tm.tm_gmtoff;
                info.is_dst = (local_tm.tm_isdst > 0) ? 1u : 0u;
                if (local_tm.tm_zone != NULL) {
                    strncpy(info.abbrev, local_tm.tm_zone, sizeof(info.abbrev) - 1);
                }
            }
            if (info.abbrev[0] == '\0') {
                strncpy(info.abbrev, "UTC", sizeof(info.abbrev) - 1);
            }

            memcpy(mmio->data_buffer + offset, &info, sizeof(info));
            resp.length = sizeof(s32_mmio_tzinfo_t);
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }

        case S32_MMIO_OP_SOCKET: {
            uint32_t packed = req->status;
            int family = (int)(packed & 0xffu);
            int type = (int)((packed >> 8) & 0xffu);
            int protocol = (int)((packed >> 16) & 0xffu);

            if (family != S32_AF_INET) {
                mmio_fail(&resp, EAFNOSUPPORT);
                break;
            }
            if (type != S32_SOCK_STREAM) {
                mmio_fail(&resp, EPROTONOSUPPORT);
                break;
            }
            if (protocol != 0 && protocol != IPPROTO_TCP) {
                mmio_fail(&resp, EPROTONOSUPPORT);
                break;
            }

            int host_fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
            if (host_fd < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }

            int guest_fd = guest_socket_fd(mmio, host_fd);
            if (guest_fd < 0) {
                mmio_fail(&resp, EMFILE);
                break;
            }

            if (trace_io_enabled) {
                fprintf(stderr, "[MMIO] SOCKET guest_fd=%d host_fd=%d\n",
                        guest_fd, host_fd);
            }
            resp.status = (uint32_t)guest_fd;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_BIND: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }

            struct sockaddr_in addr;
            int perr = parse_guest_sockaddr_in(mmio, req, &addr);
            if (perr != 0) {
                mmio_fail(&resp, perr);
                break;
            }

            int yes = 1;
            (void)setsockopt(host_fd, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof(yes));

            if (bind(host_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }

            resp.status = S32_MMIO_STATUS_OK;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_GETSOCKNAME: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }
            struct sockaddr_in bound;
            socklen_t bound_len = sizeof(bound);
            if (getsockname(host_fd, (struct sockaddr *)&bound, &bound_len) < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            write_guest_sockaddr_in(mmio, offset, &bound);
            resp.status = S32_MMIO_STATUS_OK;
            resp.length = sizeof(s32_mmio_sockaddr_in_t);
            break;
        }

        case S32_MMIO_OP_LISTEN: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }
            int backlog = (int)req->length;
            if (backlog <= 0) {
                backlog = 8;
            }
            if (backlog > 128) {
                backlog = 128;
            }
            if (listen(host_fd, backlog) < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }
            resp.status = S32_MMIO_STATUS_OK;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_CONNECT: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }

            struct sockaddr_in addr;
            int perr = parse_guest_sockaddr_in(mmio, req, &addr);
            if (perr != 0) {
                mmio_fail(&resp, perr);
                break;
            }

            if (connect(host_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }
            resp.status = S32_MMIO_STATUS_OK;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_ACCEPT: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }

            struct sockaddr_in peer;
            socklen_t peer_len = sizeof(peer);
            int new_host = accept(host_fd, (struct sockaddr *)&peer, &peer_len);
            if (new_host < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }

            int guest_fd = guest_socket_fd(mmio, new_host);
            if (guest_fd < 0) {
                mmio_fail(&resp, EMFILE);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            write_guest_sockaddr_in(mmio, offset, &peer);
            resp.status = (uint32_t)guest_fd;
            resp.length = sizeof(s32_mmio_sockaddr_in_t);
            if (trace_io_enabled) {
                fprintf(stderr, "[MMIO] ACCEPT listen=%u new_fd=%d\n",
                        req->status, guest_fd);
            }
            break;
        }

        case S32_MMIO_OP_SHUTDOWN: {
            int host_fd = host_fd_for_guest(mmio, req->status);
            if (host_fd < 0) {
                mmio_fail(&resp, EBADF);
                break;
            }
            int how = (int)req->length;
            if (how < S32_SHUT_RD || how > S32_SHUT_RDWR) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            if (shutdown(host_fd, how) < 0) {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
                break;
            }
            resp.status = S32_MMIO_STATUS_OK;
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_ARGS_INFO: {
            if (req->length < sizeof(s32_mmio_args_info_t)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_args_info_t))) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t dest = req->offset % S32_MMIO_DATA_CAPACITY;
            if (dest > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t source_offset = req->status;
            if (source_offset > mmio->args_total_bytes) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t remaining = mmio->args_total_bytes - source_offset;
            uint32_t to_copy = req->length;
            if (to_copy > remaining) {
                to_copy = remaining;
            }

            if (to_copy > 0) {
                if (!mmio->args_blob) {
                    mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_envp_info_t))) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t dest = req->offset % S32_MMIO_DATA_CAPACITY;
            if (dest > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t source_offset = req->status;
            if (source_offset > mmio->envp_total_bytes) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t remaining = mmio->envp_total_bytes - source_offset;
            uint32_t to_copy = req->length;
            if (to_copy > remaining) {
                to_copy = remaining;
            }

            if (to_copy > 0) {
                if (!mmio->envp_blob) {
                    mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            // Extract the name from data buffer
            char *name = (char *)malloc(req->length + 1);
            if (!name) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = unlink(path);
            free(path);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_RENAME: {
            // Request: oldpath + newpath in data buffer, status = old_len
            // Response: OK or ERR
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t old_len = req->status;
            if (old_len == 0 || old_len >= req->length) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *buffer = (char *)malloc(req->length + 2);
            if (!buffer) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(buffer, mmio->data_buffer + offset, req->length);
            // old_len includes the NUL terminator of oldpath.
            // Preserve the first byte of newpath by terminating at old_len - 1.
            buffer[old_len - 1] = '\0';
            buffer[req->length] = '\0';

            const char *oldpath = buffer;
            const char *newpath = buffer + old_len;

            int rc = rename(oldpath, newpath);
            free(buffer);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_MKDIR: {
            // Request: path in data buffer, status = mode
            // Response: OK or ERR
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            mode_t mode = (mode_t)req->status;
            if (mode == 0) mode = 0755;  // Default mode

            int rc = mkdir(path, mode);
            free(path);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_RMDIR: {
            // Request: path in data buffer
            // Response: OK or ERR
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = rmdir(path);
            free(path);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_LSTAT: {
            // Request: path in data buffer (like STAT but no symlink follow)
            // Response: stat result in data buffer
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_bytes = S32_MMIO_DATA_CAPACITY - offset;

            if (max_bytes < sizeof(s32_mmio_stat_result_t)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            struct stat host_stat;
            memset(&host_stat, 0, sizeof(host_stat));
            int rc = lstat(path, &host_stat);
            free(path);

            if (rc != 0) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            // Map guest access modes to host (they match POSIX values)
            int mode = (int)req->status;
            int rc = access(path, mode);
            free(path);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_CHDIR: {
            // Request: path in data buffer
            // Response: OK or ERR
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            int rc = chdir(path);
            free(path);

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }

        case S32_MMIO_OP_GETCWD: {
            // Request: length = max buffer size
            // Response: path in data buffer, status = actual length (including NUL)
            if (req->length == 0 || req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            uint32_t max_len = S32_MMIO_DATA_CAPACITY - offset;
            if (req->length < max_len) max_len = req->length;

            char *cwd = getcwd((char *)(mmio->data_buffer + offset), max_len);
            if (!cwd) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - req->length)) {
                mmio_fail(&resp, EINVAL);
                break;
            }

            char *path = (char *)malloc(req->length + 1);
            if (!path) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            memcpy(path, mmio->data_buffer + offset, req->length);
            path[req->length] = '\0';

            DIR *host_dir = opendir(path);
            free(path);

            if (!host_dir) {
                mmio_fail(&resp, errno > 0 ? errno : ENOENT);
                break;
            }

            int guest_fd = alloc_guest_dir_fd(mmio, host_dir);
            if (guest_fd < 0) {
                closedir(host_dir);
                mmio_fail(&resp, EMFILE);
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
                mmio_fail(&resp, EINVAL);
                break;
            }

            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset > (S32_MMIO_DATA_CAPACITY - sizeof(s32_mmio_dirent_t))) {
                mmio_fail(&resp, EINVAL);
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
                    mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EBADF);
                break;
            }

            if (mmio->fd_types[guest_fd] != S32_FD_TYPE_DIR || !mmio->host_dirs[guest_fd]) {
                mmio_fail(&resp, EBADF);
                break;
            }

            int rc = closedir(mmio->host_dirs[guest_fd]);
            mmio->host_dirs[guest_fd] = NULL;
            mmio->host_fds[guest_fd] = -1;
            mmio->host_fd_owned[guest_fd] = false;
            mmio->fd_types[guest_fd] = S32_FD_TYPE_FILE;

            if (rc == 0) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, errno > 0 ? errno : EIO);
            }
            resp.length = 0;
            break;
        }
        case S32_MMIO_OP_REWINDDIR: {
            // Request: status = directory descriptor
            // Response: OK or ERR (host POSIX rewinddir)
            uint32_t guest_fd = req->status;
            DIR *host_dir = host_dir_for_guest(mmio, guest_fd);
            if (!host_dir) {
                mmio_fail(&resp, EBADF);
                break;
            }
            rewinddir(host_dir);
            resp.status = S32_MMIO_STATUS_OK;
            resp.length = 0;
            break;
        }


        // ========== Service negotiation opcodes (0xF0-0xF4) ==========

        case S32_MMIO_OP_SVC_REQUEST: {
            // Request: service name in data buffer, length = name len (incl NUL)
            // Response: status in data buffer [0]=result, [4]=base_opcode, [8]=count, [12]=version
            if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
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
            if (found) {
                resp.status = S32_MMIO_STATUS_OK;
                resp.length = 0;
            } else {
                mmio_fail(&resp, ENOENT);
            }
            break;
        }

        case S32_MMIO_OP_SVC_QUERY: {
            // Request: service name in data buffer
            // Response: [0]=result code (OK if available, DENIED if policy blocks, UNKNOWN)
            if (req->length == 0 || req->length > S32_SVC_MAX_NAME_LEN) {
                mmio_fail(&resp, EINVAL);
                break;
            }
            uint32_t offset = req->offset % S32_MMIO_DATA_CAPACITY;
            if (offset + req->length > S32_MMIO_DATA_CAPACITY) {
                mmio_fail(&resp, EINVAL);
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
                mmio_fail(&resp, EINVAL);
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
