// SLOW-32 Ring Buffer MMIO Implementation
#include "mmio_ring.h"
#include "slow32.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
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

// Initialize MMIO ring buffers
void mmio_ring_init(mmio_ring_state_t *mmio, uint32_t heap_base, uint32_t heap_size) {
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
            int fd = req->status;  // fd is in status field for requests
            if (fd == 1 || fd == 2) {  // stdout or stderr
                FILE *stream = (fd == 1) ? stdout : stderr;
                uint32_t written = 0;
                for (uint32_t i = 0; i < req->length && i < S32_MMIO_DATA_CAPACITY; i++) {
                    fputc(mmio->data_buffer[(req->offset + i) % S32_MMIO_DATA_CAPACITY], stream);
                    written++;
                }
                fflush(stream);
                resp.length = written;
                resp.status = written;
            } else {
                resp.length = 0;
                resp.status = 0xFFFFFFFFu;
            }
            break;
        }
        
        case S32_MMIO_OP_READ: {
            int fd = req->status;  // fd is in status field
            if (fd == 0) {  // stdin
                uint32_t read_count = 0;
                for (uint32_t i = 0; i < req->length && i < S32_MMIO_DATA_CAPACITY; i++) {
                    int ch = fgetc(stdin);
                    if (ch == EOF) break;
                    
                    mmio->data_buffer[(req->offset + i) % S32_MMIO_DATA_CAPACITY] = (uint8_t)ch;
                    read_count++;
                    
                    if (ch == '\n') break;  // Line buffered
                }
                resp.length = read_count;
                resp.status = read_count;
            } else {
                resp.length = 0;
                resp.status = 0xFFFFFFFFu;
            }
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
            if (cpu && cpu->halted) {
                *(cpu->halted) = true;
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

            resp.length = to_copy;
            resp.status = S32_MMIO_STATUS_OK;
            break;
        }
            
        default:
            resp.status = ENOSYS;  // Not implemented
            break;
    }
    
    // Write response
    if (!ring_full(mmio->resp_head, mmio->resp_tail)) {
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
