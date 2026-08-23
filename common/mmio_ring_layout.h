#ifndef S32_MMIO_RING_LAYOUT_H
#define S32_MMIO_RING_LAYOUT_H

#include <stdint.h>

// Offsets within the MMIO window (relative to __mmio_base)
enum {
    S32_MMIO_REQ_HEAD_OFFSET    = 0x0000u,
    S32_MMIO_REQ_TAIL_OFFSET    = 0x0004u,
    S32_MMIO_REQ_RING_OFFSET    = 0x1000u,
    S32_MMIO_RESP_HEAD_OFFSET   = 0x2000u,
    S32_MMIO_RESP_TAIL_OFFSET   = 0x2004u,
    S32_MMIO_RESP_RING_OFFSET   = 0x3000u,
    S32_MMIO_DATA_BUFFER_OFFSET = 0x4000u,
};

// Ring configuration constants
enum {
    S32_MMIO_RING_ENTRIES = 256u,
    S32_MMIO_DESC_WORDS   = 4u,
};

#define S32_MMIO_DESC_BYTES   (S32_MMIO_DESC_WORDS * sizeof(uint32_t))
#define S32_MMIO_DATA_CAPACITY (48u * 1024u)  // Total bytes available in data buffer

// Common response status codes.
//
// On S32_MMIO_STATUS_ERR, the response descriptor's `length` field carries a
// positive host/guest errno (ENOENT, EBADF, EINVAL, …). Guests map that into
// the C `errno` variable. If length is 0 or out of range, guests fall back to
// EIO. Success statuses (OK, byte counts, fds) leave errno unchanged.
enum {
    S32_MMIO_STATUS_OK    = 0u,
    S32_MMIO_STATUS_EOF   = 0xFFFFFFFDu,  // End of file/directory
    S32_MMIO_STATUS_EINTR = 0xFFFFFFFEu,
    S32_MMIO_STATUS_ERR   = 0xFFFFFFFFu,
};

// Special status value for STAT requests that target a pathname (not fd)
#define S32_MMIO_STAT_PATH_SENTINEL 0xFFFFFFFFu

// Opcode range tags help keep services grouped
enum {
    S32_MMIO_OPCODE_RANGE_CORE  = 0x00,  // Basic file/process I/O
    S32_MMIO_OPCODE_RANGE_MEM   = 0x10,  // Memory & process management
    S32_MMIO_OPCODE_RANGE_FS    = 0x20,  // Filesystem metadata
    S32_MMIO_OPCODE_RANGE_TIME  = 0x30,  // Time & timers
    S32_MMIO_OPCODE_RANGE_NET   = 0x40,  // Networking / IPC
    S32_MMIO_OPCODE_RANGE_HOST  = 0x60,  // Host services (env, randomness, etc.)
    S32_MMIO_OPCODE_RANGE_USER  = 0x80,  // Experimental / user-defined
};

// Operation codes shared between guest and host
enum s32_mmio_opcode {
    // 0x00 - 0x0F : Core process & stdio syscalls
    S32_MMIO_OP_NOP     = 0x00,
    S32_MMIO_OP_PUTCHAR = 0x01,
    S32_MMIO_OP_GETCHAR = 0x02,
    S32_MMIO_OP_WRITE   = 0x03,
    S32_MMIO_OP_READ    = 0x04,
    S32_MMIO_OP_OPEN    = 0x05,
    S32_MMIO_OP_CLOSE   = 0x06,
    S32_MMIO_OP_SEEK    = 0x07,
    // 0x08 reserved (was BRK, removed — heap is statically allocated by linker)
    S32_MMIO_OP_EXIT    = 0x09,
    S32_MMIO_OP_EXEC    = 0x10,  // run another .s32x and wait; path+args in data buf
    S32_MMIO_OP_STAT    = 0x0A,  // stat()/fstat() metadata fetch
    S32_MMIO_OP_FLUSH   = 0x0B,
    S32_MMIO_OP_READ_DIRECT = 0x0C, // Direct read into guest memory (zero-copy)
    S32_MMIO_OP_FTRUNCATE   = 0x0D, // Truncate open file to specified length

    // 0x20 - 0x2F : Filesystem metadata operations
    S32_MMIO_OP_UNLINK   = 0x20,  // unlink/remove - delete a file
    S32_MMIO_OP_RENAME   = 0x21,  // rename/move a file
    S32_MMIO_OP_MKDIR    = 0x22,  // create directory
    S32_MMIO_OP_RMDIR    = 0x23,  // remove directory
    S32_MMIO_OP_LSTAT    = 0x24,  // stat without following symlinks
    S32_MMIO_OP_ACCESS   = 0x25,  // check file accessibility
    S32_MMIO_OP_CHDIR    = 0x26,  // change current directory
    S32_MMIO_OP_GETCWD   = 0x27,  // get current working directory
    S32_MMIO_OP_OPENDIR   = 0x28,  // open directory stream
    S32_MMIO_OP_READDIR   = 0x29,  // read directory entry
    S32_MMIO_OP_CLOSEDIR  = 0x2A,  // close directory stream
    S32_MMIO_OP_REWINDDIR = 0x2B,  // rewind directory stream to the start

    // 0x30 - 0x3F : Time & event services
    S32_MMIO_OP_GETTIME     = 0x30,  // Returns wall-clock time (64-bit seconds + nanos)
    S32_MMIO_OP_SLEEP       = 0x31,  // nanosleep() + remainder reporting (64-bit seconds)
    S32_MMIO_OP_GETTZ       = 0x35,  // Returns host timezone info for a queried UTC time
    S32_MMIO_OP_TIMER_START = 0x32,  // Arm timer, host completes on HP ring (future)
    S32_MMIO_OP_TIMER_CANCEL= 0x33,  // Cancel timer (future)
    S32_MMIO_OP_POLL        = 0x34,  // poll()/select()-style wait (future)

    // 0x40 - 0x4F : Networking / IPC
    // v1 is IPv4 TCP only. No DNS, no UDP, no Unix sockets.
    // Address payloads are s32_mmio_sockaddr_in_t (guest-endian).
    S32_MMIO_OP_SOCKET      = 0x40,  // socket() — status packs family|type<<8|proto<<16
    S32_MMIO_OP_CONNECT     = 0x41,  // connect() — status=fd, payload=sockaddr_in
    S32_MMIO_OP_ACCEPT      = 0x42,  // accept() — status=listen fd, returns new fd
    S32_MMIO_OP_SEND        = 0x43,  // send() — alias of WRITE on a socket fd
    S32_MMIO_OP_RECV        = 0x44,  // recv() — alias of READ on a socket fd
    S32_MMIO_OP_SHUTDOWN    = 0x45,  // shutdown() — status=fd, length=how
    S32_MMIO_OP_BIND        = 0x46,  // bind() — status=fd, payload=sockaddr_in
    S32_MMIO_OP_LISTEN      = 0x47,  // listen() — status=fd, length=backlog
    S32_MMIO_OP_GETSOCKNAME = 0x48,  // getsockname() — status=fd, writes sockaddr_in

    // 0x60 - 0x6F : Host environment services
    S32_MMIO_OP_ARGS_INFO   = 0x60,  // Query argc/total-bytes for guest argv[]
    S32_MMIO_OP_ARGS_DATA   = 0x61,  // Copy flattened argv blob from host
    S32_MMIO_OP_ENVP_INFO   = 0x62,  // Query envc/total-bytes for guest environ[]
    S32_MMIO_OP_ENVP_DATA   = 0x63,  // Copy flattened envp blob from host
    S32_MMIO_OP_GETENV      = 0x64,  // Lookup single env var by name (returns value)

    // 0xF0 - 0xFF : Service negotiation protocol
    S32_MMIO_OP_SVC_REQUEST  = 0xF0,  // Request a named service (allocates opcode range)
    S32_MMIO_OP_SVC_RELEASE  = 0xF1,  // Release a previously granted service
    S32_MMIO_OP_SVC_QUERY    = 0xF2,  // Query if a service is available (without requesting)
    S32_MMIO_OP_SVC_LIST     = 0xF3,  // List all available services
    S32_MMIO_OP_SVC_VERSION  = 0xF4,  // Query protocol version
};

// SOCKET status packing: family in bits 0-7, type in 8-15, protocol in 16-23.
#define S32_AF_INET       2
#define S32_SOCK_STREAM   1
#define S32_SHUT_RD       0
#define S32_SHUT_WR       1
#define S32_SHUT_RDWR     2
#define S32_MMIO_SOCKADDR_IN_SIZE 8

#pragma pack(push, 1)

// Compact IPv4 address written to the MMIO data buffer for BIND/CONNECT/ACCEPT.
// Multi-byte fields are guest-endian (little-endian). 0x7f000001 = 127.0.0.1.
// The host converts to sockaddr_in / network order. This is not POSIX layout.
typedef struct s32_mmio_sockaddr_in {
    uint32_t addr;
    uint16_t port;
    uint16_t family;
} s32_mmio_sockaddr_in_t;

typedef struct s32_mmio_timepair64 {
    uint32_t seconds_lo;   // low 32 bits of seconds
    uint32_t seconds_hi;   // high 32 bits of seconds
    uint32_t nanoseconds;  // 0..999,999,999
    uint32_t reserved;     // align to 16 bytes / future flags
} s32_mmio_timepair64_t;

#pragma pack(pop)

// S32_MMIO_OP_GETTZ payload. The guest writes a s32_mmio_timepair64_t holding
// the UTC time it wants to convert into the data buffer at req->offset; the host
// resolves that instant against its local timezone and overwrites the same
// region with this struct. Both structs are 16 bytes, so a single buffer slot
// serves request and response.
#pragma pack(push, 1)
typedef struct s32_mmio_tzinfo {
    int32_t  gmtoff_sec;   // seconds east of UTC (negative = west) for the queried time
    uint32_t is_dst;       // 1 if daylight saving is in effect, else 0
    char     abbrev[8];    // timezone abbreviation, NUL-terminated (e.g. "UTC","PST")
} s32_mmio_tzinfo_t;
#pragma pack(pop)

// Packed stat payload shared between guest and host
#pragma pack(push, 1)
typedef struct s32_mmio_stat_result {
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
#pragma pack(pop)

// Host argument metadata returned by S32_MMIO_OP_ARGS_INFO
#pragma pack(push, 1)
typedef struct s32_mmio_args_info {
    uint32_t argc;         // Number of arguments (argv[0] included)
    uint32_t total_bytes;  // Bytes needed to store all NUL-terminated strings
    uint32_t flags;        // Reserved for future features (envp, etc.)
    uint32_t reserved;     // Keep structure 16-byte aligned
} s32_mmio_args_info_t;
#pragma pack(pop)

#define S32_MMIO_ARGS_FLAG_ENVP 0x00000001u  // Placeholder for future envp support

#define S32_MMIO_ARGS_MAX_BYTES (64u * 1024u)  // Safety cap for argv blob transfers

// Host environment metadata returned by S32_MMIO_OP_ENVP_INFO
#pragma pack(push, 1)
typedef struct s32_mmio_envp_info {
    uint32_t envc;         // Number of environment variables
    uint32_t total_bytes;  // Bytes needed to store all NUL-terminated "KEY=VALUE" strings
    uint32_t flags;        // Reserved for future features
    uint32_t reserved;     // Keep structure 16-byte aligned
} s32_mmio_envp_info_t;
#pragma pack(pop)

#define S32_MMIO_ENVP_MAX_BYTES (128u * 1024u)  // Safety cap for envp blob transfers

// Service negotiation response codes (written to data buffer by host)
#define S32_SVC_OK          0x00  // Service granted
#define S32_SVC_DENIED      0x01  // Service denied by policy
#define S32_SVC_UNKNOWN     0x02  // Service name not recognized
#define S32_SVC_CONFLICT    0x03  // Opcode range conflict
#define S32_SVC_LIMIT       0x04  // Too many active services
#define S32_SVC_VERSION_ERR 0x05  // Protocol version mismatch

// Service negotiation protocol version
#define S32_SVC_PROTOCOL_VERSION 1

// Maximum service name length (including NUL)
#define S32_SVC_MAX_NAME_LEN 32

// Term service opcode offsets (relative to negotiated base)
#define S32_TERM_SET_MODE     0   // Set raw/cooked mode
#define S32_TERM_GET_SIZE     1   // Get terminal rows/cols
#define S32_TERM_MOVE_CURSOR  2   // Move cursor to row,col
#define S32_TERM_CLEAR        3   // Clear screen/line/to-end
#define S32_TERM_SET_ATTR     4   // Set text attribute (normal/bold/reverse)
#define S32_TERM_READ_KEY     5   // Blocking key read
#define S32_TERM_KEY_AVAIL    6   // Non-blocking key poll
#define S32_TERM_SET_COLOR    7   // Set fg/bg color
#define S32_TERM_PUTC         8   // Output single character at cursor
#define S32_TERM_PUTS         9   // Output string at cursor (data in buffer)
#define S32_TERM_SAVE_SCREEN  10  // Push screen contents onto internal stack
#define S32_TERM_RESTORE_SCREEN 11 // Pop screen contents and repaint
#define S32_TERM_BEGIN_UPDATE 12  // Begin buffered update (shadow only, no stdout)
#define S32_TERM_END_UPDATE   13  // End update: diff shadow vs prev, emit minimum ANSI
#define S32_TERM_OPCODE_COUNT 14  // Total opcodes for term service

// Tube service opcode offsets (relative to negotiated base). 16-opcode
// window; 6..15 reserved. See docs/TUBE.md.
#define S32_TUBE_INFO         0
#define S32_TUBE_OPEN         1
#define S32_TUBE_CLOSE        2
#define S32_TUBE_PRESENT      3
#define S32_TUBE_STATUS       4
#define S32_TUBE_KEYS         5
#define S32_TUBE_OPCODE_COUNT 16

#define S32_TUBE_MODE_VEC     1
#define S32_TUBE_MODE_FB      2
#define S32_TUBE_MODE_PPU     3

#define S32_TUBE_LIST_MAX_WORDS 65536u
#define S32_TUBE_KEY_QUEUE      256u

#define S32_TUBE_VOP_END      0u
#define S32_TUBE_VOP_MOVE     1u
#define S32_TUBE_VOP_DRAW     2u
#define S32_TUBE_VOP_POINT    3u
#define S32_TUBE_VOP_INTEN    4u
#define S32_TUBE_VOP_COLOR    5u

#define S32_TUBE_FB_FORMAT_P8 1u   // 8bpp indexed + 256 x 0x00RRGGBB palette
#define S32_TUBE_FB_MAX_W     640u
#define S32_TUBE_FB_MAX_H     480u
#define S32_TUBE_PPU_W        320u
#define S32_TUBE_PPU_H        200u
#define S32_TUBE_PPU_MAX_NT   128u  // nametable max dimension in tiles
#define S32_TUBE_PPU_TILES    1024u
#define S32_TUBE_PPU_SPRITES  128u

// Access mode constants for S32_MMIO_OP_ACCESS
#define S32_MMIO_F_OK 0  // File exists
#define S32_MMIO_X_OK 1  // Execute permission
#define S32_MMIO_W_OK 2  // Write permission
#define S32_MMIO_R_OK 4  // Read permission

// File type constants for dirent (matching POSIX DT_* values)
#define S32_DT_UNKNOWN 0
#define S32_DT_FIFO    1
#define S32_DT_CHR     2
#define S32_DT_DIR     4
#define S32_DT_BLK     6
#define S32_DT_REG     8
#define S32_DT_LNK     10
#define S32_DT_SOCK    12

// Directory entry structure for S32_MMIO_OP_READDIR
#pragma pack(push, 1)
typedef struct s32_mmio_dirent {
    uint64_t d_ino;        // Inode number
    uint32_t d_type;       // File type (DT_REG, DT_DIR, DT_LNK, etc.)
    uint32_t d_namlen;     // Length of name (excluding NUL)
    char d_name[256];      // Filename (NUL-terminated)
} s32_mmio_dirent_t;
#pragma pack(pop)

#define S32_MMIO_DIRENT_SIZE sizeof(s32_mmio_dirent_t)  // 272 bytes


#endif // S32_MMIO_RING_LAYOUT_H
