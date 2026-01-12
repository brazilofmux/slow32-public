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

// Common response status codes (errno wiring TBD)
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
    S32_MMIO_OP_BRK     = 0x08,
    S32_MMIO_OP_EXIT    = 0x09,
    S32_MMIO_OP_STAT    = 0x0A,  // stat()/fstat() metadata fetch
    S32_MMIO_OP_FLUSH   = 0x0B,

    // 0x20 - 0x2F : Filesystem metadata operations
    S32_MMIO_OP_UNLINK   = 0x20,  // unlink/remove - delete a file
    S32_MMIO_OP_RENAME   = 0x21,  // rename/move a file
    S32_MMIO_OP_MKDIR    = 0x22,  // create directory
    S32_MMIO_OP_RMDIR    = 0x23,  // remove directory
    S32_MMIO_OP_LSTAT    = 0x24,  // stat without following symlinks
    S32_MMIO_OP_ACCESS   = 0x25,  // check file accessibility
    S32_MMIO_OP_CHDIR    = 0x26,  // change current directory
    S32_MMIO_OP_GETCWD   = 0x27,  // get current working directory
    S32_MMIO_OP_OPENDIR  = 0x28,  // open directory stream
    S32_MMIO_OP_READDIR  = 0x29,  // read directory entry
    S32_MMIO_OP_CLOSEDIR = 0x2A,  // close directory stream

    // 0x30 - 0x3F : Time & event services
    S32_MMIO_OP_GETTIME     = 0x30,  // Returns wall-clock time (64-bit seconds + nanos)
    S32_MMIO_OP_SLEEP       = 0x31,  // nanosleep() + remainder reporting (64-bit seconds)
    S32_MMIO_OP_TIMER_START = 0x32,  // Arm timer, host completes on HP ring (future)
    S32_MMIO_OP_TIMER_CANCEL= 0x33,  // Cancel timer (future)
    S32_MMIO_OP_POLL        = 0x34,  // poll()/select()-style wait (future)

    // 0x40 - 0x4F : Networking / IPC
    S32_MMIO_OP_SOCKET      = 0x40,  // socket()
    S32_MMIO_OP_CONNECT     = 0x41,  // connect()
    S32_MMIO_OP_ACCEPT      = 0x42,  // accept()
    S32_MMIO_OP_SEND        = 0x43,  // send()/write() on socket
    S32_MMIO_OP_RECV        = 0x44,  // recv()/read() on socket
    S32_MMIO_OP_SHUTDOWN    = 0x45,  // shutdown()/close socket half

    // 0x60 - 0x6F : Host environment services
    S32_MMIO_OP_ARGS_INFO   = 0x60,  // Query argc/total-bytes for guest argv[]
    S32_MMIO_OP_ARGS_DATA   = 0x61,  // Copy flattened argv blob from host
    S32_MMIO_OP_ENVP_INFO   = 0x62,  // Query envc/total-bytes for guest environ[]
    S32_MMIO_OP_ENVP_DATA   = 0x63,  // Copy flattened envp blob from host
    S32_MMIO_OP_GETENV      = 0x64,  // Lookup single env var by name (returns value)
};

#pragma pack(push, 1)

typedef struct s32_mmio_timepair64 {
    uint32_t seconds_lo;   // low 32 bits of seconds
    uint32_t seconds_hi;   // high 32 bits of seconds
    uint32_t nanoseconds;  // 0..999,999,999
    uint32_t reserved;     // align to 16 bytes / future flags
} s32_mmio_timepair64_t;

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
