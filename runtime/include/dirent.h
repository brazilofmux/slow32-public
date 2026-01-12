#ifndef SLOW32_DIRENT_H
#define SLOW32_DIRENT_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// File type constants (matching POSIX DT_* values)
#define DT_UNKNOWN  0
#define DT_FIFO     1
#define DT_CHR      2
#define DT_DIR      4
#define DT_BLK      6
#define DT_REG      8
#define DT_LNK      10
#define DT_SOCK     12

// Directory entry structure
struct dirent {
    uint64_t d_ino;          // Inode number
    unsigned char d_type;    // File type (DT_REG, DT_DIR, etc.)
    char d_name[256];        // Filename (NUL-terminated)
};

// Opaque directory stream type
typedef struct {
    int dd_fd;       // Directory descriptor from MMIO
    int dd_loc;      // Current position (unused, for compatibility)
} DIR;

// Open a directory stream
DIR *opendir(const char *name);

// Read next directory entry
// Returns NULL when end of directory is reached or on error
struct dirent *readdir(DIR *dirp);

// Close a directory stream
int closedir(DIR *dirp);

// Get file descriptor for directory stream
int dirfd(DIR *dirp);

// Reset directory stream position to beginning
void rewinddir(DIR *dirp);

#ifdef __cplusplus
}
#endif

#endif // SLOW32_DIRENT_H
