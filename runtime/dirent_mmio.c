// SLOW-32 Directory MMIO Operations
// Implements: opendir, readdir, closedir, rewinddir, dirfd

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "dirent.h"
#include "stdlib.h"

// Static storage for the current directory entry (readdir is not reentrant)
static struct dirent s_current_dirent;

DIR *opendir(const char *name) {
    if (!name) {
        return NULL;
    }

    size_t len = strlen(name) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return NULL;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, name, len);

    int result = s32_mmio_request(S32_MMIO_OP_OPENDIR,
                                  (unsigned int)len,
                                  0u,
                                  0u);
    if (result == (int)S32_MMIO_STATUS_ERR) {
        return NULL;
    }

    // Allocate DIR structure
    DIR *dirp = (DIR *)malloc(sizeof(DIR));
    if (!dirp) {
        // Close the directory on the host side since we can't return it
        s32_mmio_request(S32_MMIO_OP_CLOSEDIR, 0u, 0u, (unsigned int)result);
        return NULL;
    }

    dirp->dd_fd = result;  // Store the directory descriptor
    dirp->dd_loc = 0;
    return dirp;
}

struct dirent *readdir(DIR *dirp) {
    if (!dirp || dirp->dd_fd < 0) {
        return NULL;
    }

    int result = s32_mmio_request(S32_MMIO_OP_READDIR,
                                  (unsigned int)sizeof(s32_mmio_dirent_t),
                                  0u,
                                  (unsigned int)dirp->dd_fd);

    if (result == (int)S32_MMIO_STATUS_EOF) {
        // End of directory
        return NULL;
    }

    if (result != (int)S32_MMIO_STATUS_OK) {
        // Error
        return NULL;
    }

    // Copy the dirent from data buffer
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    s32_mmio_dirent_t mmio_dirent;
    memcpy(&mmio_dirent, (const void *)data_buffer, sizeof(mmio_dirent));

    // Convert to struct dirent
    s_current_dirent.d_ino = mmio_dirent.d_ino;
    s_current_dirent.d_type = (unsigned char)mmio_dirent.d_type;
    memcpy(s_current_dirent.d_name, mmio_dirent.d_name, sizeof(s_current_dirent.d_name));
    s_current_dirent.d_name[255] = '\0';  // Ensure NUL termination

    dirp->dd_loc++;
    return &s_current_dirent;
}

int closedir(DIR *dirp) {
    if (!dirp) {
        return -1;
    }

    int result = s32_mmio_request(S32_MMIO_OP_CLOSEDIR,
                                  0u,
                                  0u,
                                  (unsigned int)dirp->dd_fd);

    free(dirp);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int dirfd(DIR *dirp) {
    if (!dirp) {
        return -1;
    }
    return dirp->dd_fd;
}

void rewinddir(DIR *dirp) {
    if (!dirp || dirp->dd_fd < 0) {
        return;
    }

    // Close and reopen the directory to reset position
    // This is a simple implementation - a more efficient one would
    // add a REWINDDIR MMIO operation

    // Note: We don't have access to the original path, so this is
    // a limitation. For now, just reset the location counter.
    // A full implementation would need to either:
    // 1. Store the path in the DIR structure
    // 2. Add a REWINDDIR MMIO operation
    dirp->dd_loc = 0;

    // TODO: Implement proper rewinddir via MMIO operation
    // For now, this is a no-op beyond resetting dd_loc
}
