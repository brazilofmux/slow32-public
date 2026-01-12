// SLOW-32 Filesystem MMIO Operations
// Implements: unlink, rename, mkdir, rmdir, lstat, access, chdir, getcwd

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "sys/stat.h"
#include "unistd.h"

// Helper to copy result into stat struct (shared with stat_mmio.c via inline)
static int copy_lstat_result(struct stat *dst) {
    s32_mmio_stat_result_t result = {0};
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy(&result, (const void *)data_buffer, sizeof(result));

    dst->st_dev = (dev_t)result.st_dev;
    dst->st_ino = (ino_t)result.st_ino;
    dst->st_mode = (mode_t)result.st_mode;
    dst->st_nlink = (nlink_t)result.st_nlink;
    dst->st_uid = (uid_t)result.st_uid;
    dst->st_gid = (gid_t)result.st_gid;
    dst->st_rdev = (dev_t)result.st_rdev;
    dst->st_size = (off_t)result.st_size;
    dst->st_blksize = (blksize_t)result.st_blksize;
    dst->st_blocks = (blkcnt_t)result.st_blocks;
    dst->st_atime = (time_t)result.st_atime_sec;
    dst->st_atime_nsec = result.st_atime_nsec;
    dst->st_mtime = (time_t)result.st_mtime_sec;
    dst->st_mtime_nsec = result.st_mtime_nsec;
    dst->st_ctime = (time_t)result.st_ctime_sec;
    dst->st_ctime_nsec = result.st_ctime_nsec;
    return 0;
}

int unlink(const char *pathname) {
    if (!pathname) {
        return -1;
    }

    size_t len = strlen(pathname) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len);

    int result = s32_mmio_request(S32_MMIO_OP_UNLINK,
                                  (unsigned int)len,
                                  0u,
                                  0u);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int rename(const char *oldpath, const char *newpath) {
    if (!oldpath || !newpath) {
        return -1;
    }

    size_t old_len = strlen(oldpath) + 1u;
    size_t new_len = strlen(newpath) + 1u;
    size_t total_len = old_len + new_len;

    if (old_len == 0 || new_len == 0 || total_len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, oldpath, old_len);
    memcpy((void *)(data_buffer + old_len), newpath, new_len);

    int result = s32_mmio_request(S32_MMIO_OP_RENAME,
                                  (unsigned int)total_len,
                                  0u,
                                  (unsigned int)old_len);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int mkdir(const char *pathname, mode_t mode) {
    if (!pathname) {
        return -1;
    }

    size_t len = strlen(pathname) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len);

    int result = s32_mmio_request(S32_MMIO_OP_MKDIR,
                                  (unsigned int)len,
                                  0u,
                                  (unsigned int)mode);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int rmdir(const char *pathname) {
    if (!pathname) {
        return -1;
    }

    size_t len = strlen(pathname) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len);

    int result = s32_mmio_request(S32_MMIO_OP_RMDIR,
                                  (unsigned int)len,
                                  0u,
                                  0u);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int lstat(const char *path, struct stat *buf) {
    if (!path || !buf) {
        return -1;
    }

    size_t len = strlen(path) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, path, len);

    int result = s32_mmio_request(S32_MMIO_OP_LSTAT,
                                  (unsigned int)len,
                                  0u,
                                  S32_MMIO_STAT_PATH_SENTINEL);
    if (result != (int)S32_MMIO_STATUS_OK) {
        return -1;
    }

    return copy_lstat_result(buf);
}

int access(const char *pathname, int mode) {
    if (!pathname) {
        return -1;
    }

    size_t len = strlen(pathname) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, pathname, len);

    int result = s32_mmio_request(S32_MMIO_OP_ACCESS,
                                  (unsigned int)len,
                                  0u,
                                  (unsigned int)mode);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

int chdir(const char *path) {
    if (!path) {
        return -1;
    }

    size_t len = strlen(path) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, path, len);

    int result = s32_mmio_request(S32_MMIO_OP_CHDIR,
                                  (unsigned int)len,
                                  0u,
                                  0u);
    return (result == (int)S32_MMIO_STATUS_OK) ? 0 : -1;
}

char *getcwd(char *buf, size_t size) {
    if (!buf || size == 0) {
        return NULL;
    }

    if (size > S32_MMIO_DATA_CAPACITY) {
        size = S32_MMIO_DATA_CAPACITY;
    }

    int result = s32_mmio_request(S32_MMIO_OP_GETCWD,
                                  (unsigned int)size,
                                  0u,
                                  0u);
    if (result == (int)S32_MMIO_STATUS_ERR || result == 0) {
        return NULL;
    }

    // Copy result from data buffer to user buffer
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    size_t copy_len = (size_t)result;
    if (copy_len > size) copy_len = size;
    memcpy(buf, (const void *)data_buffer, copy_len);

    // Ensure NUL termination
    if (copy_len < size) {
        buf[copy_len] = '\0';
    } else {
        buf[size - 1] = '\0';
    }

    return buf;
}

// remove() is defined in stdio.h and calls unlink()
int remove(const char *pathname) {
    return unlink(pathname);
}
