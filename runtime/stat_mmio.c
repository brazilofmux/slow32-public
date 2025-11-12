#include <stddef.h>
#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "sys/stat.h"

static int copy_result_into_stat(struct stat *dst) {
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

static int issue_stat_request(unsigned int status_arg,
                              unsigned int payload_len,
                              struct stat *buf) {
    if (!buf) {
        return -1;
    }

    int result = s32_mmio_request(S32_MMIO_OP_STAT,
                                  payload_len,
                                  0u,
                                  status_arg);
    if (result != (int)S32_MMIO_STATUS_OK) {
        return -1;
    }

    return copy_result_into_stat(buf);
}

int stat(const char *path, struct stat *buf) {
    if (!path || !buf) {
        return -1;
    }

    size_t len = strlen(path) + 1u;
    if (len == 0 || len > S32_MMIO_DATA_CAPACITY) {
        return -1;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    memcpy((void *)data_buffer, path, len);

    return issue_stat_request(S32_MMIO_STAT_PATH_SENTINEL,
                              (unsigned int)len,
                              buf);
}

int fstat(int fd, struct stat *buf) {
    if (fd < 0 || !buf) {
        return -1;
    }

    return issue_stat_request((unsigned int)fd,
                              0u,
                              buf);
}
