/* sys/stat.h -- struct stat as fstat (stage01/mmio_minimal.s) fills it:
 * the MMIO stat reply (s32_mmio_stat_result_t) copied word for word,
 * through st_blksize's low word. */
#ifndef _SYS_STAT_H
#define _SYS_STAT_H

struct stat {
    unsigned int st_dev_lo;     /*  0 */
    unsigned int st_dev_hi;
    unsigned int st_ino_lo;     /*  8 */
    unsigned int st_ino_hi;
    unsigned int st_mode;       /* 16 */
    unsigned int st_nlink;      /* 20 */
    unsigned int st_uid;        /* 24 */
    unsigned int st_gid;        /* 28 */
    unsigned int st_rdev_lo;    /* 32 */
    unsigned int st_rdev_hi;
    long long    st_size;       /* 40 */
    unsigned int st_blksize_lo; /* 48 */
    unsigned int st_pad[3];
};

int fstat(int fd, struct stat *st);

#endif
