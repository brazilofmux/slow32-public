// runtime/include/sys/statvfs.h (SLOW-32)
// File-system statistics.  The machine has no service that answers this,
// so statvfs and fstatvfs fail with ENOSYS (posix_extra.c).
#ifndef SLOW32_SYS_STATVFS_H
#define SLOW32_SYS_STATVFS_H

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long long fsblkcnt_t;
typedef unsigned long long fsfilcnt_t;

struct statvfs {
    unsigned long f_bsize;
    unsigned long f_frsize;
    fsblkcnt_t f_blocks;
    fsblkcnt_t f_bfree;
    fsblkcnt_t f_bavail;
    fsfilcnt_t f_files;
    fsfilcnt_t f_ffree;
    fsfilcnt_t f_favail;
    unsigned long f_fsid;
    unsigned long f_flag;
    unsigned long f_namemax;
};

#define ST_RDONLY 1
#define ST_NOSUID 2

int statvfs(const char *path, struct statvfs *buf);
int fstatvfs(int fd, struct statvfs *buf);

#ifdef __cplusplus
}
#endif

#endif
