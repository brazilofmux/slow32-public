#ifndef SLOW32_SYS_STAT_H
#define SLOW32_SYS_STAT_H

#include <stddef.h>
#include <stdint.h>
#include "time.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef unsigned long long dev_t;
typedef unsigned long long ino_t;
typedef unsigned int mode_t;
typedef unsigned int nlink_t;
typedef unsigned int uid_t;
typedef unsigned int gid_t;
typedef long long off_t;
typedef unsigned long blksize_t;
typedef unsigned long long blkcnt_t;

struct stat {
    dev_t st_dev;
    ino_t st_ino;
    mode_t st_mode;
    nlink_t st_nlink;
    uid_t st_uid;
    gid_t st_gid;
    dev_t st_rdev;
    off_t st_size;
    blksize_t st_blksize;
    blkcnt_t st_blocks;
    /* POSIX 2008: the times as timespecs, st_atime and the others the
     * seconds of them.  The same bytes as the time_t + nanoseconds pairs
     * these were. */
    struct timespec st_atim;
    struct timespec st_mtim;
    struct timespec st_ctim;
};
#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec

#define S_IFMT   0170000
#define S_IFSOCK 0140000
#define S_IFLNK  0120000
#define S_IFREG  0100000
#define S_IFBLK  0060000
#define S_IFDIR  0040000
#define S_IFCHR  0020000
#define S_IFIFO  0010000

#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)  (((m) & S_IFMT) == S_IFBLK)
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISLNK(m)  (((m) & S_IFMT) == S_IFLNK)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)

int stat(const char *path, struct stat *buf);
int fstat(int fd, struct stat *buf);
int lstat(const char *path, struct stat *buf);
int mkdir(const char *pathname, mode_t mode);

/* The machine sets no permissions: chmod and fchmod succeed when the
 * permission bits asked for are the ones the file has, and fail with
 * ENOSYS otherwise (posix_extra.c). */
int chmod(const char *path, mode_t mode);
int fchmod(int fd, mode_t mode);

#ifdef __cplusplus
}
#endif
#endif
