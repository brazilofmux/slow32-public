/* sys/stat.h -- struct stat is the MMIO stat reply (s32_mmio_stat_result_t)
 * laid out in place: the asm fstat (stage01/mmio_minimal.s) copies its
 * first 52 bytes word for word, stat/lstat (libc/posix_more.c) copy it
 * whole. */
#ifndef _SYS_STAT_H
#define _SYS_STAT_H
#include <sys/types.h>

struct stat {
    unsigned long long st_dev;      /*  0 */
    unsigned long long st_ino;      /*  8 */
    unsigned int       st_mode;     /* 16 */
    unsigned int       st_nlink;    /* 20 */
    unsigned int       st_uid;      /* 24 */
    unsigned int       st_gid;      /* 28 */
    unsigned long long st_rdev;     /* 32 */
    long long          st_size;     /* 40 */
    unsigned long long st_blksize;  /* 48 */
    unsigned long long st_blocks;   /* 56 */
    long long          st_atime;    /* 64 */
    unsigned int       st_atime_nsec;
    unsigned int       st_pad0;
    long long          st_mtime;    /* 80 */
    unsigned int       st_mtime_nsec;
    unsigned int       st_pad1;
    long long          st_ctime;    /* 96 */
    unsigned int       st_ctime_nsec;
    unsigned int       st_pad2;
};

#define S_IFMT   0170000
#define S_IFSOCK 0140000
#define S_IFLNK  0120000
#define S_IFREG  0100000
#define S_IFBLK  0060000
#define S_IFDIR  0040000
#define S_IFCHR  0020000
#define S_IFIFO  0010000
#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISLNK(m)  (((m) & S_IFMT) == S_IFLNK)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)  (((m) & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)
#define S_IRUSR 0400
#define S_IWUSR 0200
#define S_IXUSR 0100
#define S_IRWXU 0700

int stat(const char *path, struct stat *st);
int lstat(const char *path, struct stat *st);
int fstat(int fd, struct stat *st);
int mkdir(const char *path, mode_t mode);

#endif
