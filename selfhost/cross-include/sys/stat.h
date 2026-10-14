/* sys/stat.h -- the kernel's struct stat (cross builds).  libc_linux.c
 * hands it to the stat/fstat/newfstatat syscalls as is, so it must be
 * the kernel's own: arch/x86/include/uapi/asm/stat.h on x86-64 (144
 * bytes), include/uapi/asm-generic/stat.h on AArch64 (128).  The SLOW-32
 * guests' struct stat (stage08/include) is the MMIO reply instead. */
#ifndef _SYS_STAT_H
#define _SYS_STAT_H
#include <sys/types.h>
#include <time.h>

#if defined(__aarch64__)
struct stat {
    unsigned long   st_dev;         /*   0 */
    unsigned long   st_ino;         /*   8 */
    unsigned int    st_mode;        /*  16 */
    unsigned int    st_nlink;       /*  20 */
    unsigned int    st_uid;         /*  24 */
    unsigned int    st_gid;         /*  28 */
    unsigned long   st_rdev;        /*  32 */
    unsigned long   __pad1;         /*  40 */
    long            st_size;        /*  48 */
    int             st_blksize;     /*  56 */
    int             __pad2;         /*  60 */
    long            st_blocks;      /*  64 */
    struct timespec st_atim;        /*  72 */
    struct timespec st_mtim;        /*  88 */
    struct timespec st_ctim;        /* 104 */
    unsigned int    __unused4;      /* 120 */
    unsigned int    __unused5;      /* 124 */
};
#else
struct stat {
    unsigned long   st_dev;         /*   0 */
    unsigned long   st_ino;         /*   8 */
    unsigned long   st_nlink;       /*  16 */
    unsigned int    st_mode;        /*  24 */
    unsigned int    st_uid;         /*  28 */
    unsigned int    st_gid;         /*  32 */
    unsigned int    __pad0;         /*  36 */
    unsigned long   st_rdev;        /*  40 */
    long            st_size;        /*  48 */
    long            st_blksize;     /*  56 */
    long            st_blocks;      /*  64 */
    struct timespec st_atim;        /*  72 */
    struct timespec st_mtim;        /*  88 */
    struct timespec st_ctim;        /* 104 */
    long            __unused[3];    /* 120 */
};
#endif
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
#define S_ISREG(m)  (((m) & S_IFMT) == S_IFREG)
#define S_ISDIR(m)  (((m) & S_IFMT) == S_IFDIR)
#define S_ISCHR(m)  (((m) & S_IFMT) == S_IFCHR)
#define S_ISBLK(m)  (((m) & S_IFMT) == S_IFBLK)
#define S_ISFIFO(m) (((m) & S_IFMT) == S_IFIFO)
#define S_ISLNK(m)  (((m) & S_IFMT) == S_IFLNK)
#define S_ISSOCK(m) (((m) & S_IFMT) == S_IFSOCK)

int stat(const char *path, struct stat *st);
int lstat(const char *path, struct stat *st);
int fstat(int fd, struct stat *st);
int mkdir(const char *path, unsigned int mode);
int chmod(const char *path, unsigned int mode);
#endif
