#ifndef _SYS_STAT_H
#define _SYS_STAT_H

typedef unsigned int off_t;

struct stat {
    unsigned int st_dev;
    unsigned int st_ino;
    unsigned int st_mode;
    unsigned int st_nlink;
    unsigned int st_uid;
    unsigned int st_gid;
    unsigned int st_rdev;
    int st_size;
    unsigned int st_blksize;
    unsigned int st_blocks;
    unsigned int st_atime;
    unsigned int st_mtime;
    unsigned int st_ctime;
};

int fstat(int fd, struct stat *buf);
int stat(const char *path, struct stat *buf);

#endif
