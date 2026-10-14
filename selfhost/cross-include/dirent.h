/* dirent.h -- directory streams over getdents64 (cross builds).
 * struct dirent is linux_dirent64's head, so records copy as they are. */
#ifndef _DIRENT_H
#define _DIRENT_H
typedef struct __dir_stream {
    int  fd;
    int  pos;                 /* next record in buf */
    int  len;                 /* bytes in buf */
    char buf[4096];
} DIR;
struct dirent {
    unsigned long  d_ino;
    long           d_off;
    unsigned short d_reclen;
    unsigned char  d_type;
    char           d_name[256];
};
#define DT_UNKNOWN 0
#define DT_FIFO 1
#define DT_CHR 2
#define DT_DIR 4
#define DT_BLK 6
#define DT_REG 8
#define DT_LNK 10
#define DT_SOCK 12
DIR *opendir(const char *name);
struct dirent *readdir(DIR *dirp);
int closedir(DIR *dirp);
void rewinddir(DIR *dirp);
int dirfd(DIR *dirp);
#endif
