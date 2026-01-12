// SLOW-32 Filesystem Stub Operations (for DEBUG build)
// These functions always return -1 or NULL to indicate failure

#include <stddef.h>
#include "sys/stat.h"
#include "unistd.h"

int unlink(const char *pathname) {
    (void)pathname;
    return -1;
}

int rename(const char *oldpath, const char *newpath) {
    (void)oldpath;
    (void)newpath;
    return -1;
}

int mkdir(const char *pathname, mode_t mode) {
    (void)pathname;
    (void)mode;
    return -1;
}

int rmdir(const char *pathname) {
    (void)pathname;
    return -1;
}

int lstat(const char *path, struct stat *buf) {
    (void)path;
    (void)buf;
    return -1;
}

int access(const char *pathname, int mode) {
    (void)pathname;
    (void)mode;
    return -1;
}

int chdir(const char *path) {
    (void)path;
    return -1;
}

char *getcwd(char *buf, size_t size) {
    (void)buf;
    (void)size;
    return NULL;
}

int remove(const char *pathname) {
    (void)pathname;
    return -1;
}
