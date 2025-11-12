#include "sys/stat.h"

int stat(const char *path, struct stat *buf) {
    (void)path;
    (void)buf;
    return -1;
}

int fstat(int fd, struct stat *buf) {
    (void)fd;
    (void)buf;
    return -1;
}
