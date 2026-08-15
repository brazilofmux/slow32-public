#include "errno.h"
#include "unistd.h"

int s32_execv(const char *path, char *const argv[]) {
    (void)path;
    (void)argv;
    errno = ENOSYS;
    return -1;
}

int s32_execv_fd(const char *path, char *const argv[], int guest_fd) {
    (void)guest_fd;
    return s32_execv(path, argv);
}
