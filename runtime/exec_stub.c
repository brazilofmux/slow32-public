#include "errno.h"
#include "unistd.h"

int s32_execv(const char *path, char *const argv[]) {
    (void)path;
    (void)argv;
    errno = ENOSYS;
    return -1;
}
