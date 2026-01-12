// SLOW-32 Directory Stub Operations (for DEBUG build)
// These functions always return -1 or NULL to indicate failure

#include <stddef.h>
#include "dirent.h"

DIR *opendir(const char *name) {
    (void)name;
    return NULL;
}

struct dirent *readdir(DIR *dirp) {
    (void)dirp;
    return NULL;
}

int closedir(DIR *dirp) {
    (void)dirp;
    return -1;
}

int dirfd(DIR *dirp) {
    (void)dirp;
    return -1;
}

void rewinddir(DIR *dirp) {
    (void)dirp;
}
