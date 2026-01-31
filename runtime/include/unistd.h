#ifndef SLOW32_UNISTD_H
#define SLOW32_UNISTD_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

// Access mode constants for access()
#define F_OK 0  // File exists
#define X_OK 1  // Execute permission
#define W_OK 2  // Write permission
#define R_OK 4  // Read permission

// Sleep functions
unsigned int sleep(unsigned int seconds);
int usleep(unsigned int usec);

// File/directory operations
int access(const char *pathname, int mode);
int chdir(const char *path);
char *getcwd(char *buf, size_t size);
int unlink(const char *pathname);
int rmdir(const char *pathname);
int mkdir(const char *pathname, unsigned int mode);
int open(const char *pathname, int flags, ...);
int close(int fd);

#ifdef __cplusplus
}
#endif

#endif // SLOW32_UNISTD_H
