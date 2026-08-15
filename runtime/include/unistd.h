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
int read(int fd, void *buf, size_t count);
int write(int fd, const void *buf, size_t count);
int lseek(int fd, int offset, int whence);
int ftruncate(int fd, int length);

/* Run another .s32x via the host emulator and wait. Returns the
 * child's exit code, or -1 on failure. argv[0] is ignored; path is
 * the image. Extra arguments start at argv[1]. argv may be NULL. */
int s32_execv(const char *path, char *const argv[]);

#ifdef __cplusplus
}
#endif

#endif // SLOW32_UNISTD_H
