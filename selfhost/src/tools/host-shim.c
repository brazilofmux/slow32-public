/* Host shim: lets selfhost/src/tools/s32-as.c build natively for fast
 * iteration.  The fd* layer is the selfhost libc's file API; on the
 * host it maps straight onto POSIX fds. */
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>

void fdputc(int c, int fd) { char b = (char)c; write(fd, &b, 1); }
void fdputs(const char *s, int fd) { write(fd, s, strlen(s)); }
void fdputuint(int fd, unsigned int v) { char b[16]; snprintf(b, 16, "%u", v); write(fd, b, strlen(b)); }

int fdopen_path(const char *path, const char *mode) {
    if (mode && mode[0] == 'w')
        return openat(AT_FDCWD, path, O_WRONLY | O_CREAT | O_TRUNC, 0644);
    return openat(AT_FDCWD, path, O_RDONLY);
}
int fdclose(int fd) { return close(fd); }
int fdwrite(const char *buf, int sz, int count, int fd) {
    return (int)write(fd, buf, (size_t)sz * (size_t)count);
}
int fdtell(int fd) { return (int)lseek(fd, 0, SEEK_CUR); }
int fdread(char *buf, int sz, int count, int fd) {
    return (int)read(fd, buf, (size_t)sz * (size_t)count);
}
int fdseek(int fd, int off, int whence) {
    return (int)lseek(fd, off, whence);
}
char *fdgets(char *buf, int n, int fd) {
    int i = 0; char c;
    while (i < n - 1) {
        if (read(fd, &c, 1) != 1) break;
        buf[i++] = c;
        if (c == '\n') break;
    }
    if (i == 0) return 0;
    buf[i] = 0;
    return buf;
}
