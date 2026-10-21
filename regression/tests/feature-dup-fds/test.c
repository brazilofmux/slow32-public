/* dup, dup2 and F_DUPFD: a second descriptor for one open file, sharing
 * its offset; the lowest free number; dup2 onto an open descriptor closes
 * it first, onto itself does nothing, from a closed one fails and leaves
 * the target alone; closing one leaves the other.  Every line is what
 * glibc and macOS print. */
#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

int main(void) {
    const char *path = "dupfds.tmp";
    int a = open(path, O_RDWR | O_CREAT | O_TRUNC, 0644);
    write(a, "harbor-lights", 13);
    lseek(a, 0, SEEK_SET);

    int b = dup(a);
    printf("dup: new descriptor %d, above the first %d\n", b >= 0, b > a);
    char buf[16] = {0};
    read(a, buf, 6);
    int got = (int)read(b, buf + 6, 7);
    printf("shared offset: read %d more, \"%s\"\n", got, buf);

    int c = fcntl(a, F_DUPFD, 20);
    printf("F_DUPFD 20: %d\n", c);
    int d = fcntl(a, F_DUPFD, 20);
    printf("again: %d (20 is taken)\n", d);

    int other = open(path, O_RDONLY);
    int r = dup2(a, other);
    printf("dup2 onto an open fd: %d same %d\n", r, r == other);
    lseek(a, 2, SEEK_SET);
    memset(buf, 0, sizeof buf);
    read(other, buf, 4);
    printf("it is the first one now: \"%s\"\n", buf);

    printf("dup2 onto itself: %d\n", dup2(a, a) == a);
    close(d);
    errno = 0;
    r = dup2(d, c);
    int e = errno;
    printf("dup2 from a closed fd: %d EBADF %d, target still open %d\n", r, e == EBADF, fcntl(c, F_GETFD) >= 0);
    errno = 0;
    r = dup(9999);
    e = errno;
    printf("dup of a bad fd: %d EBADF %d\n", r, e == EBADF);

    mkdir("dupdir.tmp", 0755);
    DIR *dp = opendir("dupdir.tmp");
    int dfd = dirfd(dp);
    printf("dirfd F_GETFD: %d\n", dfd >= 0 && fcntl(dfd, F_GETFD) >= 0);
    int onto = dup2(c, dfd);
    printf("dup2 onto dirfd: %d same %d still open %d\n", onto >= 0, onto == dfd, fcntl(dfd, F_GETFD) >= 0);
    rmdir("dupdir.tmp");

    close(a);
    lseek(b, 0, SEEK_SET);
    memset(buf, 0, sizeof buf);
    printf("after closing the first, the dup still reads %d\n", (int)read(b, buf, 6));
    close(b); close(c); close(other);
    unlink(path);
    printf("fd constants %d %d %d\n", STDIN_FILENO, STDOUT_FILENO, STDERR_FILENO);
    return 0;
}
