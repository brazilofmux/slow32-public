/* The string functions the host need not have, or whose text is the
 * library's own: memrchr, strcasestr, and what strerror and perror say
 * (the two libraries here must say the same). */
#include <stdio.h>
#include <string.h>
#include <errno.h>

int main(void) {
    static char buf[32];
    static int codes[] = {0, EPERM, ENOENT, EIO, EBADF, ENOMEM, EACCES, EEXIST, ENOTDIR, EISDIR, EINVAL,
                          EDOM, ERANGE, ENOSYS, 9999, -1};
    char *p;
    int i;
    strcpy(buf, "abcabcabc");
    p = memrchr(buf, 'b', 9);   printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = memrchr(buf, 'b', 7);   printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = memrchr(buf, 'a', 1);   printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = memrchr(buf, 'z', 9);   printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = memrchr(buf, 'a', 0);   printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = memrchr(buf, 0, 10);    printf("memrchr %d\n", p ? (int)(p - buf) : -1);
    p = strcasestr("Hello, World", "WORLD");   printf("strcasestr %s\n", p ? p : "(null)");
    p = strcasestr("Hello, World", "");        printf("strcasestr %s\n", p ? p : "(null)");
    p = strcasestr("Hello", "hello!");         printf("strcasestr %s\n", p ? p : "(null)");
    for (i = 0; i < (int)(sizeof codes / sizeof codes[0]); i++) printf("strerror(%d) = %s\n", codes[i], strerror(codes[i]));
    fflush(stdout);
    errno = ENOENT;  perror("perror");
    errno = ERANGE;  perror("a longer prefix");
    errno = EACCES;  perror("");
    errno = EINVAL;  perror(0);
    errno = 0;       perror("no error");
    return 0;
}
