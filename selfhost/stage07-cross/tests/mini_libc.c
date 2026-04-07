/* mini_libc.c — minimal libc for linker testing.
 * Provides strlen and write via __syscall. */

int __syscall();

int write(int fd, char *buf, int len) {
    return __syscall(1, fd, buf, len, 0, 0, 0);
}

int strlen(char *s) {
    int n;
    n = 0;
    while (s[n]) n = n + 1;
    return n;
}
