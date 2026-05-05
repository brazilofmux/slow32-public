/* Tests string literals + the __syscall builtin (write to stdout, then exit).
 * Pass = stdout contains "hi a64\n" and exit code 0. */

int __syscall();

int strlen_local(char *s) {
    int n;
    n = 0;
    while (s[n]) n = n + 1;
    return n;
}

int main(void) {
    char *msg;
    msg = "hi a64\n";
    __syscall(64, 1, msg, strlen_local(msg), 0, 0, 0);  /* write(1, msg, len) */
    return 0;
}
