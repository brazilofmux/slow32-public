/* Test argc/argv/envp via the proper crt0 */

int write(int fd, char *buf, int len);
int strlen(char *s);
int printf(char *fmt, int a0, int a1, int a2, int a3,
           int a4, int a5, int a6, int a7);
char *getenv(char *name);

int main(int argc, char **argv) {
    char *home;
    int i;

    printf("argc=%d\n", argc, 0, 0, 0, 0, 0, 0, 0);

    i = 0;
    while (i < argc) {
        write(1, "argv: ", 6);
        write(1, argv[i], strlen(argv[i]));
        write(1, "\n", 1);
        i = i + 1;
    }

    home = getenv("HOME");
    if (home) {
        write(1, "HOME=", 5);
        write(1, home, strlen(home));
        write(1, "\n", 1);
    } else {
        write(2, "getenv failed\n", 14);
        return 1;
    }

    return 0;
}
