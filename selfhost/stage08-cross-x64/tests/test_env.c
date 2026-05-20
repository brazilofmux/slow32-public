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
        printf("argv[%d]=%s\n", i, argv[i], 0, 0, 0, 0, 0, 0);
        i = i + 1;
    }

    home = getenv("HOME");
    if (home) {
        printf("HOME=%s\n", home, 0, 0, 0, 0, 0, 0, 0);
    } else {
        write(2, "getenv failed\n", 14);
        return 1;
    }

    return 0;
}
