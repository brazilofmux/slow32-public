/* Minimal argv test */

int write(int fd, char *buf, int len);
int strlen(char *s);

char *getenv(char *name);

int main(int argc, char **argv) {
    char *s;
    int i;
    i = 0;
    while (i < argc) {
        s = argv[i];
        write(1, s, strlen(s));
        write(1, "\n", 1);
        i = i + 1;
    }
    s = getenv("HOME");
    if (s) {
        write(1, "HOME=", 5);
        write(1, s, strlen(s));
        write(1, "\n", 1);
    }
    return 0;
}
