/* misc.c — Miscellaneous libc functions */

int write(int fd, char *buf, int len);
void exit(int code);
int strlen(char *s);
int strncmp(char *a, char *b, int n);
char *malloc(int size);
char *memcpy(char *dst, char *src, int n);

/* ---- Environment ---- */

static char **saved_envp;
char **environ;     /* POSIX global; set in lockstep with saved_envp */

void file_sys_init(void);

void __save_envp(char **envp) {
    saved_envp = envp;
    environ = envp;
    /* Initialise stdio FILEs eagerly so that fprintf(stderr, ...) — which
     * binds the parameter before its body's lazy init — sees a non-NULL
     * stderr on the very first call. dbt's usage()/error paths hit this
     * before any printf would have triggered the lazy path. */
    file_sys_init();
}

char *getenv(char *name) {
    int nlen;
    char **ep;
    if (!saved_envp) return 0;
    nlen = strlen(name);
    ep = saved_envp;
    while (*ep) {
        if (strncmp(*ep, name, nlen) == 0 && (*ep)[nlen] == '=')
            return *ep + nlen + 1;
        ep = ep + 1;
    }
    return 0;
}

/* ---- Utility ---- */

void abort(void) {
    write(2, "abort\n", 6);
    exit(134);
}

int atoi(char *s) {
    int n;
    int neg;
    n = 0;
    neg = 0;
    while (*s == ' ' || *s == '\t' || *s == '\n') s = s + 1;
    if (*s == '-') { neg = 1; s = s + 1; }
    else if (*s == '+') s = s + 1;
    while (*s >= '0' && *s <= '9') {
        n = n * 10 + (*s - '0');
        s = s + 1;
    }
    if (neg) return 0 - n;
    return n;
}

int abs(int x) {
    if (x < 0) return 0 - x;
    return x;
}

/* ---- Character classification ---- */

int isspace(int c) {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r';
}

int isdigit(int c) {
    return c >= '0' && c <= '9';
}

int isalpha(int c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}

int toupper(int c) {
    if (c >= 'a' && c <= 'z') return c - 32;
    return c;
}

int tolower(int c) {
    if (c >= 'A' && c <= 'Z') return c + 32;
    return c;
}
