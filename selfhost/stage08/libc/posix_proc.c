/* posix_proc.c -- abort, assert, signal, getenv.  Split from
 * posix_more.c so getenv does not drag fabs into the link
 * (GitHub issue 65).  Plain C: stage07 compiles this libc too. */

unsigned char *__s32_mmio_data(void);
int s32_mmio_request(unsigned int opcode, unsigned int length,
                     unsigned int offset, unsigned int status);
int strlen(const char *s);
int write(int fd, const void *buf, int n);
void exit(int status);
void *memcpy(void *d, const void *s, unsigned int n);
char *realloc(char *ptr, int size);

#define MMIO_DATA           (__s32_mmio_data())
#define MMIO_DATA_CAPACITY  (48 * 1024)
#define MMIO_OP_GETENV      0x64

void abort(void) {
    exit(134);
}

static void af_puts(const char *z) {
    write(2, z, strlen(z));
}

void __assert_fail(const char *expr, const char *file, int line) {
    char num[12];
    int i;
    int n;
    af_puts("assertion failed: ");
    af_puts(expr);
    af_puts(" (");
    af_puts(file);
    af_puts(":");
    n = line;
    i = 11;
    num[i] = 0;
    if (n == 0) { i = i - 1; num[i] = 48; }
    while (n > 0) { i = i - 1; num[i] = 48 + n % 10; n = n / 10; }
    af_puts(num + i);
    af_puts(")\n");
    abort();
}

/* signal: no signals are ever delivered on SLOW-32; accept the handler
 * (the shell installs one for SIGINT) and report the previous as default. */
typedef void (*pm_sighandler)(int);
pm_sighandler signal(int sig, pm_sighandler fn) {
    (void)sig; (void)fn;
    return (pm_sighandler)0;
}

/* getenv over the GETENV request (selfhost ISSUES-68, GitHub issue 55).
 * Protocol: the name (with its NUL) goes in the data buffer, length is the
 * byte count; the reply puts the value at the same offset and returns its
 * length, or fails when the name is not set. */
static char *pm_env_buf;
static int   pm_env_cap;

char *getenv(const char *name) {
    unsigned int len;
    int vlen;
    const char *p;
    char *nb;

    if (name == 0 || name[0] == 0) return (char *)0;
    /* A name containing '=' can never be set (C11 7.22.4.6). */
    p = name;
    while (*p != 0) {
        if (*p == 61) return (char *)0;
        p = p + 1;
    }
    len = strlen(name) + 1;
    if (len > MMIO_DATA_CAPACITY) return (char *)0;
    memcpy(MMIO_DATA, name, len);
    vlen = s32_mmio_request(MMIO_OP_GETENV, len, 0, 0);
    if (vlen < 0) return (char *)0;          /* not set */
    if (vlen > MMIO_DATA_CAPACITY) return (char *)0;
    if (vlen >= pm_env_cap) {
        nb = realloc(pm_env_buf, vlen + 1);
        if (nb == 0) return (char *)0;
        pm_env_buf = nb;
        pm_env_cap = vlen + 1;
    }
    memcpy(pm_env_buf, MMIO_DATA, vlen);
    pm_env_buf[vlen] = 0;
    return pm_env_buf;
}
