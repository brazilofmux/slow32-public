/* Test libc functions via the linker pipeline (no #include needed) */

int write(int fd, char *buf, int len);
int strlen(char *s);
int strcmp(char *a, char *b);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
char *malloc(int size);
int lseek(int fd, int offset, int whence);
int printf(char *fmt, int a0, int a1, int a2, int a3,
           int a4, int a5, int a6, int a7);
struct _file {
    int fd;
    int flags;
    char *buf;
    int buf_pos;
    int buf_len;
    int buf_dirty;
};
typedef struct _file FILE;
FILE *fopen(char *path, char *mode);
int fclose(FILE *f);
int fflush(FILE *f);
int fread(char *ptr, int size, int count, FILE *f);
int fwrite(char *ptr, int size, int count, FILE *f);

int main(int argc, char **argv) {
    char buf[64];
    char *p;
    FILE *f;

    /* Test write */
    write(1, "Hello from linked libc!\n", 24);

    /* Test string functions */
    if (strlen("hello") != 5) return 1;
    if (strcmp("abc", "abc") != 0) return 2;
    if (strcmp("abc", "abd") >= 0) return 3;

    /* Test memcpy */
    memcpy(buf, "test123", 8);
    if (strcmp(buf, "test123") != 0) return 4;

    /* Test memset */
    memset(buf, 65, 5);
    buf[5] = 0;
    if (strcmp(buf, "AAAAA") != 0) return 5;

    /* Test malloc */
    p = malloc(100);
    if (!p) return 6;
    memcpy(p, "dynamic!", 9);
    if (strcmp(p, "dynamic!") != 0) return 7;

    /* Test printf */
    printf("argc=%d str=%s hex=0x%x\n", argc, "ok", 255, 0, 0, 0, 0, 0);

    /* Test w+ mode: same stream must support both writing and reading */
    f = fopen("/tmp/test_libc_link.tmp", "w+");
    if (!f) return 8;
    if (fwrite("xy", 1, 2, f) != 2) return 9;
    fflush(f);
    if (lseek(f->fd, 0, 0) != 0) return 10;
    if (fread(buf, 1, 2, f) != 2) return 11;
    if (buf[0] != 'x' || buf[1] != 'y') return 12;

    /* fread with zero size/count must return 0 without faulting */
    if (fread(buf, 0, 4, f) != 0) return 13;
    if (fread(buf, 4, 0, f) != 0) return 14;
    fclose(f);

    write(1, "All tests passed!\n", 18);
    return 0;
}
