/* posix_fs.c -- filesystem calls over the MMIO ring: access, ftruncate,
 * stat/lstat, mkdir, chdir, fsync, utimes, opendir/readdir/closedir.
 * Split from posix_more.c (GitHub issue 65).  Plain C: stage07 compiles
 * this libc too. */

/* The data buffer comes from mmio_no_start.s: stage07 turns any extern
 * object declaration into a definition, which overrode the linker's
 * absolute __mmio_base (selfhost ISSUES-67). */
unsigned char *__s32_mmio_data(void);
int s32_mmio_request(unsigned int opcode, unsigned int length,
                     unsigned int offset, unsigned int status);
int strlen(const char *s);
void *memcpy(void *d, const void *s, unsigned int n);
void *malloc(unsigned int n);
void free(void *p);

#define MMIO_DATA           (__s32_mmio_data())
#define MMIO_DATA_CAPACITY  (48 * 1024)
#define MMIO_OP_FTRUNCATE   0x0D
#define MMIO_OP_ACCESS      0x25
#define MMIO_OP_STAT        0x0A
#define MMIO_OP_MKDIR       0x22
#define MMIO_OP_LSTAT       0x24
#define MMIO_OP_CHDIR       0x26
#define MMIO_OP_OPENDIR     0x28
#define MMIO_OP_READDIR     0x29
#define MMIO_OP_CLOSEDIR    0x2A
#define MMIO_STAT_PATH      (-1)

int access(const char *path, int mode) {
    unsigned int len;
    if (path == 0) return -1;
    len = strlen(path) + 1;
    if (len > MMIO_DATA_CAPACITY) return -1;
    memcpy(MMIO_DATA, path, len);
    if (s32_mmio_request(MMIO_OP_ACCESS, len, 0, mode) != 0) return -1;
    return 0;
}

int ftruncate(int fd, int length) {
    int *p;
    if (fd < 0) return -1;
    p = (int *)MMIO_DATA;
    *p = length;
    if (s32_mmio_request(MMIO_OP_FTRUNCATE, 4, 0, fd) != 0) return -1;
    return 0;
}

static int pm_path_request(int op, const char *path, int arg) {
    unsigned int len;
    if (path == 0) return -1;
    len = strlen(path) + 1;
    if (len > MMIO_DATA_CAPACITY) return -1;
    memcpy(MMIO_DATA, path, len);
    return s32_mmio_request(op, len, 0, arg);
}

int stat(const char *path, void *st) {
    if (st == 0 || pm_path_request(MMIO_OP_STAT, path, MMIO_STAT_PATH) != 0) return -1;
    memcpy(st, MMIO_DATA, 112);
    return 0;
}

int lstat(const char *path, void *st) {
    if (st == 0 || pm_path_request(MMIO_OP_LSTAT, path, MMIO_STAT_PATH) != 0) return -1;
    memcpy(st, MMIO_DATA, 112);
    return 0;
}

int mkdir(const char *path, unsigned int mode) {
    return pm_path_request(MMIO_OP_MKDIR, path, mode) == 0 ? 0 : -1;
}

int chdir(const char *path) {
    return pm_path_request(MMIO_OP_CHDIR, path, 0) == 0 ? 0 : -1;
}

int fsync(int fd) { (void)fd; return 0; }               /* the host writes through */
int utimes(const char *path, const void *times) { (void)path; (void)times; return 0; }

struct pm_dir { int dd_fd; int dd_loc; };
struct pm_dirent { long d_ino; long d_off; unsigned short d_reclen; unsigned char d_type; char d_name[256]; };
static struct pm_dirent pm_cur_dirent;

struct pm_dir *opendir(const char *name) {
    int r;
    struct pm_dir *d;
    r = pm_path_request(MMIO_OP_OPENDIR, name, 0);
    if (r == -1) return 0;
    d = (struct pm_dir *)malloc(sizeof(struct pm_dir));
    if (d == 0) { s32_mmio_request(MMIO_OP_CLOSEDIR, 0, 0, r); return 0; }
    d->dd_fd = r;
    d->dd_loc = 0;
    return d;
}

struct pm_dirent *readdir(struct pm_dir *d) {
    int r;
    unsigned int *w;
    if (d == 0 || d->dd_fd < 0) return 0;
    r = s32_mmio_request(MMIO_OP_READDIR, 272, 0, d->dd_fd);
    if (r != 0) return 0;              /* EOF or error */
    w = (unsigned int *)MMIO_DATA;     /* d_ino lo/hi, d_type, d_namlen, d_name[256] */
    pm_cur_dirent.d_ino = (long)w[0];
    pm_cur_dirent.d_off = d->dd_loc;
    pm_cur_dirent.d_reclen = sizeof(struct pm_dirent);
    pm_cur_dirent.d_type = (unsigned char)w[2];
    memcpy(pm_cur_dirent.d_name, (char *)MMIO_DATA + 16, 256);
    pm_cur_dirent.d_name[255] = 0;
    d->dd_loc = d->dd_loc + 1;
    return &pm_cur_dirent;
}

int closedir(struct pm_dir *d) {
    int r;
    if (d == 0) return -1;
    r = s32_mmio_request(MMIO_OP_CLOSEDIR, 0, 0, d->dd_fd);
    free(d);
    return r == 0 ? 0 : -1;
}
