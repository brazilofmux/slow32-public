/* Errno from the host reaches the guest in the guest's own (Linux)
 * numbering on any host (docs/SPEC.md 8.4.2), and a failing call keeps
 * its errno instead of collapsing to EIO.  rmdir of a non-empty directory
 * is ENOTEMPTY, 39 here and 66 on macOS: before s32_errno_from_host a Mac
 * host handed the guest 66.  fstat goes through the guest's fd table. */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/stat.h>

static int fails;
static void check(const char *what, int rc, int want)
{
    int e = errno;
    if (rc != -1 || e != want) {
        printf("FAIL %s: rc=%d errno=%d want %d\n", what, rc, e, want);
        fails++;
    } else {
        printf("ok %s: %s\n", what, strerror(e));
    }
}

int main(void)
{
    const char *d = "errno-host.dir", *f = "errno-host.dir/f", *g = "errno-host.f";
    struct stat st;

    rmdir(d); unlink(f); unlink(g);

    errno = 0; check("unlink missing", unlink("errno-host.missing"), ENOENT);
    errno = 0; check("stat missing", stat("errno-host.missing", &st), ENOENT);
    errno = 0; check("rmdir missing", rmdir("errno-host.missing"), ENOENT);

    if (mkdir(d, 0755) != 0) { printf("FAIL mkdir\n"); return 1; }
    errno = 0; check("mkdir existing", mkdir(d, 0755), EEXIST);
    FILE *fp = fopen(f, "w");
    if (!fp) { printf("FAIL fopen\n"); return 1; }
    fputs("x", fp); fclose(fp);
    errno = 0; check("rmdir non-empty", rmdir(d), ENOTEMPTY);
    unlink(f); rmdir(d);

    /* fstat through the table.  Closing guest fd 0 frees the slot but not
     * the host's stdin, so the next open is guest fd 0 on a host fd that is
     * not 0: the old host stat'ed its own stdin there. */
    fp = fopen(g, "w");
    fputs("hello, fstat", fp); fclose(fp);
    close(0);
    int b = open(g, O_RDONLY);
    if (b != 0) printf("note: open gave fd %d, not 0\n", b);
    if (b < 0 || fstat(b, &st) != 0 || st.st_size != 12) {
        printf("FAIL fstat size %ld\n", (long)st.st_size); fails++;
    } else {
        printf("ok fstat: size 12\n");
    }
    close(b); unlink(g);
    errno = 0; check("fstat closed", fstat(b, &st), EBADF);

    printf(fails ? "errno-host: FAILED\n" : "errno-host: all tests passed\n");
    return fails != 0;
}
