/* The host-interface edges docs/SPEC.md used to record as quirks
 * (2026-10-03), each as it is now:
 *  - a zero-length WRITE succeeds with 0 (it failed with EINVAL);
 *  - a path sent without its NUL keeps its last byte;
 *  - open flags outside the SLOW-32 encoding are EINVAL (they went to the
 *    host's open as its own O_* bits);
 *  - a data-buffer offset past the end is EINVAL (it wrapped around);
 *  - getcwd into too small a buffer says ERANGE (it said EINVAL);
 *  - getchar at end of input leaves errno alone (it set EIO);
 *  - asking for an active service is CONFLICT (it granted a second
 *    session), and a released range is given out again. */
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include "../../../runtime/mmio_ring.h"

static int fails;
static void ok(int cond, const char *what)
{
    printf("%s %s\n", cond ? "ok" : "FAIL", what);
    if (!cond) fails++;
}

static unsigned svc(unsigned op, const char *name, unsigned *base)
{
    volatile unsigned char *d = S32_MMIO_DATA_BUFFER;
    unsigned n = (unsigned)strlen(name) + 1;
    for (unsigned i = 0; i < n; i++) d[i] = (unsigned char)name[i];
    s32_mmio_request(op, n, 0u, 0u);
    unsigned r = d[0] | d[1] << 8 | d[2] << 16 | (unsigned)d[3] << 24;
    if (base) *base = d[4] | d[5] << 8;
    return r;
}

int main(void)
{
    volatile unsigned char *d = S32_MMIO_DATA_BUFFER;
    const char *f = "host-edges.f";
    char cwd[2];

    unlink(f);
    FILE *fp = fopen(f, "w");
    fputs("x", fp); fclose(fp);

    unsigned r = (unsigned)s32_mmio_request(S32_MMIO_OP_WRITE, 0u, 0u, 1u);
    ok(r == 0, "zero-length WRITE is 0");

    /* "host-edges.f" without its NUL, flags READ */
    unsigned n = (unsigned)strlen(f);
    for (unsigned i = 0; i < n; i++) d[i] = (unsigned char)f[i];
    int fd = s32_mmio_request(S32_MMIO_OP_OPEN, n, 0u, 1u);
    ok(fd >= 0, "path without its NUL opens the whole name");
    if (fd >= 0) close(fd);

    errno = 0;
    ok(open(f, 0x100) == -1 && errno == EINVAL, "unknown open flag is EINVAL");

    errno = 0;
    r = (unsigned)s32_mmio_request(S32_MMIO_OP_GETTIME, 16u, 0xC000u, 0u);
    ok(r == 0xFFFFFFFFu && errno == EINVAL, "offset past the buffer is EINVAL");

    errno = 0;
    ok(getcwd(cwd, sizeof cwd) == NULL && errno == ERANGE, "small getcwd is ERANGE");

    errno = 0;
    ok(getchar() == EOF && errno == 0, "getchar at end of input keeps errno");

    unsigned b1 = 0, b2 = 0;
    ok(svc(0xF0, "term", &b1) == 0 && b1 == 0x80, "term granted at 0x80");
    ok(svc(0xF0, "term", NULL) == 3, "term again is CONFLICT");
    svc(0xF1, "term", NULL);
    ok(svc(0xF0, "term", &b2) == 0 && b2 == 0x80, "released range given out again");
    svc(0xF1, "term", NULL);

    /* the reply is written where the name was: a name at the very end of
     * the buffer leaves no room for it, and the request is refused (the
     * host used to write the grant past the buffer's end) */
    {
        unsigned cap = S32_MMIO_DATA_CAPACITY, b3 = 0;
        memcpy((void *)(d + cap - 5), "term", 5);
        ok(s32_mmio_request(0xF0, 5u, cap - 5, 0u) == (int)S32_MMIO_STATUS_ERR, "a grant that would not fit the buffer is refused");
        ok(svc(0xF0, "term", &b3) == 0 && b3 == 0x80, "and granted nothing");
        svc(0xF1, "term", NULL);
        memcpy((void *)(d + cap - 2), "t", 2);
        ok(s32_mmio_request(0xF2, 2u, cap - 2, 0u) == (int)S32_MMIO_STATUS_ERR, "a query whose answer would not fit is refused");
    }

    unlink(f);
    printf(fails ? "host-edges: FAILED\n" : "host-edges: all tests passed\n");
    return fails != 0;
}
