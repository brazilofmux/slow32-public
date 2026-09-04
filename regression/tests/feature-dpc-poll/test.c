/* Wait-for-any: sleep until a timer fires or an fd is readable, whichever
 * comes first (docs/plans/dpc.md).  stdin is the pipe in stdin.sh:
 * unreadable for half a second, then one line, then EOF. */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "s32dpc.h"

static char pbuf[4];
static char mix[16];

static const char *why(unsigned c) {
    if (c & S32_DPC_NVAL) return "not open";
    if (c & (S32_DPC_IN | S32_DPC_HUP)) return "readable";
    return "?";
}

int main(void) {
    s32_dpc_t d;
    int in = 0;
    int nfd;

    /* A portable "always readable, reads EOF" fd: an empty regular file.
     * NOT /dev/null -- that is a character device, and macOS poll() reports
     * it POLLNVAL rather than POLLIN, so a POST or readiness wait on it never
     * completes there. */
    { int c = open("dpc_empty.tmp", O_WRONLY | O_CREAT | O_TRUNC, 0644);
      if (c >= 0) close(c); }

    /* 1: a short timer beats an fd that is not readable yet */
    s32_timer_start(0, 20000000u, 0x11);
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_TIMER)
        printf("timer first: id=%d cookie=%x\n", (int)d.id, d.cookie);
    else printf("BAD: expected the timer\n");

    /* 2: a POST already queued is harvested by wait_on; unread puts it
     *    back so a later poll recovers it instead of dropping it. */
    nfd = open("dpc_empty.tmp", O_RDONLY);
    if (nfd < 0) { printf("BAD: empty file\n"); return 1; }
    memset(pbuf, 0, sizeof pbuf);
    if (s32_post_read(nfd, pbuf, 4, 0x88) != 0) { printf("BAD: post null\n"); return 1; }
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_POST) {
        printf("post ahead of stdin\n");
        if (s32_dpc_unread(&d) != 0) { printf("BAD: unread\n"); return 1; }
    } else printf("BAD: expected post first\n");
    if (s32_dpc_poll(&d) && d.kind == S32_DPC_POST) printf("recovered post\n");
    else printf("BAD: did not recover post\n");
    close(nfd);

    /* 3: POST owns stdin: wait-for-any must not emit READY and steal
     *    the bytes. stdin.sh writes after 500 ms. */
    memset(mix, 0, sizeof mix);
    if (s32_post_read(in, mix, 16, 0x77) != 0) { printf("BAD: post stdin\n"); return 1; }
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_POST)
        printf("post owns stdin: n=%u\n", d.length);
    else if (d.kind == S32_DPC_READY) printf("BAD: ready stole the post\n");
    else printf("BAD: expected post on stdin\n");
    printf("bytes=%s", mix);

    /* 4: after the post consumed the line, the fd is at EOF */
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_READY)
        printf("fd %u %s at eof\n", d.id, why(d.cookie));
    {
        char buf[16];
        int n = (int)read(in, buf, sizeof buf - 1);
        printf("read %d\n", n);
    }

    /* 5: readiness beats a long timer when both could arrive: a file is
     *    always readable.  The armed timer is cancelled after. */
    {
        int f = open("dpc_empty.tmp", O_RDONLY);
        int t = s32_timer_start(2, 0, 0x44);
        if (s32_dpc_wait_on(&f, 1, &d) == 1 && d.kind == S32_DPC_READY && (int)d.id == f)
            printf("file ready before the long timer\n");
        else printf("BAD: expected the file\n");
        s32_timer_cancel(t);
        close(f);
    }

    /* 6: an fd that is not open is reported, not waited on */
    {
        int bad = 77;
        if (s32_dpc_wait_on(&bad, 1, &d) == 1 && d.kind == S32_DPC_READY)
            printf("fd %u %s\n", d.id, why(d.cookie));
    }

    /* 7: nothing armed and no fd: nothing could arrive */
    if (s32_dpc_wait_on(0, 0, &d) < 0) printf("nothing to wait for\n");

    /* 8: the partition: too many fds is refused before the host is asked */
    {
        int many[S32_DPC_MAX_FDS + 1];
        if (s32_dpc_wait_on(many, S32_DPC_MAX_FDS + 1, &d) < 0) printf("too many fds\n");
    }
    unlink("dpc_empty.tmp");
    return 0;
}
