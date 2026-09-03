/* A request that needs a reply comes back through the queue, not as a
 * call into the instance (docs/plans/dpc.md second demo).  The dest is
 * the caller's own buffer, not the MMIO bounce, so a printf between
 * post and harvest cannot eat the flow. */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "s32dpc.h"

/* Mailboxes in .bss: a flow owns its buffer; the stack is the instance. */
static char a[16];
static char b[16];
static char tmp[8];

int main(void) {
    s32_dpc_t d;
    int fd, w;
    const char *msg = "hello-flow\n";

    w = open("flow.txt", O_WRONLY | O_CREAT | O_TRUNC);
    if (w < 0) { printf("BAD: create\n"); return 1; }
    if (write(w, msg, 11) != 11) { printf("BAD: write\n"); return 1; }
    close(w);

    fd = open("flow.txt", O_RDONLY);
    if (fd < 0) { printf("BAD: open\n"); return 1; }

    if (s32_dpc_poll(&d)) printf("BAD: an entry before anything was posted\n");

    memset(a, 0, sizeof a);
    if (s32_post_read(fd, a, 16, 0xA1) != 0) { printf("BAD: post a\n"); return 1; }
    printf("posted a\n");

    if (s32_dpc_wait(&d) != 1) { printf("BAD: wait a\n"); return 1; }
    printf("dpc kind=%x n=%u cookie=%x\n", d.kind, d.length, d.cookie);
    if (d.id != (unsigned)(unsigned long)a) printf("BAD: dest is not a\n");
    printf("bytes=%s", a);

    {
        int fd2 = open("flow.txt", O_RDONLY);
        if (fd2 < 0) { printf("BAD: open2\n"); return 1; }
        memset(b, 0, sizeof b);
        if (s32_post_read(fd2, b, 16, 0xB2) != 0) { printf("BAD: post b\n"); return 1; }
        printf("posted b\n");
        if (s32_dpc_wait(&d) != 1) { printf("BAD: wait b\n"); return 1; }
        printf("dpc kind=%x n=%u cookie=%x\n", d.kind, d.length, d.cookie);
        if (d.id != (unsigned)(unsigned long)b) printf("BAD: dest is not b\n");
        printf("bytes=%s", b);
        close(fd2);
    }

    if (s32_dpc_poll(&d)) printf("BAD: extra entry\n"); else printf("queue empty\n");

    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        if (s < 0) { printf("BAD: socket\n"); return 1; }
        if (s32_post_read(s, tmp, 8, 0xC3) == 0) printf("BAD: blocked fd was taken\n");
        else printf("would-block refused\n");
        if (s32_dpc_wait(&d) < 0) printf("nothing can arrive\n");
        close(s);
    }

    close(fd);
    printf("done\n");
    return 0;
}
