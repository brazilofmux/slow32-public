/* A request that needs a reply comes back through the queue, not as a
 * call into the instance (docs/plans/dpc.md second demo).  The instance
 * posts a read, prints, then harvests the DPC: one stack, no reader
 * thread.  The file is ready so the host completes at the posting
 * YIELD; the bytes are still a queue entry, not read()'s return. */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "s32dpc.h"

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

    /* dest sits above the stdio bounce at DATA_BUFFER[0], so a printf
     * between post and harvest does not eat the flow's bytes. */
    if (s32_post_read(fd, 256, 16, 0xA1) != 0) { printf("BAD: post a\n"); return 1; }
    printf("posted a\n");

    /* still one stack: this print happens before we look at the bytes */
    if (s32_dpc_wait(&d) != 1) { printf("BAD: wait a\n"); return 1; }
    printf("dpc kind=%x dest=%u n=%u cookie=%x\n", d.kind, d.id, d.length, d.cookie);
    {
        char buf[17];
        unsigned n = d.length < 16 ? d.length : 16;
        s32_post_copy(d.id, buf, n);
        buf[n] = 0;
        printf("bytes=%s", buf);
    }

    /* a second fd on the same file: another flow, different dest */
    {
        int fd2 = open("flow.txt", O_RDONLY);
        if (fd2 < 0) { printf("BAD: open2\n"); return 1; }
        if (s32_post_read(fd2, 320, 16, 0xB2) != 0) { printf("BAD: post b\n"); return 1; }
        printf("posted b\n");
        if (s32_dpc_wait(&d) != 1) { printf("BAD: wait b\n"); return 1; }
        printf("dpc kind=%x dest=%u n=%u cookie=%x\n", d.kind, d.id, d.length, d.cookie);
        close(fd2);
    }

    if (s32_dpc_poll(&d)) printf("BAD: extra entry\n"); else printf("queue empty\n");

    /* would-block is a refused flow, not a parked thread.  An unconnected
     * stream has no bytes: POLL says nothing can arrive. */
    {
        int s = socket(AF_INET, SOCK_STREAM, 0);
        if (s < 0) { printf("BAD: socket\n"); return 1; }
        if (s32_post_read(s, 384, 8, 0xC3) == 0) printf("BAD: blocked fd was taken\n");
        else printf("would-block refused\n");
        if (s32_dpc_wait(&d) < 0) printf("nothing can arrive\n");
        close(s);
    }

    close(fd);
    printf("done\n");
    return 0;
}
