/* A request that needs a reply comes back through the queue, not as a
 * call into the instance (docs/plans/dpc.md).  The dest is the callers
 * own buffer, not the MMIO bounce.  A would-block fd is taken into a
 * POST_MAX slot and completes at a later service point -- close, or a
 * later write on the peer -- still one stack. */
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "s32dpc.h"

/* Mailboxes in .bss: a flow owns its buffer; the stack is the instance. */
static char a[16];
static char b[16];
static char tmp[8];
static char peer[16];

int main(void) {
    s32_dpc_t d;
    int fd, w, s, ls, cli, acc;
    struct sockaddr_in addr;
    socklen_t alen;
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

    /* Unconnected SOCK_STREAM is not readable. The flow is taken, not
     * refused. close() is a later service point: 0-byte DPC. Do not wait
     * first -- POLL would sleep on a fd that this stack alone can close. */
    s = socket(AF_INET, SOCK_STREAM, 0);
    if (s < 0) { printf("BAD: socket\n"); return 1; }
    memset(tmp, 0, sizeof tmp);
    if (s32_post_read(s, tmp, 8, 0xC3) != 0) { printf("BAD: blocked fd refused\n"); return 1; }
    printf("posted blocked\n");
    close(s);
    printf("closed blocked\n");
    if (s32_dpc_wait(&d) != 1) { printf("BAD: wait blocked\n"); return 1; }
    printf("dpc kind=%x n=%u cookie=%x\n", d.kind, d.length, d.cookie);
    if (d.id != (unsigned)(unsigned long)tmp) printf("BAD: dest is not tmp\n");
    if (s32_dpc_poll(&d)) printf("BAD: extra after close\n"); else printf("queue empty\n");
    if (s32_dpc_wait(&d) < 0) printf("nothing can arrive\n");

    /* Loopback: post on the accepted fd, write from the client at the
     * next YIELD, harvest. Same opcode. Still one stack. */
    ls = socket(AF_INET, SOCK_STREAM, 0);
    cli = socket(AF_INET, SOCK_STREAM, 0);
    if (ls < 0 || cli < 0) { printf("BAD: loop sockets\n"); return 1; }
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    inet_aton("127.0.0.1", &addr.sin_addr);
    addr.sin_port = htons(0);
    if (bind(ls, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        printf("BAD: bind\n"); return 1;
    }
    if (listen(ls, 4) < 0) { printf("BAD: listen\n"); return 1; }
    alen = sizeof(addr);
    if (getsockname(ls, (struct sockaddr *)&addr, &alen) < 0) {
        printf("BAD: getsockname\n"); return 1;
    }
    if (connect(cli, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        printf("BAD: connect\n"); return 1;
    }
    acc = accept(ls, 0, 0);
    if (acc < 0) { printf("BAD: accept\n"); return 1; }
    printf("listen-ok\n");

    memset(peer, 0, sizeof peer);
    if (s32_post_read(acc, peer, 16, 0xD4) != 0) { printf("BAD: post peer\n"); return 1; }
    printf("posted peer\n");
    if (write(cli, msg, 11) != 11) { printf("BAD: write peer\n"); return 1; }
    printf("wrote peer\n");
    if (s32_dpc_wait(&d) != 1) { printf("BAD: wait peer\n"); return 1; }
    printf("dpc kind=%x n=%u cookie=%x\n", d.kind, d.length, d.cookie);
    if (d.id != (unsigned)(unsigned long)peer) printf("BAD: dest is not peer\n");
    printf("bytes=%s", peer);

    close(acc);
    close(cli);
    close(ls);
    close(fd);
    printf("done\n");
    return 0;
}
