/* Wait-for-any: sleep until a timer fires or an fd is readable, whichever
 * comes first (docs/plans/dpc.md, the second demo).  The reply to "wake me
 * for this fd" is a DPC ring entry, not a call.  stdin is the pipe in
 * stdin.sh: unreadable for half a second, then one line, then EOF, so the
 * order below is the same on every run and every engine. */
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include "s32dpc.h"

static const char *why(unsigned c) {
    if (c & S32_DPC_NVAL) return "not open";
    if (c & (S32_DPC_IN | S32_DPC_HUP)) return "readable";
    return "?";
}

int main(void) {
    s32_dpc_t d;
    int in = 0;
    char buf[16];

    /* 1: a short timer beats an fd that is not readable yet */
    int t = s32_timer_start(0, 20000000u, 0x11);
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_TIMER)
        printf("timer first: id=%d cookie=%x\n", (int)d.id, d.cookie);
    else printf("BAD: expected the timer\n");

    /* 2: nothing armed: the wait is on the fd alone, and ends when the
     *    writer writes */
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_READY)
        printf("fd %u %s\n", d.id, why(d.cookie));
    else printf("BAD: expected readiness\n");
    int n = (int)read(in, buf, sizeof buf - 1);
    if (n > 0) { buf[n] = 0; printf("read %d: %s", n, buf); }

    /* 3: after the writer closes, the fd is readable again, at EOF */
    if (s32_dpc_wait_on(&in, 1, &d) == 1 && d.kind == S32_DPC_READY)
        printf("fd %u %s at eof\n", d.id, why(d.cookie));
    n = (int)read(in, buf, sizeof buf - 1);
    printf("read %d\n", n);

    /* 4: readiness beats a long timer when both could arrive: a file is
     *    always readable.  The armed timer is cancelled after. */
    int f = open("/dev/null", O_RDONLY);
    t = s32_timer_start(2, 0, 0x44);
    if (s32_dpc_wait_on(&f, 1, &d) == 1 && d.kind == S32_DPC_READY && (int)d.id == f)
        printf("file ready before the long timer\n");
    else printf("BAD: expected the file\n");
    s32_timer_cancel(t);
    close(f);

    /* 5: an fd that is not open is reported, not waited on */
    int bad = 77;
    if (s32_dpc_wait_on(&bad, 1, &d) == 1 && d.kind == S32_DPC_READY)
        printf("fd %u %s\n", d.id, why(d.cookie));

    /* 6: nothing armed and no fd: nothing could arrive */
    if (s32_dpc_wait_on(0, 0, &d) < 0) printf("nothing to wait for\n");

    /* 7: the partition: too many fds is refused before the host is asked */
    int many[S32_DPC_MAX_FDS + 1] = {0};
    if (s32_dpc_wait_on(many, S32_DPC_MAX_FDS + 1, &d) < 0) printf("too many fds\n");
    return 0;
}
