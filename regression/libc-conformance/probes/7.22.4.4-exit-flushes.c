/* 7.22.4.4 exit: after the handlers run, unwritten buffered output is flushed, including output the handlers made */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

static char buf[4096];
static void bye(void) { printf(" handler-text"); }
int main(void) {
    FILE *f;
    setvbuf(stdout, buf, _IOFBF, sizeof buf);
    atexit(bye);
    printf("buffered, no newline");
    f = tmpfile();
    if (f) fputs("x", f);              /* left open: exit closes it */
    exit(3);
}
