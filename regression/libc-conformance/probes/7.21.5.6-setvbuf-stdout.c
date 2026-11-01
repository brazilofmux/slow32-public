/* 7.21.5.6 setvbuf: stdout fully buffered still delivers everything, in order, by normal termination */
#include <stdio.h>

static char buf[64];

int main(void) {
    setvbuf(stdout, buf, _IOFBF, sizeof buf);
    for (int i = 0; i < 40; i++) printf("line %d of buffered output\n", i);
    fputs("no trailing newline", stdout);
    return 0;
}
