/* <termios.h> where nothing is a terminal: the harness runs each test
 * with standard input from a file and output to a pipe.  isatty and
 * tcgetattr say ENOTTY for those and EBADF for a descriptor that was
 * never opened; tcsetattr refuses an unknown action or mode with EINVAL
 * before it asks the host.  .expect beside it: the host's own termios
 * has other bits, so there is no host leg. */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

static const char *why(void) {
    if (errno == 0) return "none";
    if (errno == ENOTTY) return "ENOTTY";
    if (errno == EBADF) return "EBADF";
    if (errno == EINVAL) return "EINVAL";
    return "something else";
}

static void probe(int fd) {
    struct termios t;
    int a;
    int g;
    errno = 0;
    a = isatty(fd);
    printf("fd %d: isatty %d (%s)", fd, a, why());
    memset(&t, 0xaa, sizeof t);
    errno = 0;
    g = tcgetattr(fd, &t);
    printf(", tcgetattr %d (%s)\n", g, why());
}

static void change(int fd, int when, unsigned int modes) {
    struct termios t;
    int r;
    memset(&t, 0, sizeof t);
    t.c_lflag = modes;
    errno = 0;
    r = tcsetattr(fd, when, &t);
    printf("set fd %d when %d modes 0x%x: %d (%s)\n", fd, when, modes, r, why());
}

int main(void) {
    probe(0);
    probe(1);
    probe(2);
    probe(17);
    change(1, TCSANOW, ECHO | ICANON);
    change(0, TCSADRAIN, ISIG | IEXTEN);
    change(17, TCSAFLUSH, ECHONL);
    change(1, 5, ECHO);
    change(1, -2, ECHO);
    change(1, TCSANOW, 0x100);
    printf("ECHO 0x%x ECHOE 0x%x ECHOK 0x%x ECHONL 0x%x ICANON 0x%x ISIG 0x%x IEXTEN 0x%x\n",
           ECHO, ECHOE, ECHOK, ECHONL, ICANON, ISIG, IEXTEN);
    return 0;
}
