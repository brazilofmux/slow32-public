/* <termios.h> and isatty on the term service's GET_LFLAG and SET_LFLAG
 * (docs/SPEC.md 8.14, sub-opcodes 16 and 17, and 8.14.6), where nothing is
 * a terminal: the harness gives a test /dev/null and files.  Each standard
 * descriptor says ENOTTY, a closed one EBADF, and tcsetattr refuses an
 * action or a mode the machine has not got before it asks.  What a real
 * terminal does is checked by hand under a pty (the termios commit). */
#include <errno.h>
#include <stdio.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>

static void tty(int fd)
{
    errno = 0;
    int r = isatty(fd);
    printf("isatty(%d) = %d, errno %s\n", fd, r, errno == ENOTTY ? "ENOTTY" : errno == EBADF ? "EBADF" : errno ? "other" : "0");
}

static void get(int fd)
{
    struct termios t;
    memset(&t, 0x55, sizeof t);
    errno = 0;
    int r = tcgetattr(fd, &t);
    printf("tcgetattr(%d) = %d, errno %s\n", fd, r, errno == ENOTTY ? "ENOTTY" : errno == EBADF ? "EBADF" : errno ? "other" : "0");
}

static void set(int fd, int when, tcflag_t lflag)
{
    struct termios t;
    memset(&t, 0, sizeof t);
    t.c_lflag = lflag;
    errno = 0;
    int r = tcsetattr(fd, when, &t);
    printf("tcsetattr(%d, %d, %#x) = %d, errno %s\n", fd, when, lflag, r,
           errno == ENOTTY ? "ENOTTY" : errno == EBADF ? "EBADF" : errno == EINVAL ? "EINVAL" : errno ? "other" : "0");
}

int main(void)
{
    tty(0); tty(1); tty(2); tty(9); tty(1000);
    get(0); get(1); get(9);
    set(0, TCSANOW, ECHO | ICANON | ISIG);
    set(1, TCSAFLUSH, 0);
    set(9, TCSADRAIN, ICANON);
    set(0, 3, ECHO);                    /* no such action */
    set(0, -1, ECHO);
    set(0, TCSANOW, 0x80);              /* a mode the machine has not got */
    printf("bits %#x %#x %#x %#x %#x %#x %#x\n", ECHO, ECHOE, ECHOK, ECHONL, ICANON, ISIG, IEXTEN);
    return 0;
}
