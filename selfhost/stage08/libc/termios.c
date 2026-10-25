/* termios.c -- tcgetattr, tcsetattr and isatty on the term service's
 * GET_LFLAG and SET_LFLAG (docs/SPEC.md 8.14.6, service version 3).  The
 * service is found by name the first time; a host without it, or from
 * before version 3, has no terminals (ENOTTY), as before.  A request the
 * host refuses leaves the host's errno: EBADF for a closed descriptor,
 * ENOTTY for one that is not a terminal. */

unsigned char *__s32_mmio_data(void);
int s32_mmio_request(unsigned int opcode, unsigned int length,
                     unsigned int offset, unsigned int status);
void *memset(void *d, int c, unsigned int n);
extern int errno;

#include <termios.h>

#define TM_SVC_REQUEST   0xF0
#define TM_GET_LFLAG     16
#define TM_SET_LFLAG     17
#define TM_LF_ALL        0x7Fu
#define TM_ENOTTY        25
#define TM_EINVAL        22

static int tm_state;            /* 0 not asked, 1 ready, -1 absent */
static unsigned int tm_base;

static int tm_ready(void) {
    unsigned char *d;
    unsigned int *w;
    unsigned int count;
    if (tm_state == 0) {
        d = __s32_mmio_data();
        d[0] = 't'; d[1] = 'e'; d[2] = 'r'; d[3] = 'm'; d[4] = 0;
        tm_state = -1;
        if (s32_mmio_request(TM_SVC_REQUEST, 5, 0, 0) != -1) {
            w = (unsigned int *)d;      /* result, base opcode, count, version */
            count = w[2];
            if (w[0] == 0 && count > TM_SET_LFLAG) {
                tm_base = w[1];
                tm_state = 1;
            }
        }
    }
    if (tm_state != 1) {
        errno = TM_ENOTTY;
        return 0;
    }
    return 1;
}

int tcgetattr(int fd, struct termios *t) {
    int r;
    if (!tm_ready()) return -1;
    r = s32_mmio_request(tm_base + TM_GET_LFLAG, 0, (unsigned int)fd, 0);
    if (r == -1) return -1;
    memset(t, 0, sizeof(struct termios));
    t->c_lflag = (tcflag_t)r;
    return 0;
}

int tcsetattr(int fd, int optional_actions, const struct termios *t) {
    if (optional_actions < TCSANOW || optional_actions > TCSAFLUSH ||
        (t->c_lflag & ~TM_LF_ALL) != 0) {
        errno = TM_EINVAL;
        return -1;
    }
    if (!tm_ready()) return -1;
    if (s32_mmio_request(tm_base + TM_SET_LFLAG, 0, (unsigned int)fd,
                         t->c_lflag | ((unsigned int)optional_actions << 16)) != 0)
        return -1;
    return 0;
}

int isatty(int fd) {
    struct termios t;
    return tcgetattr(fd, &t) == 0;
}
