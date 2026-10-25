/* termios.h -- a terminal's local modes, as far as the machine carries
 * them: c_lflag, through the term service's GET_LFLAG and SET_LFLAG
 * (docs/SPEC.md 8.14.6).  Enough to read a password without echo, or a
 * key at a time without canonical input.  The other fields read as 0 and
 * are not sent; the input, output and control modes stay the host's.  A
 * descriptor that is not a terminal, or a host without those requests,
 * fails with ENOTTY.  The same interface as runtime/include/termios.h. */
#ifndef _TERMIOS_H
#define _TERMIOS_H

typedef unsigned int tcflag_t;
typedef unsigned char cc_t;
typedef unsigned int speed_t;

#define NCCS 32
struct termios {
    tcflag_t c_iflag;
    tcflag_t c_oflag;
    tcflag_t c_cflag;
    tcflag_t c_lflag;
    cc_t c_cc[NCCS];
};

/* c_lflag: the machine's bits, not any host's */
#define ECHO    0x01u
#define ECHOE   0x02u
#define ECHOK   0x04u
#define ECHONL  0x08u
#define ICANON  0x10u
#define ISIG    0x20u
#define IEXTEN  0x40u

/* tcsetattr's optional_actions */
#define TCSANOW   0
#define TCSADRAIN 1
#define TCSAFLUSH 2

int tcgetattr(int fd, struct termios *t);
int tcsetattr(int fd, int optional_actions, const struct termios *t);

#endif
