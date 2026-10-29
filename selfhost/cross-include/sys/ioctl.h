/* sys/ioctl.h -- Linux ioctl(2) and the terminal requests (cross builds;
 * asm-generic numbers, the same on x86-64 and AArch64). */
#ifndef _SYS_IOCTL_H
#define _SYS_IOCTL_H
struct winsize {
    unsigned short ws_row;
    unsigned short ws_col;
    unsigned short ws_xpixel;
    unsigned short ws_ypixel;
};
#define TCGETS     0x5401
#define TCSETS     0x5402
#define TCSETSW    0x5403
#define TCSETSF    0x5404
#define TIOCGWINSZ 0x5413
#define FIONREAD   0x541B
#define FIONBIO    0x5421
int ioctl(int fd, unsigned long request, ...);
#endif
