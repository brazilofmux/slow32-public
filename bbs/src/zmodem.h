#ifndef ZMODEM_H
#define ZMODEM_H

/* CRC-16 ZMODEM send subset (Forsberg). No 32-bit CRC, no resume
 * beyond ZRPOS 0. Good enough to hand a file to a matching receiver. */
int zmodem_send(int fd, const char *path);

#endif
