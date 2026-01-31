#ifndef SLOW32_FCNTL_H
#define SLOW32_FCNTL_H

#define O_RDONLY 0x01
#define O_WRONLY 0x02
#define O_RDWR   0x03
#define O_APPEND 0x04
#define O_CREAT  0x08
#define O_TRUNC  0x10

int open(const char *pathname, int flags, ...);

#endif
