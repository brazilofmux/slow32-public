/* fcntl.h -- open flags, as the MMIO host takes them
 * (common/mmio_ring_layout.h); open itself is declared in unistd.h. */
#ifndef _FCNTL_H
#define _FCNTL_H

#define O_RDONLY 0x01
#define O_WRONLY 0x02
#define O_RDWR   0x03
#define O_APPEND 0x04
#define O_CREAT  0x08
#define O_TRUNC  0x10

#endif
