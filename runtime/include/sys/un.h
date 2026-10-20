#ifndef SLOW32_SYS_UN_H
#define SLOW32_SYS_UN_H

#include <sys/socket.h>

/* The address of a local socket.  The machine has none (socket() refuses
 * AF_UNIX); the type is here so programs that offer them compile. */
struct sockaddr_un {
    sa_family_t sun_family;
    char sun_path[108];
};

#endif
