#ifndef SLOW32_NETINET_IN_H
#define SLOW32_NETINET_IN_H

#include <stdint.h>
#include <sys/socket.h>

#ifdef __cplusplus
extern "C" {
#endif

#define INADDR_ANY      ((in_addr_t)0x00000000u)
#define INADDR_LOOPBACK ((in_addr_t)0x7f000001u)

struct in_addr {
    in_addr_t s_addr;  /* network byte order, POSIX-style */
};

struct sockaddr_in {
    uint16_t sin_family;
    uint16_t sin_port;       /* network byte order */
    struct in_addr sin_addr; /* network byte order */
    char sin_zero[8];
};

#ifdef __cplusplus
}
#endif

#endif
