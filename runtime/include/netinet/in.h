#ifndef SLOW32_NETINET_IN_H
#define SLOW32_NETINET_IN_H

#include <stdint.h>
#include <sys/socket.h>

#ifdef __cplusplus
extern "C" {
#endif

#define INADDR_ANY       ((in_addr_t)0x00000000u)
#define INADDR_LOOPBACK  ((in_addr_t)0x7f000001u)
#define INADDR_BROADCAST ((in_addr_t)0xffffffffu)
#define INADDR_NONE      ((in_addr_t)0xffffffffu)

#define IPPROTO_IPV6 41
#define IPPROTO_UDP  17
#define IPV6_V6ONLY  26

#define INET_ADDRSTRLEN  16
#define INET6_ADDRSTRLEN 46

typedef uint16_t in_port_t;

struct in_addr {
    in_addr_t s_addr;  /* network byte order, POSIX-style */
};

struct sockaddr_in {
    uint16_t sin_family;
    uint16_t sin_port;       /* network byte order */
    struct in_addr sin_addr; /* network byte order */
    char sin_zero[8];
};

/* IPv6 addresses: inet_pton, inet_ntop and getaddrinfo convert them;
 * the machine's sockets do not carry them (socket() refuses AF_INET6). */
struct in6_addr {
    union {
        uint8_t  __u6_addr8[16];
        uint16_t __u6_addr16[8];
        uint32_t __u6_addr32[4];
    } __in6_u;
};
#define s6_addr __in6_u.__u6_addr8

struct sockaddr_in6 {
    uint16_t sin6_family;
    uint16_t sin6_port;      /* network byte order */
    uint32_t sin6_flowinfo;
    struct in6_addr sin6_addr;
    uint32_t sin6_scope_id;
};

#define IN6ADDR_ANY_INIT      { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } }
#define IN6ADDR_LOOPBACK_INIT { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } }
extern const struct in6_addr in6addr_any;
extern const struct in6_addr in6addr_loopback;

#define IN6_IS_ADDR_UNSPECIFIED(a) \
    ((a)->__in6_u.__u6_addr32[0] == 0 && (a)->__in6_u.__u6_addr32[1] == 0 && \
     (a)->__in6_u.__u6_addr32[2] == 0 && (a)->__in6_u.__u6_addr32[3] == 0)
#define IN6_IS_ADDR_LOOPBACK(a) \
    ((a)->__in6_u.__u6_addr32[0] == 0 && (a)->__in6_u.__u6_addr32[1] == 0 && \
     (a)->__in6_u.__u6_addr32[2] == 0 && (a)->s6_addr[12] == 0 && \
     (a)->s6_addr[13] == 0 && (a)->s6_addr[14] == 0 && (a)->s6_addr[15] == 1)
#define IN6_IS_ADDR_V4MAPPED(a) \
    ((a)->__in6_u.__u6_addr32[0] == 0 && (a)->__in6_u.__u6_addr32[1] == 0 && \
     (a)->s6_addr[8] == 0 && (a)->s6_addr[9] == 0 && \
     (a)->s6_addr[10] == 0xff && (a)->s6_addr[11] == 0xff)

#ifdef __cplusplus
}
#endif

#endif
