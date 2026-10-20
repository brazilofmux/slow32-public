#ifndef SLOW32_SYS_SOCKET_H
#define SLOW32_SYS_SOCKET_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Linux's numbers.  The machine's sockets are IPv4 TCP only
 * (docs/SPEC.md): AF_INET6, AF_UNIX and SOCK_DGRAM are here so programs
 * that offer them compile and ask, and socket() refuses them. */
#define AF_UNSPEC     0
#define AF_UNIX       1
#define AF_LOCAL      AF_UNIX
#define AF_INET       2
#define AF_INET6      10
#define PF_UNSPEC     AF_UNSPEC
#define PF_UNIX       AF_UNIX
#define PF_LOCAL      AF_LOCAL
#define PF_INET       AF_INET
#define PF_INET6      AF_INET6
#define SOCK_STREAM   1
#define SOCK_DGRAM    2
#define SOCK_RAW      3
#define SOMAXCONN     128
#define SHUT_RD       0
#define SHUT_WR       1
#define SHUT_RDWR     2
/* The options the machine carries (docs/SPEC.md 8.9.2); any other is
 * ENOPROTOOPT */
#define SOL_SOCKET    1
#define SO_REUSEADDR  2
#define SO_TYPE       3
#define SO_ERROR      4
#define SO_SNDBUF     7
#define SO_RCVBUF     8
#define SO_KEEPALIVE  9
#define SO_LINGER     13

struct linger {
    int l_onoff;
    int l_linger;
};

#define IPPROTO_IP    0
#define IPPROTO_TCP   6

typedef unsigned socklen_t;
typedef uint32_t in_addr_t;
typedef uint16_t sa_family_t;

struct sockaddr {
    sa_family_t sa_family;
    char sa_data[14];
};

/* Big enough, and aligned enough, for any address above */
struct sockaddr_storage {
    sa_family_t ss_family;
    char __ss_pad[118];
    unsigned long long __ss_align;
};

int socket(int domain, int type, int protocol);
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int listen(int sockfd, int backlog);
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int setsockopt(int sockfd, int level, int optname, const void *optval, socklen_t optlen);
int getsockopt(int sockfd, int level, int optname, void *optval, socklen_t *optlen);
int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int shutdown(int sockfd, int how);
int send(int sockfd, const void *buf, int len, int flags);
int recv(int sockfd, void *buf, int len, int flags);

#ifdef __cplusplus
}
#endif

#endif
