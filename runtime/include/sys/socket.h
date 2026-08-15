#ifndef SLOW32_SYS_SOCKET_H
#define SLOW32_SYS_SOCKET_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define AF_INET       2
#define SOCK_STREAM   1
#define SHUT_RD       0
#define SHUT_WR       1
#define SHUT_RDWR     2
#define SOL_SOCKET    1
#define SO_REUSEADDR  2

#define IPPROTO_IP    0
#define IPPROTO_TCP   6

typedef unsigned socklen_t;
typedef uint32_t in_addr_t;

struct sockaddr {
    uint16_t sa_family;
    char sa_data[14];
};

int socket(int domain, int type, int protocol);
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int listen(int sockfd, int backlog);
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int shutdown(int sockfd, int how);
int send(int sockfd, const void *buf, int len, int flags);
int recv(int sockfd, void *buf, int len, int flags);

#ifdef __cplusplus
}
#endif

#endif
