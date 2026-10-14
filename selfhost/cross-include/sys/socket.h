/* sys/socket.h -- Linux sockets (cross builds; values the same on
 * x86-64 and AArch64). */
#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H
#include <sys/types.h>
typedef unsigned int   socklen_t;
typedef unsigned short sa_family_t;
struct sockaddr {
    sa_family_t sa_family;
    char        sa_data[14];
};
struct sockaddr_storage {
    sa_family_t   ss_family;
    char          __ss_pad[118];
    unsigned long __ss_align;
};
#define AF_UNSPEC 0
#define AF_UNIX   1
#define AF_INET   2
#define AF_INET6  10
#define PF_INET   AF_INET
#define SOCK_STREAM 1
#define SOCK_DGRAM  2
#define SOCK_NONBLOCK 04000
#define SOCK_CLOEXEC  02000000
#define SOL_SOCKET   1
#define SO_REUSEADDR 2
#define SO_ERROR     4
#define SO_SNDBUF    7
#define SO_RCVBUF    8
#define SO_KEEPALIVE 9
#define MSG_PEEK     0x2
#define MSG_DONTWAIT 0x40
#define MSG_NOSIGNAL 0x4000
#define SHUT_RD   0
#define SHUT_WR   1
#define SHUT_RDWR 2
int socket(int domain, int type, int protocol);
int bind(int fd, const struct sockaddr *a, socklen_t len);
int listen(int fd, int backlog);
int accept(int fd, struct sockaddr *a, socklen_t *len);
int connect(int fd, const struct sockaddr *a, socklen_t len);
int getsockname(int fd, struct sockaddr *a, socklen_t *len);
int getpeername(int fd, struct sockaddr *a, socklen_t *len);
int setsockopt(int fd, int level, int name, const void *val, socklen_t len);
int getsockopt(int fd, int level, int name, void *val, socklen_t *len);
long send(int fd, const void *buf, unsigned long n, int flags);
long recv(int fd, void *buf, unsigned long n, int flags);
int shutdown(int fd, int how);
#endif
