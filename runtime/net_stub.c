// DEBUG-build socket stubs. Networking requires MMIO.

#include "errno.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "arpa/inet.h"

uint32_t htonl(uint32_t x) {
    return ((x & 0x000000ffu) << 24) |
           ((x & 0x0000ff00u) << 8) |
           ((x & 0x00ff0000u) >> 8) |
           ((x & 0xff000000u) >> 24);
}

uint16_t htons(uint16_t x) {
    return (uint16_t)(((x & 0x00ffu) << 8) | ((x & 0xff00u) >> 8));
}

uint32_t ntohl(uint32_t x) { return htonl(x); }
uint16_t ntohs(uint16_t x) { return htons(x); }

int inet_aton(const char *cp, struct in_addr *inp) {
    (void)cp;
    (void)inp;
    return 0;
}

in_addr_t inet_addr(const char *cp) {
    (void)cp;
    return (in_addr_t)0xffffffffu;
}

char *inet_ntoa(struct in_addr in) {
    (void)in;
    return (char *)"0.0.0.0";
}

int socket(int domain, int type, int protocol) {
    (void)domain;
    (void)type;
    (void)protocol;
    errno = ENOSYS;
    return -1;
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
    (void)sockfd;
    (void)addr;
    (void)addrlen;
    errno = ENOSYS;
    return -1;
}

int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
    (void)sockfd;
    (void)addr;
    (void)addrlen;
    errno = ENOSYS;
    return -1;
}

int listen(int sockfd, int backlog) {
    (void)sockfd;
    (void)backlog;
    errno = ENOSYS;
    return -1;
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
    (void)sockfd;
    (void)addr;
    (void)addrlen;
    errno = ENOSYS;
    return -1;
}

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
    (void)sockfd;
    (void)addr;
    (void)addrlen;
    errno = ENOSYS;
    return -1;
}

int shutdown(int sockfd, int how) {
    (void)sockfd;
    (void)how;
    errno = ENOSYS;
    return -1;
}

int send(int sockfd, const void *buf, int len, int flags) {
    (void)sockfd;
    (void)buf;
    (void)len;
    (void)flags;
    errno = ENOSYS;
    return -1;
}

int recv(int sockfd, void *buf, int len, int flags) {
    (void)sockfd;
    (void)buf;
    (void)len;
    (void)flags;
    errno = ENOSYS;
    return -1;
}
