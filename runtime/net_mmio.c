// SLOW-32 IPv4 TCP sockets via MMIO.
// No DNS. Address conversion lives here; the host sees guest-endian
// s32_mmio_sockaddr_in_t payloads only.

#include <string.h>
#include <stdint.h>

#include "errno.h"
#include "mmio_ring.h"
#include "sys/socket.h"
#include "netinet/in.h"
#include "arpa/inet.h"
#include "unistd.h"

uint32_t htonl(uint32_t x) {
    return ((x & 0x000000ffu) << 24) |
           ((x & 0x0000ff00u) << 8) |
           ((x & 0x00ff0000u) >> 8) |
           ((x & 0xff000000u) >> 24);
}

uint16_t htons(uint16_t x) {
    return (uint16_t)(((x & 0x00ffu) << 8) | ((x & 0xff00u) >> 8));
}

uint32_t ntohl(uint32_t x) {
    return htonl(x);
}

uint16_t ntohs(uint16_t x) {
    return htons(x);
}

static int parse_octet(const char **pp, unsigned *out) {
    const char *p = *pp;
    if (*p < '0' || *p > '9') {
        return -1;
    }
    unsigned v = 0;
    int digits = 0;
    while (*p >= '0' && *p <= '9') {
        v = v * 10u + (unsigned)(*p - '0');
        if (v > 255u) {
            return -1;
        }
        p++;
        digits++;
        if (digits > 3) {
            return -1;
        }
    }
    if (digits == 0) {
        return -1;
    }
    *out = v;
    *pp = p;
    return 0;
}

int inet_aton(const char *cp, struct in_addr *inp) {
    if (!cp || !inp) {
        return 0;
    }
    const char *p = cp;
    unsigned a, b, c, d;
    if (parse_octet(&p, &a) < 0 || *p != '.') return 0;
    p++;
    if (parse_octet(&p, &b) < 0 || *p != '.') return 0;
    p++;
    if (parse_octet(&p, &c) < 0 || *p != '.') return 0;
    p++;
    if (parse_octet(&p, &d) < 0 || *p != '\0') return 0;
    inp->s_addr = htonl((a << 24) | (b << 16) | (c << 8) | d);
    return 1;
}

in_addr_t inet_addr(const char *cp) {
    struct in_addr addr;
    if (!inet_aton(cp, &addr)) {
        return (in_addr_t)0xffffffffu;
    }
    return addr.s_addr;
}

char *inet_ntoa(struct in_addr in) {
    static char buf[16];
    uint32_t a = ntohl(in.s_addr);
    unsigned o0 = (a >> 24) & 0xffu;
    unsigned o1 = (a >> 16) & 0xffu;
    unsigned o2 = (a >> 8) & 0xffu;
    unsigned o3 = a & 0xffu;
    /* Manual decimal so we do not pull in printf. */
    char *p = buf;
    unsigned oct[4] = { o0, o1, o2, o3 };
    int i;
    for (i = 0; i < 4; i++) {
        unsigned v = oct[i];
        char tmp[3];
        int n = 0;
        if (v == 0) {
            tmp[n++] = '0';
        } else {
            while (v > 0 && n < 3) {
                tmp[n++] = (char)('0' + (v % 10));
                v /= 10;
            }
        }
        while (n > 0) {
            *p++ = tmp[--n];
        }
        if (i < 3) {
            *p++ = '.';
        }
    }
    *p = '\0';
    return buf;
}

static int pack_sin(const struct sockaddr *addr, socklen_t addrlen,
                    s32_mmio_sockaddr_in_t *out) {
    const struct sockaddr_in *sin = (const struct sockaddr_in *)addr;
    if (!addr || !out || addrlen < 8) {
        errno = EINVAL;
        return -1;
    }
    if (sin->sin_family != AF_INET) {
        errno = EAFNOSUPPORT;
        return -1;
    }
    out->family = S32_AF_INET;
    out->port = ntohs(sin->sin_port);
    out->addr = ntohl(sin->sin_addr.s_addr);
    return 0;
}

static void unpack_sin(const s32_mmio_sockaddr_in_t *in,
                       struct sockaddr *addr, socklen_t *addrlen) {
    if (!addr || !addrlen) {
        return;
    }
    struct sockaddr_in sin;
    memset(&sin, 0, sizeof(sin));
    sin.sin_family = AF_INET;
    sin.sin_port = htons(in->port);
    sin.sin_addr.s_addr = htonl(in->addr);
    socklen_t n = sizeof(sin);
    if (*addrlen < n) {
        n = *addrlen;
    }
    memcpy(addr, &sin, n);
    *addrlen = sizeof(sin);
}

int socket(int domain, int type, int protocol) {
    unsigned status = ((unsigned)domain & 0xffu) |
                      (((unsigned)type & 0xffu) << 8) |
                      (((unsigned)protocol & 0xffu) << 16);
    int r = s32_mmio_request(S32_MMIO_OP_SOCKET, 0u, 0u, status);
    if (r < 0) {
        return -1;
    }
    return r;
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
    s32_mmio_sockaddr_in_t packed;
    if (pack_sin(addr, addrlen, &packed) < 0) {
        return -1;
    }
    memcpy((void *)S32_MMIO_DATA_BUFFER, &packed, sizeof(packed));
    int r = s32_mmio_request(S32_MMIO_OP_BIND, sizeof(packed), 0u,
                             (unsigned)sockfd);
    return (r < 0) ? -1 : 0;
}

int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
    int r = s32_mmio_request(S32_MMIO_OP_GETSOCKNAME,
                             sizeof(s32_mmio_sockaddr_in_t), 0u,
                             (unsigned)sockfd);
    if (r < 0) {
        return -1;
    }
    if (addr && addrlen) {
        s32_mmio_sockaddr_in_t packed;
        memcpy(&packed, (const void *)S32_MMIO_DATA_BUFFER, sizeof(packed));
        unpack_sin(&packed, addr, addrlen);
    }
    return 0;
}

int listen(int sockfd, int backlog) {
    unsigned bl = (backlog < 0) ? 0u : (unsigned)backlog;
    int r = s32_mmio_request(S32_MMIO_OP_LISTEN, bl, 0u, (unsigned)sockfd);
    return (r < 0) ? -1 : 0;
}

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
    s32_mmio_sockaddr_in_t packed;
    if (pack_sin(addr, addrlen, &packed) < 0) {
        return -1;
    }
    memcpy((void *)S32_MMIO_DATA_BUFFER, &packed, sizeof(packed));
    int r = s32_mmio_request(S32_MMIO_OP_CONNECT, sizeof(packed), 0u,
                             (unsigned)sockfd);
    return (r < 0) ? -1 : 0;
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
    int r = s32_mmio_request(S32_MMIO_OP_ACCEPT, sizeof(s32_mmio_sockaddr_in_t),
                             0u, (unsigned)sockfd);
    if (r < 0) {
        return -1;
    }
    if (addr && addrlen) {
        s32_mmio_sockaddr_in_t packed;
        memcpy(&packed, (const void *)S32_MMIO_DATA_BUFFER, sizeof(packed));
        unpack_sin(&packed, addr, addrlen);
    }
    return r;
}

int shutdown(int sockfd, int how) {
    int r = s32_mmio_request(S32_MMIO_OP_SHUTDOWN, (unsigned)how, 0u,
                             (unsigned)sockfd);
    return (r < 0) ? -1 : 0;
}

int send(int sockfd, const void *buf, int len, int flags) {
    (void)flags;
    if (len < 0) {
        errno = EINVAL;
        return -1;
    }
    return write(sockfd, buf, (size_t)len);
}

int recv(int sockfd, void *buf, int len, int flags) {
    (void)flags;
    if (len < 0) {
        errno = EINVAL;
        return -1;
    }
    return read(sockfd, buf, (size_t)len);
}
