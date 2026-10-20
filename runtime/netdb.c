/* Address text, and the lookups that need no resolver: inet_pton and
 * inet_ntop, getaddrinfo and getnameinfo, gethostbyname and friends.
 *
 * The machine has no name service and no services database, so a lookup
 * translates what the text itself says -- a numeric address, a numeric
 * port -- plus the one name every system knows without asking anyone,
 * "localhost" (RFC 6761).  Any other name is not found, and a service
 * name is not one getaddrinfo can resolve.  Nothing here touches the
 * machine, so both libc archives carry it.
 *
 * The machine's sockets are IPv4 TCP (docs/SPEC.md).  Where getaddrinfo
 * chooses what to return -- no family asked for, no socket type asked
 * for -- it returns what those sockets can use: IPv4 and SOCK_STREAM,
 * as AI_ADDRCONFIG would on a host with only IPv4 configured.  An IPv6
 * address written out, or a family or type asked for by name, is
 * translated all the same; socket() is where the machine says no.
 */
#include <errno.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/socket.h>

const struct in6_addr in6addr_any = IN6ADDR_ANY_INIT;
const struct in6_addr in6addr_loopback = IN6ADDR_LOOPBACK_INIT;

int h_errno;

/* network byte order without arpa/inet.h's functions, which live in the
 * socket objects */
static uint16_t be16(uint16_t x) { return (uint16_t)((x << 8) | (x >> 8)); }

static int hexval(int c) {
    if (c >= '0' && c <= '9') return c - '0';
    if (c >= 'a' && c <= 'f') return c - 'a' + 10;
    if (c >= 'A' && c <= 'F') return c - 'A' + 10;
    return -1;
}

/* four parts of decimal, each 0..255 and without a leading zero */
static int pton4(const char *src, uint8_t *dst) {
    uint8_t out[4];
    int parts = 0;
    while (parts < 4) {
        if (*src < '0' || *src > '9') return 0;
        unsigned v = 0;
        int digits = 0;
        while (*src >= '0' && *src <= '9') {
            if (digits == 1 && v == 0) return 0;
            v = v * 10u + (unsigned)(*src++ - '0');
            if (++digits > 3 || v > 255u) return 0;
        }
        out[parts++] = (uint8_t)v;
        if (parts < 4) {
            if (*src != '.') return 0;
            src++;
        }
    }
    if (*src != '\0') return 0;
    memcpy(dst, out, 4);
    return 1;
}

/* groups of up to four hex digits; one "::" for a run of zero groups; the
 * last 32 bits may be written as dotted decimal */
static int pton6(const char *src, uint8_t *dst) {
    uint8_t tmp[16];
    uint8_t *tp = tmp, *endp = tmp + 16, *colonp = NULL;
    const char *curtok;
    int digits = 0;
    unsigned val = 0;
    int ch;

    memset(tmp, 0, sizeof tmp);
    if (*src == ':' && *++src != ':') return 0;
    curtok = src;
    while ((ch = *src++) != '\0') {
        int d = hexval(ch);
        if (d >= 0) {
            if (++digits > 4) return 0;
            val = (val << 4) | (unsigned)d;
            continue;
        }
        if (ch == ':') {
            curtok = src;
            if (digits == 0) {
                if (colonp) return 0;
                colonp = tp;
                continue;
            }
            if (*src == '\0' || tp + 2 > endp) return 0;
            *tp++ = (uint8_t)(val >> 8);
            *tp++ = (uint8_t)val;
            digits = 0;
            val = 0;
            continue;
        }
        if (ch == '.' && tp + 4 <= endp && pton4(curtok, tp)) {
            tp += 4;
            digits = 0;
            break;
        }
        return 0;
    }
    if (digits) {
        if (tp + 2 > endp) return 0;
        *tp++ = (uint8_t)(val >> 8);
        *tp++ = (uint8_t)val;
    }
    if (colonp) {
        if (tp == endp) return 0;               /* "::" must stand for something */
        size_t n = (size_t)(tp - colonp);
        memmove(endp - n, colonp, n);
        memset(colonp, 0, (size_t)(endp - n - colonp));
        tp = endp;
    }
    if (tp != endp) return 0;
    memcpy(dst, tmp, 16);
    return 1;
}

int inet_pton(int af, const char *src, void *dst) {
    if (af == AF_INET) return pton4(src, (uint8_t *)dst);
    if (af == AF_INET6) return pton6(src, (uint8_t *)dst);
    errno = EAFNOSUPPORT;
    return -1;
}

static char *put_dec(char *p, unsigned v) {
    char tmp[3];
    int n = 0;
    do { tmp[n++] = (char)('0' + v % 10u); v /= 10u; } while (v);
    while (n) *p++ = tmp[--n];
    return p;
}

static char *ntop4_raw(const uint8_t *a, char *p) {
    for (int i = 0; i < 4; i++) {
        if (i) *p++ = '.';
        p = put_dec(p, a[i]);
    }
    return p;
}

/* RFC 5952: lower case, no leading zeros, the longest run of two or more
 * zero groups (the first, of equal runs) as "::".  An address whose first
 * 96 bits are zero, or ::ffff: before 32 bits, has its last 32 bits in
 * dotted decimal -- glibc's rule, which leaves "::" and "::1" alone */
static char *ntop6_raw(const uint8_t *a, char *p) {
    unsigned w[8];
    int best = -1, bestlen = 0, cur = -1, curlen = 0;
    for (int i = 0; i < 8; i++) {
        w[i] = ((unsigned)a[2 * i] << 8) | a[2 * i + 1];
        if (w[i] == 0) {
            if (cur < 0) { cur = i; curlen = 0; }
            if (++curlen > bestlen) { best = cur; bestlen = curlen; }
        } else {
            cur = -1;
        }
    }
    if (bestlen < 2) best = -1;
    for (int i = 0; i < 8; i++) {
        if (best >= 0 && i >= best && i < best + bestlen) {
            if (i == best) *p++ = ':';
            continue;
        }
        if (i) *p++ = ':';
        if (i == 6 && best == 0 && (bestlen == 6 || (bestlen == 5 && w[5] == 0xffffu))) {
            return ntop4_raw(a + 12, p);
        }
        static const char hex[] = "0123456789abcdef";
        int started = 0;
        for (int s = 12; s >= 0; s -= 4) {
            unsigned d = (w[i] >> s) & 0xfu;
            if (d || started || s == 0) { *p++ = hex[d]; started = 1; }
        }
    }
    if (best >= 0 && best + bestlen == 8) *p++ = ':';
    return p;
}

const char *inet_ntop(int af, const void *src, char *dst, socklen_t size) {
    char buf[INET6_ADDRSTRLEN];
    char *end;
    if (af == AF_INET) end = ntop4_raw((const uint8_t *)src, buf);
    else if (af == AF_INET6) end = ntop6_raw((const uint8_t *)src, buf);
    else { errno = EAFNOSUPPORT; return NULL; }
    size_t n = (size_t)(end - buf);
    if (n + 1 > size) { errno = ENOSPC; return NULL; }
    memcpy(dst, buf, n);
    dst[n] = '\0';
    return dst;
}

/* ---- getaddrinfo ---- */

static int is_localhost(const char *name) {
    return strcasecmp(name, "localhost") == 0 || strcasecmp(name, "localhost.") == 0;
}

/* one result: the addrinfo and its address in a single allocation, so
 * freeaddrinfo frees one block per entry (and the first's canonname) */
struct ai_block {
    struct addrinfo ai;
    union {
        struct sockaddr_in in4;
        struct sockaddr_in6 in6;
    } sa;
};

static struct addrinfo *make_ai(int family, const uint8_t *addr, uint16_t port,
                                int socktype, int protocol, int flags) {
    struct ai_block *b = calloc(1, sizeof *b);
    if (!b) return NULL;
    b->ai.ai_flags = flags;
    b->ai.ai_family = family;
    b->ai.ai_socktype = socktype;
    b->ai.ai_protocol = protocol;
    if (family == AF_INET) {
        b->sa.in4.sin_family = AF_INET;
        b->sa.in4.sin_port = be16(port);
        memcpy(&b->sa.in4.sin_addr, addr, 4);
        b->ai.ai_addrlen = sizeof b->sa.in4;
    } else {
        b->sa.in6.sin6_family = AF_INET6;
        b->sa.in6.sin6_port = be16(port);
        memcpy(&b->sa.in6.sin6_addr, addr, 16);
        b->ai.ai_addrlen = sizeof b->sa.in6;
    }
    b->ai.ai_addr = (struct sockaddr *)&b->sa;
    return &b->ai;
}

void freeaddrinfo(struct addrinfo *res) {
    while (res) {
        struct addrinfo *next = res->ai_next;
        free(res->ai_canonname);
        free(res);              /* the ai_block: ai is its first member */
        res = next;
    }
}

#define AI_KNOWN (AI_PASSIVE | AI_CANONNAME | AI_NUMERICHOST | AI_V4MAPPED | \
                  AI_ALL | AI_ADDRCONFIG | AI_NUMERICSERV)

int getaddrinfo(const char *node, const char *service,
                const struct addrinfo *hints, struct addrinfo **res) {
    int flags = hints ? hints->ai_flags : 0;
    int family = hints ? hints->ai_family : AF_UNSPEC;
    int socktype = hints ? hints->ai_socktype : 0;
    int protocol = hints ? hints->ai_protocol : 0;
    uint16_t port = 0;

    if (!res) return EAI_FAIL;
    *res = NULL;
    if (flags & ~AI_KNOWN) return EAI_BADFLAGS;
    if ((flags & AI_CANONNAME) && !node) return EAI_BADFLAGS;
    if (!node && !service) return EAI_NONAME;
    if (family != AF_UNSPEC && family != AF_INET && family != AF_INET6) return EAI_FAMILY;

    if (socktype == 0) {
        if (protocol == 0 || protocol == IPPROTO_TCP) socktype = SOCK_STREAM;
        else if (protocol == IPPROTO_UDP) socktype = SOCK_DGRAM;
        else return EAI_SOCKTYPE;
    }
    if (socktype == SOCK_STREAM) {
        if (protocol && protocol != IPPROTO_TCP) return EAI_SOCKTYPE;
        protocol = IPPROTO_TCP;
    } else if (socktype == SOCK_DGRAM) {
        if (protocol && protocol != IPPROTO_UDP) return EAI_SOCKTYPE;
        protocol = IPPROTO_UDP;
    } else {
        return EAI_SOCKTYPE;
    }

    if (service) {
        const char *s = service;
        unsigned long v = 0;
        if (*s == '\0') return EAI_NONAME;
        while (*s >= '0' && *s <= '9' && v <= 65535ul) v = v * 10ul + (unsigned long)(*s++ - '0');
        if (*s != '\0') return (flags & AI_NUMERICSERV) ? EAI_NONAME : EAI_SERVICE;
        if (v > 65535ul) return EAI_SERVICE;
        port = (uint16_t)v;
    }

    /* the addresses: one IPv4 and/or one IPv6 */
    uint8_t a4[4], a6[16];
    int have4 = 0, have6 = 0;
    if (!node || is_localhost(node)) {
        if ((flags & AI_NUMERICHOST) && node) return EAI_NONAME;
        int passive = !node && (flags & AI_PASSIVE);
        if (family != AF_INET6) {
            uint32_t v = passive ? INADDR_ANY : INADDR_LOOPBACK;
            a4[0] = (uint8_t)(v >> 24); a4[1] = (uint8_t)(v >> 16);
            a4[2] = (uint8_t)(v >> 8);  a4[3] = (uint8_t)v;
            have4 = 1;
        }
        if (family == AF_INET6) {
            memcpy(a6, passive ? &in6addr_any : &in6addr_loopback, 16);
            have6 = 1;
        }
    } else if (pton4(node, a4)) {
        if (family == AF_INET6) {
            if (!(flags & AI_V4MAPPED)) return EAI_ADDRFAMILY;
            memset(a6, 0, 10);
            a6[10] = a6[11] = 0xff;
            memcpy(a6 + 12, a4, 4);
            have6 = 1;
        } else {
            have4 = 1;
        }
    } else if (pton6(node, a6)) {
        if (family == AF_INET) return EAI_ADDRFAMILY;
        have6 = 1;
    } else {
        return EAI_NONAME;      /* a name, and nothing to ask about it */
    }

    struct addrinfo *first = NULL, **tail = &first;
    if (have4) {
        *tail = make_ai(AF_INET, a4, port, socktype, protocol, flags);
        if (!*tail) { freeaddrinfo(first); return EAI_MEMORY; }
        tail = &(*tail)->ai_next;
    }
    if (have6) {
        *tail = make_ai(AF_INET6, a6, port, socktype, protocol, flags);
        if (!*tail) { freeaddrinfo(first); return EAI_MEMORY; }
        tail = &(*tail)->ai_next;
    }
    if (flags & AI_CANONNAME) {
        first->ai_canonname = strdup(node);
        if (!first->ai_canonname) { freeaddrinfo(first); return EAI_MEMORY; }
    }
    *res = first;
    return 0;
}

const char *gai_strerror(int errcode) {
    switch (errcode) {
    case 0:            return "Success";
    case EAI_BADFLAGS: return "Bad value for ai_flags";
    case EAI_NONAME:   return "Name or service not known";
    case EAI_AGAIN:    return "Temporary failure in name resolution";
    case EAI_FAIL:     return "Non-recoverable failure in name resolution";
    case EAI_FAMILY:   return "ai_family not supported";
    case EAI_ADDRFAMILY: return "Address family for hostname not supported";
    case EAI_SOCKTYPE: return "ai_socktype not supported";
    case EAI_SERVICE:  return "Servname not supported for ai_socktype";
    case EAI_MEMORY:   return "Memory allocation failure";
    case EAI_SYSTEM:   return "System error";
    case EAI_OVERFLOW: return "Argument buffer overflow";
    default:           return "Unknown error";
    }
}

static int is_loopback(int family, const uint8_t *a) {
    if (family == AF_INET) return a[0] == 127 && a[1] == 0 && a[2] == 0 && a[3] == 1;
    return memcmp(a, &in6addr_loopback, 16) == 0;
}

/* The reverse of getaddrinfo: the address in numbers, except that the
 * loopback address is "localhost" unless numbers are asked for -- the one
 * name there is.  The port is always a number. */
int getnameinfo(const struct sockaddr *sa, socklen_t salen,
                char *host, socklen_t hostlen,
                char *serv, socklen_t servlen, int flags) {
    const uint8_t *addr;
    uint16_t port;
    int family;

    if (!sa) return EAI_FAIL;
    family = sa->sa_family;
    if (family == AF_INET) {
        const struct sockaddr_in *s4 = (const struct sockaddr_in *)sa;
        if (salen < sizeof *s4) return EAI_FAMILY;
        addr = (const uint8_t *)&s4->sin_addr;
        port = be16(s4->sin_port);
    } else if (family == AF_INET6) {
        const struct sockaddr_in6 *s6 = (const struct sockaddr_in6 *)sa;
        if (salen < sizeof *s6) return EAI_FAMILY;
        addr = s6->sin6_addr.s6_addr;
        port = be16(s6->sin6_port);
    } else {
        return EAI_FAMILY;
    }
    if (!(host && hostlen) && !(serv && servlen)) return EAI_NONAME;

    if (host && hostlen) {
        if (!(flags & NI_NUMERICHOST) && is_loopback(family, addr)) {
            if (hostlen < sizeof "localhost") return EAI_OVERFLOW;
            memcpy(host, "localhost", sizeof "localhost");
        } else if (flags & NI_NAMEREQD) {
            return EAI_NONAME;
        } else if (!inet_ntop(family, addr, host, hostlen)) {
            return EAI_OVERFLOW;
        }
    }
    if (serv && servlen) {
        char tmp[6];
        char *e = put_dec(tmp, port);
        size_t n = (size_t)(e - tmp);
        if (n + 1 > servlen) return EAI_OVERFLOW;
        memcpy(serv, tmp, n);
        serv[n] = '\0';
    }
    return 0;
}

/* ---- the older interfaces ---- */

static struct {
    struct hostent h;
    char *aliases[1];
    char *addrs[2];
    uint8_t addr[16];
    char name[NI_MAXHOST];
} hostbuf;

static struct hostent *fill_host(const char *name, int type, const uint8_t *addr, int len) {
    size_t n = strlen(name);
    if (n >= sizeof hostbuf.name) n = sizeof hostbuf.name - 1;
    memcpy(hostbuf.name, name, n);
    hostbuf.name[n] = '\0';
    memcpy(hostbuf.addr, addr, (size_t)len);
    hostbuf.aliases[0] = NULL;
    hostbuf.addrs[0] = (char *)hostbuf.addr;
    hostbuf.addrs[1] = NULL;
    hostbuf.h.h_name = hostbuf.name;
    hostbuf.h.h_aliases = hostbuf.aliases;
    hostbuf.h.h_addrtype = type;
    hostbuf.h.h_length = len;
    hostbuf.h.h_addr_list = hostbuf.addrs;
    return &hostbuf.h;
}

struct hostent *gethostbyname(const char *name) {
    uint8_t a[4];
    if (name && is_localhost(name)) {
        static const uint8_t lo[4] = { 127, 0, 0, 1 };
        return fill_host("localhost", AF_INET, lo, 4);
    }
    if (name && pton4(name, a)) return fill_host(name, AF_INET, a, 4);
    h_errno = HOST_NOT_FOUND;
    return NULL;
}

struct hostent *gethostbyaddr(const void *addr, socklen_t len, int type) {
    if (addr && ((type == AF_INET && len == 4) || (type == AF_INET6 && len == 16)) &&
        is_loopback(type, (const uint8_t *)addr)) {
        return fill_host("localhost", type, (const uint8_t *)addr, (int)len);
    }
    h_errno = HOST_NOT_FOUND;
    return NULL;
}

const char *hstrerror(int err) {
    switch (err) {
    case 0:              return "Resolver Error 0 (no error)";
    case HOST_NOT_FOUND: return "Unknown host";
    case TRY_AGAIN:      return "Host name lookup failure";
    case NO_RECOVERY:    return "Unknown server error";
    case NO_DATA:        return "No address associated with name";
    default:             return "Unknown resolver error";
    }
}

struct servent *getservbyname(const char *name, const char *proto) {
    (void)name; (void)proto;
    return NULL;
}

struct servent *getservbyport(int port, const char *proto) {
    (void)port; (void)proto;
    return NULL;
}
