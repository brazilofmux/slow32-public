#ifndef SLOW32_ARPA_INET_H
#define SLOW32_ARPA_INET_H

#include <stdint.h>
#include <netinet/in.h>

#ifdef __cplusplus
extern "C" {
#endif

uint32_t htonl(uint32_t hostlong);
uint16_t htons(uint16_t hostshort);
uint32_t ntohl(uint32_t netlong);
uint16_t ntohs(uint16_t netshort);

int inet_aton(const char *cp, struct in_addr *inp);
in_addr_t inet_addr(const char *cp);
char *inet_ntoa(struct in_addr in);

/* AF_INET and AF_INET6 text forms (netdb.c).  inet_pton takes only the
 * four-part dotted decimal for IPv4, without leading zeros; inet_ntop
 * writes RFC 5952's form for IPv6. */
int inet_pton(int af, const char *src, void *dst);
const char *inet_ntop(int af, const void *src, char *dst, socklen_t size);

#ifdef __cplusplus
}
#endif

#endif
