/* inet_pton, inet_ntop, getaddrinfo, getnameinfo and gethostbyname, which
 * need no resolver: numeric text and "localhost".  Up to the "-- machine
 * --" line, each case prints what glibc and macOS both print for it.
 * After it come the cases where those two differ, which follow glibc
 * (leading zeros, AI_CANONNAME, a short sockaddr, EAI_ADDRFAMILY), and
 * the machine's own choices: one localhost entry per family, a port past
 * 65535 refused, and, asked for no family or socket type, what the
 * machine's sockets can use -- IPv4 and SOCK_STREAM. */
#define _GNU_SOURCE     /* glibc names EAI_ADDRFAMILY only then */
#include <arpa/inet.h>
#include <netdb.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>

static const char *eai(int e) {
    switch (e) {
    case 0: return "ok";
    case EAI_NONAME: return "EAI_NONAME";
    case EAI_SERVICE: return "EAI_SERVICE";
    case EAI_FAMILY: return "EAI_FAMILY";
    case EAI_BADFLAGS: return "EAI_BADFLAGS";
    case EAI_SOCKTYPE: return "EAI_SOCKTYPE";
    case EAI_OVERFLOW: return "EAI_OVERFLOW";
#if defined(EAI_ADDRFAMILY) && !defined(__APPLE__)   /* macOS's is obsolete, never returned */
    case EAI_ADDRFAMILY: return "EAI_ADDRFAMILY";
#endif
    default: return "other";
    }
}

static void round6(const char *text) {
    unsigned char a[16];
    char out[INET6_ADDRSTRLEN];
    int r = inet_pton(AF_INET6, text, a);
    if (r != 1) { printf("pton6 %-28s -> %d\n", text, r); return; }
    printf("pton6 %-28s -> %s\n", text, inet_ntop(AF_INET6, a, out, sizeof out));
}

static void round4(const char *text) {
    unsigned char a[4];
    char out[INET_ADDRSTRLEN];
    int r = inet_pton(AF_INET, text, a);
    if (r != 1) { printf("pton4 %-16s -> %d\n", text, r); return; }
    printf("pton4 %-16s -> %s (%u %u %u %u)\n", text,
           inet_ntop(AF_INET, a, out, sizeof out), a[0], a[1], a[2], a[3]);
}

static void show(const char *what, const char *node, const char *serv, int family, int flags) {
    struct addrinfo hints, *res = NULL;
    memset(&hints, 0, sizeof hints);
    hints.ai_family = family;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags = flags;
    int r = getaddrinfo(node, serv, &hints, &res);
    printf("gai %-24s %s", what, eai(r));
    for (struct addrinfo *ai = res; ai; ai = ai->ai_next) {
        char h[NI_MAXHOST], s[NI_MAXSERV];
        int g = getnameinfo(ai->ai_addr, ai->ai_addrlen, h, sizeof h, s, sizeof s,
                            NI_NUMERICHOST | NI_NUMERICSERV);
        printf(" [%s %s port %s proto %s%s%s]", ai->ai_family == AF_INET ? "inet" : "inet6",
               g ? eai(g) : h, g ? "-" : s, ai->ai_protocol == IPPROTO_TCP ? "tcp" : "?",
               ai->ai_canonname ? " canon " : "", ai->ai_canonname ? ai->ai_canonname : "");
    }
    printf("\n");
    freeaddrinfo(res);
}

int main(void) {
    round4("192.0.2.33");
    round4("0.0.0.0");
    round4("255.255.255.255");
    round4("10.1.2");
    round4("10.1.2.300");
    round4("10.1.2.3 ");
    round4("1.2.3.4.5");

    round6("::");
    round6("::1");
    round6("2001:db8::7");
    round6("2001:DB8:0:0:1:0:0:1");
    round6("2001:db8:0:0:0:0:2:1");
    round6("2001:db8:0:1:1:1:1:1");
    round6("fe80:0:0:0:0:0:0:0");
    round6("::ffff:198.51.100.9");
    round6("::ffff:c633:6409");
    round6("1:2:3:4:5:6:7::");
    round6("1:2:3:4:5:6:7:8");
    round6("1:2:3:4:5:6:7:8:9");
    round6("1::2::3");
    round6("12345::");
    round6(":1::");
    round6("1:");
    round6("::1.2.3");

    char tiny[8];
    unsigned char a4[4] = { 203, 0, 113, 200 };
    printf("ntop into 8 bytes: %s\n", inet_ntop(AF_INET, a4, tiny, sizeof tiny) ? "fits" : "NULL");
    printf("pton af 99: %d\n", inet_pton(99, "1.2.3.4", a4));

    show("numeric v4 port", "198.51.100.20", "4201", AF_INET, AI_NUMERICHOST | AI_NUMERICSERV);
    show("numeric v6", "2001:db8::42", "7", AF_INET6, AI_NUMERICHOST);
    show("passive v4", NULL, "2860", AF_INET, AI_PASSIVE);
    show("loopback v4", NULL, "2860", AF_INET, 0);
    show("passive v6", NULL, "2860", AF_INET6, AI_PASSIVE);
    show("name, numeric only", "mudhost.example", NULL, AF_INET, AI_NUMERICHOST);
    show("named serv, numeric", "192.0.2.5", "telnet", AF_INET, AI_NUMERICSERV);
    show("nothing", NULL, NULL, AF_INET, 0);
    show("family 77", "192.0.2.5", NULL, 77, 0);

    struct sockaddr_in sin;
    memset(&sin, 0, sizeof sin);
    sin.sin_family = AF_INET;
    sin.sin_port = htons(6250);
    inet_pton(AF_INET, "192.0.2.77", &sin.sin_addr);
    char h[NI_MAXHOST], s[NI_MAXSERV], small[4];
    int g = getnameinfo((struct sockaddr *)&sin, sizeof sin, h, sizeof h, s, sizeof s,
                        NI_NUMERICHOST | NI_NUMERICSERV);
    printf("gni numeric: %s %s %s\n", eai(g), h, s);
    g = getnameinfo((struct sockaddr *)&sin, sizeof sin, small, sizeof small, NULL, 0, NI_NUMERICHOST);
    printf("gni small host buffer: %s\n", eai(g));
    g = getnameinfo((struct sockaddr *)&sin, sizeof sin, NULL, 0, s, sizeof s, NI_NUMERICSERV);
    printf("gni service only: %s %s\n", eai(g), s);
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    g = getnameinfo((struct sockaddr *)&sin, sizeof sin, h, sizeof h, NULL, 0, NI_NUMERICHOST);
    printf("gni loopback, numeric: %s %s\n", eai(g), h);

    struct hostent *he = gethostbyname("192.0.2.99");
    printf("ghbn numeric: %s", he ? he->h_name : "NULL");
    if (he) printf(" len %d %u.%u.%u.%u", he->h_length, (unsigned char)he->h_addr[0],
                   (unsigned char)he->h_addr[1], (unsigned char)he->h_addr[2],
                   (unsigned char)he->h_addr[3]);
    printf("\n");

    /* the machine's own choices, and its one name */
    printf("-- machine --\n");
    round4("10.01.2.3");
    show("canon", "192.0.2.5", NULL, AF_INET, AI_CANONNAME | AI_NUMERICHOST);
    show("canon without node", NULL, "9", AF_INET, AI_CANONNAME);
    show("v4 text as v6", "192.0.2.5", NULL, AF_INET6, AI_NUMERICHOST);
    show("v6 text as v4", "2001:db8::5", NULL, AF_INET, AI_NUMERICHOST);
    show("v4 text as v6, mapped", "192.0.2.5", "21", AF_INET6, AI_NUMERICHOST | AI_V4MAPPED);
    show("localhost v4", "localhost", "80", AF_INET, 0);
    show("localhost v6", "LocalHost", "80", AF_INET6, 0);
    show("port too big", "192.0.2.5", "65536", AF_INET, 0);
    sin.sin_addr.s_addr = htonl(0xC000024Du);
    g = getnameinfo((struct sockaddr *)&sin, 4, h, sizeof h, NULL, 0, NI_NUMERICHOST);
    printf("gni short sockaddr: %s\n", eai(g));
    sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    struct addrinfo *res = NULL;
    int r = getaddrinfo(NULL, "2860", NULL, &res);
    printf("unspec passive-less: %s", eai(r));
    for (struct addrinfo *ai = res; ai; ai = ai->ai_next)
        printf(" [%d %d %d]", ai->ai_family == AF_INET, ai->ai_socktype == SOCK_STREAM,
               ai->ai_next == NULL);
    printf("\n");
    freeaddrinfo(res);
    show("name", "mudhost.example", "4201", AF_INET, 0);
    g = getnameinfo((struct sockaddr *)&sin, sizeof sin, h, sizeof h, NULL, 0, 0);
    printf("gni loopback: %s %s\n", eai(g), h);
    sin.sin_addr.s_addr = htonl(0xC0000263u);
    g = getnameinfo((struct sockaddr *)&sin, sizeof sin, h, sizeof h, NULL, 0, NI_NAMEREQD);
    printf("gni name required: %s\n", eai(g));
    he = gethostbyname("mudhost.example");
    printf("ghbn name: %s h_errno %s\n", he ? "found" : "NULL", h_errno == HOST_NOT_FOUND ? "HOST_NOT_FOUND" : "other");
    printf("servbyname: %s\n", getservbyname("telnet", "tcp") ? "found" : "NULL");
    return 0;
}
