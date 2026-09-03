/* kermit - the civilized file protocol, on the SLOW-32 hose.
 *
 *   kermit -r                  listen on 127.0.0.1:0, write kermit.port,
 *                              receive files into the cwd
 *   kermit -s PORT FILE...     connect to 127.0.0.1:PORT and send
 *   kermit -s -h A.B.C.D PORT FILE...   send to another address
 *
 * Classic Kermit: every packet printable, ctl-quoted with '#', one
 * type-1 checksum character, stop-and-wait ACK, SEQ mod 64.  Not fast.
 * Polite.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include "s32dpc.h"

#define SOH   0x01
#define MAXL  94            /* max LEN field value we offer */
#define QCTL  '#'
#define MAXTRY 10
#define DEFTIME 5           /* seconds we ask the peer to wait for us */

/* Test knobs (-x N drops our Nth outgoing packet once; -t S is the TIME
 * we advertise, and our own until the peer's arrives). */
static int opt_drop = 0;
static int opt_time = DEFTIME;

#define tochar(x) ((x) + 32)
#define unchar(c) ((c) - 32)
#define ctl(c)    ((c) ^ 64)

typedef struct {
    int fd;
    unsigned char rbuf[512];
    int rlen, rpos;
    int maxl;               /* peer's max packet length */
    int qctl;               /* peer's control-quote char */
    int timeout;            /* seconds to wait for the peer: its TIME */
    int timer;              /* the armed packet timer's id, or -1 */
    unsigned cookie;        /* what it was armed with: stale entries differ */
} link_t;

/* The packet timer.  Armed once per packet read; the wait below sleeps
 * until the socket is readable or it fires, whichever comes first
 * (docs/plans/dpc.md: the reply is a DPC ring entry either way).  A timer
 * that fired after we stopped caring leaves a stale entry in the ring;
 * its cookie will not match, and the wait drops it. */
static void link_arm(link_t *lk) {
    lk->timer = -1;
    if (lk->timeout > 0) {
        lk->cookie++;
        lk->timer = s32_timer_start((unsigned)lk->timeout, 0u, lk->cookie);
    }
}

static void link_disarm(link_t *lk) {
    if (lk->timer >= 0) {
        s32_timer_cancel(lk->timer);    /* -1 if it already fired: stale entry, ignored later */
        lk->timer = -1;
    }
}

/* One byte from the peer: -1 on link death, -2 on timeout. */
static int link_getc(link_t *lk) {
    if (lk->rpos >= lk->rlen) {
        s32_dpc_t d;
        int n;
        for (;;) {
            if (s32_dpc_wait_on(&lk->fd, 1, &d) < 0) {
                return -1;
            }
            if (d.kind == S32_DPC_TIMER) {
                if (d.cookie == lk->cookie && lk->timer >= 0) {
                    lk->timer = -1;     /* fired: the id is free again */
                    return -2;
                }
                continue;               /* a timer we stopped caring about */
            }
            if (d.kind == S32_DPC_READY && (int)d.id == lk->fd) {
                if (d.cookie & S32_DPC_NVAL) {
                    return -1;
                }
                break;                  /* readable, or at EOF: recv says which */
            }
        }
        n = recv(lk->fd, (char *)lk->rbuf, (int)sizeof(lk->rbuf), 0);
        if (n <= 0) {
            return -1;
        }
        lk->rlen = n;
        lk->rpos = 0;
    }
    return lk->rbuf[lk->rpos++];
}

static int link_put(link_t *lk, const unsigned char *p, int n) {
    int off = 0;
    while (off < n) {
        int w = send(lk->fd, (const char *)p + off, n - off, 0);
        if (w <= 0) {
            return -1;
        }
        off += w;
    }
    return 0;
}

/* ---- packets ------------------------------------------------------ */

typedef struct {
    int seq;
    int type;
    unsigned char data[128];
    int dlen;
} pkt_t;

static int check1(const unsigned char *p, int n) {
    int s = 0, i;
    for (i = 0; i < n; i++) {
        s += p[i];
    }
    return tochar((s + ((s & 192) >> 6)) & 63);
}

static int send_pkt(link_t *lk, int seq, int type,
                    const unsigned char *data, int dlen) {
    static int sent = 0;
    unsigned char buf[128 + 8];
    int n = 0;
    if (++sent == opt_drop) {
        fprintf(stderr, "kermit: (test) dropping packet %d, type %c\n", sent, type);
        return 0;                       /* into the void, as if sent */
    }
    buf[n++] = SOH;
    buf[n++] = (unsigned char)tochar(dlen + 3);
    buf[n++] = (unsigned char)tochar(seq & 63);
    buf[n++] = (unsigned char)type;
    memcpy(buf + n, data, (size_t)dlen);
    n += dlen;
    buf[n] = (unsigned char)check1(buf + 1, n - 1);
    n++;
    buf[n++] = '\r';
    return link_put(lk, buf, n);
}

/* Read one packet.  Returns 0 on success, -1 on link death, 1 on a
 * damaged packet (caller NAKs), 2 on timeout (caller resends or NAKs). */
#define LINK_ERR(c) ((c) == -2 ? 2 : -1)
static int read_pkt_timed(link_t *lk, pkt_t *p) {
    unsigned char raw[128 + 4];
    int c, len, i, n;

    do {
        c = link_getc(lk);
        if (c < 0) {
            return LINK_ERR(c);
        }
    } while (c != SOH);

    n = 0;
    c = link_getc(lk);
    if (c < 0) {
        return LINK_ERR(c);
    }
    raw[n++] = (unsigned char)c;
    len = unchar(c);
    if (len < 3 || len > MAXL) {
        return 1;
    }
    for (i = 0; i < len; i++) {
        c = link_getc(lk);
        if (c < 0) {
            return LINK_ERR(c);
        }
        if (c == SOH) {         /* a fresh start mid-packet: damaged */
            return 1;
        }
        raw[n++] = (unsigned char)c;
    }
    if (raw[n - 1] != (unsigned char)check1(raw, n - 1)) {
        return 1;
    }
    p->seq = unchar(raw[1]);
    p->type = raw[2];
    p->dlen = len - 3;
    memcpy(p->data, raw + 3, (size_t)p->dlen);
    return 0;
}

/* The timer covers the whole packet, not each byte. */
static int read_pkt(link_t *lk, pkt_t *p) {
    int r;
    link_arm(lk);
    r = read_pkt_timed(lk, p);
    link_disarm(lk);
    if (r == 2) {
        fprintf(stderr, "kermit: timeout after %d s\n", lk->timeout);
    }
    return r;
}

/* ---- init parameters ---------------------------------------------- */

static void my_params(unsigned char *d, int *dlen) {
    d[0] = tochar(MAXL);    /* MAXL */
    d[1] = tochar(opt_time); /* TIME: how long to wait for us */
    d[2] = tochar(0);       /* NPAD */
    d[3] = ctl(0);          /* PADC */
    d[4] = tochar(13);      /* EOL  */
    d[5] = QCTL;            /* QCTL */
    *dlen = 6;
}

static void take_params(link_t *lk, const pkt_t *p) {
    lk->maxl = MAXL;
    lk->qctl = QCTL;
    if (p->dlen >= 1) {
        int m = unchar(p->data[0]);
        if (m >= 20 && m <= 94) {
            lk->maxl = m;
        }
    }
    if (p->dlen >= 2) {
        int t = unchar(p->data[1]);     /* TIME: how long the peer wants us to wait for it */
        if (t >= 1 && t <= 94) {
            lk->timeout = t;
        }
    }
    if (p->dlen >= 6 && p->data[5] > 32 && p->data[5] < 127) {
        lk->qctl = p->data[5];
    }
}

static void link_init(link_t *lk, int fd) {
    memset(lk, 0, sizeof(*lk));
    lk->fd = fd;
    lk->maxl = MAXL;
    lk->qctl = QCTL;
    lk->timeout = opt_time;
    lk->timer = -1;
}

/* ---- sender ------------------------------------------------------- */

static int wait_ack(link_t *lk, int seq) {
    pkt_t p;
    for (;;) {
        int r = read_pkt(lk, &p);
        if (r < 0) {
            return -1;
        }
        if (r > 0) {
            return 1;               /* damaged or timed out: resend */
        }
        if (p.type == 'N') {
            return 1;
        }
        if (p.type == 'E') {
            fprintf(stderr, "kermit: peer error\n");
            return -1;
        }
        if (p.type == 'Y' && p.seq == (seq & 63)) {
            return 0;
        }
        /* A stale ACK for an earlier packet: keep listening. */
    }
}

static int send_reliably(link_t *lk, int seq, int type,
                         const unsigned char *data, int dlen) {
    int try;
    for (try = 0; try < MAXTRY; try++) {
        int r;
        if (send_pkt(lk, seq, type, data, dlen) < 0) {
            return -1;
        }
        r = wait_ack(lk, seq);
        if (r == 0) {
            return 0;
        }
        if (r < 0) {
            return -1;
        }
    }
    return -1;
}

/* Ctl-quote one byte into the packet buffer. */
static void encode_byte(link_t *lk, unsigned char *data, int *dlen,
                        unsigned char b) {
    int low = b & 0x7f;
    if (low < 32 || low == 127) {
        data[(*dlen)++] = (unsigned char)lk->qctl;
        data[(*dlen)++] = (unsigned char)ctl(b);
    } else if (b == (unsigned char)lk->qctl) {
        data[(*dlen)++] = (unsigned char)lk->qctl;
        data[(*dlen)++] = b;
    } else {
        data[(*dlen)++] = b;
    }
}

static const char *basename_of(const char *path) {
    const char *b = path, *p;
    for (p = path; *p; p++) {
        if (*p == '/') {
            b = p + 1;
        }
    }
    return b;
}

static int send_file(link_t *lk, int *seq, const char *path) {
    unsigned char data[128];
    int dlen;
    long total = 0;
    FILE *f = fopen(path, "rb");
    const char *name = basename_of(path);

    if (!f) {
        fprintf(stderr, "kermit: cannot open %s\n", path);
        return -1;
    }
    printf("Sending %s... ", name);
    fflush(stdout);

    dlen = 0;
    {
        const char *q;
        for (q = name; *q && dlen + 2 <= lk->maxl - 3; q++) {
            encode_byte(lk, data, &dlen, (unsigned char)*q);
        }
    }
    if (send_reliably(lk, *seq, 'F', data, dlen) < 0) {
        fclose(f);
        return -1;
    }
    *seq = (*seq + 1) & 63;

    for (;;) {
        int room = lk->maxl - 3;
        int c;
        dlen = 0;
        /* Encode until the packet is full (a quoted byte needs 2). */
        while (dlen + 2 <= room && (c = fgetc(f)) != EOF) {
            encode_byte(lk, data, &dlen, (unsigned char)c);
            total++;
        }
        if (dlen == 0) {
            break;
        }
        if (send_reliably(lk, *seq, 'D', data, dlen) < 0) {
            fclose(f);
            return -1;
        }
        *seq = (*seq + 1) & 63;
    }
    fclose(f);

    if (send_reliably(lk, *seq, 'Z', NULL, 0) < 0) {
        return -1;
    }
    *seq = (*seq + 1) & 63;
    printf("OK (%ld bytes)\n", total);
    return 0;
}

static int do_send(const char *host, int port, char **files, int nfiles) {
    link_t lk;
    struct sockaddr_in addr;
    unsigned char data[128];
    int dlen, seq, i;

    link_init(&lk, socket(AF_INET, SOCK_STREAM, 0));
    if (lk.fd < 0) {
        fprintf(stderr, "kermit: socket failed\n");
        return 1;
    }
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = inet_addr(host);
    addr.sin_port = htons((unsigned short)port);
    if (connect(lk.fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        fprintf(stderr, "kermit: cannot connect to %s:%d\n", host, port);
        return 1;
    }

    /* Send-Init: S with our parameters; the Y carries theirs. */
    my_params(data, &dlen);
    seq = 0;
    {
        pkt_t p;
        int try, got = -1;
        for (try = 0; try < MAXTRY && got != 0; try++) {
            if (send_pkt(&lk, seq, 'S', data, dlen) < 0) {
                return 1;
            }
            got = read_pkt(&lk, &p);
            if (got < 0) {
                fprintf(stderr, "kermit: link lost in init\n");
                return 1;
            }
            if (got == 0 && (p.type != 'Y' || p.seq != 0)) {
                got = 1;
            }
        }
        if (got != 0) {
            fprintf(stderr, "kermit: no answer to Send-Init\n");
            return 1;
        }
        take_params(&lk, &p);
    }
    seq = 1;

    for (i = 0; i < nfiles; i++) {
        if (send_file(&lk, &seq, files[i]) < 0) {
            fprintf(stderr, "kermit: transfer failed\n");
            return 1;
        }
    }

    if (send_reliably(&lk, seq, 'B', NULL, 0) < 0) {
        fprintf(stderr, "kermit: no goodbye\n");
        return 1;
    }
    close(lk.fd);
    return 0;
}

/* ---- receiver ----------------------------------------------------- */

/* Undo ctl-quoting; returns the decoded length. */
static int decode_data(link_t *lk, const pkt_t *p, unsigned char *out,
                       int cap) {
    int i = 0, n = 0;
    while (i < p->dlen && n < cap) {
        int c = p->data[i++];
        if (c == lk->qctl && i < p->dlen) {
            int x = p->data[i++];
            int y = x ^ 64;
            if ((y & 0x7f) < 32 || (y & 0x7f) == 127) {
                c = y;
            } else {
                c = x;
            }
        }
        out[n++] = (unsigned char)c;
    }
    return n;
}

static void sanitize(char *name) {
    char *p;
    /* Strip any path the sender tried to include; refuse hidden files. */
    p = name + strlen(name);
    while (p > name && p[-1] != '/') {
        p--;
    }
    if (p != name) {
        memmove(name, p, strlen(p) + 1);
    }
    if (!name[0] || name[0] == '.') {
        strcpy(name, "kermit.out");
    }
}

static int do_receive(void) {
    link_t lk;
    int listen_fd;
    struct sockaddr_in addr;
    socklen_t alen;
    FILE *portf, *out = NULL;
    pkt_t p;
    unsigned char data[128];
    int dlen;
    int expect = 0, done = 0, tries = 0;
    long total = 0;
    char fname[96];

    listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        fprintf(stderr, "kermit: socket failed\n");
        return 1;
    }
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons(0);
    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
        listen(listen_fd, 1) < 0) {
        fprintf(stderr, "kermit: bind/listen failed\n");
        return 1;
    }
    alen = sizeof(addr);
    memset(&addr, 0, sizeof(addr));
    getsockname(listen_fd, (struct sockaddr *)&addr, &alen);
    portf = fopen("kermit.port", "w");
    if (!portf) {
        fprintf(stderr, "kermit: cannot write kermit.port\n");
        return 1;
    }
    fprintf(portf, "%u\n", (unsigned)ntohs(addr.sin_port));
    fclose(portf);
    printf("Kermit ready on 127.0.0.1:%u\n", (unsigned)ntohs(addr.sin_port));
    fflush(stdout);

    link_init(&lk, accept(listen_fd, 0, 0));
    if (lk.fd < 0) {
        fprintf(stderr, "kermit: accept failed\n");
        return 1;
    }

    while (!done) {
        int r = read_pkt(&lk, &p);
        if (r < 0) {
            fprintf(stderr, "kermit: link lost\n");
            if (out) {
                fclose(out);
            }
            return 1;
        }
        if (r > 0) {
            /* Damaged, or nothing came: NAK what we expect.  A peer that
             * stays silent for MAXTRY timeouts is gone. */
            if (r == 2 && ++tries >= MAXTRY) {
                fprintf(stderr, "kermit: peer silent, giving up\n");
                send_pkt(&lk, expect, 'E', NULL, 0);
                if (out) {
                    fclose(out);
                }
                return 1;
            }
            send_pkt(&lk, expect, 'N', NULL, 0);
            continue;
        }
        tries = 0;
        if (p.type == 'S') {
            /* Fresh or retransmitted Send-Init: (re-)answer with params. */
            take_params(&lk, &p);
            my_params(data, &dlen);
            send_pkt(&lk, p.seq, 'Y', data, dlen);
            expect = (p.seq + 1) & 63;
            continue;
        }
        if (p.seq == ((expect - 1) & 63)) {
            /* A retransmission of the packet we already have: re-ACK. */
            send_pkt(&lk, p.seq, 'Y', NULL, 0);
            continue;
        }
        if (p.seq != (expect & 63)) {
            send_pkt(&lk, expect, 'N', NULL, 0);
            continue;
        }
        switch (p.type) {
        case 'F': {
            int n = decode_data(&lk, &p, (unsigned char *)fname,
                                (int)sizeof(fname) - 1);
            fname[n] = '\0';
            sanitize(fname);
            if (out) {
                fclose(out);
            }
            out = fopen(fname, "wb");
            if (!out) {
                fprintf(stderr, "kermit: cannot create %s\n", fname);
                send_pkt(&lk, p.seq, 'E', NULL, 0);
                return 1;
            }
            total = 0;
            printf("Receiving %s... ", fname);
            fflush(stdout);
            break;
        }
        case 'D': {
            unsigned char plain[128];
            int n = decode_data(&lk, &p, plain, (int)sizeof(plain));
            if (out && n > 0) {
                fwrite(plain, 1, (size_t)n, out);
                total += n;
            }
            break;
        }
        case 'Z':
            if (out) {
                fclose(out);
                out = NULL;
                printf("OK (%ld bytes)\n", total);
            }
            break;
        case 'B':
            done = 1;
            break;
        default:
            break;
        }
        send_pkt(&lk, p.seq, 'Y', NULL, 0);
        expect = (expect + 1) & 63;
    }

    if (out) {
        fclose(out);
    }
    close(lk.fd);
    close(listen_fd);
    printf("Goodbye.\n");
    return 0;
}

/* ---- main --------------------------------------------------------- */

/* -x N and -t S may follow -r or -s; returns the index after them. */
static int knobs(int argc, char **argv, int i) {
    while (i + 1 < argc) {
        if (strcmp(argv[i], "-x") == 0) {
            opt_drop = atoi(argv[i + 1]);
        } else if (strcmp(argv[i], "-t") == 0) {
            opt_time = atoi(argv[i + 1]);
            if (opt_time < 0 || opt_time > 94) {
                opt_time = DEFTIME;
            }
        } else {
            break;
        }
        i += 2;
    }
    return i;
}

int main(int argc, char **argv) {
    if (argc >= 2 && strcmp(argv[1], "-r") == 0) {
        knobs(argc, argv, 2);
        return do_receive();
    }
    if (argc >= 2 && strcmp(argv[1], "-s") == 0) {
        const char *host = "127.0.0.1";
        int i = knobs(argc, argv, 2), port;
        if (i + 1 < argc && strcmp(argv[i], "-h") == 0) {
            host = argv[i + 1];
            i += 2;
        }
        if (i >= argc) {
            goto usage;
        }
        port = atoi(argv[i++]);
        if (port <= 0 || port > 65535 || i >= argc) {
            goto usage;
        }
        return do_send(host, port, argv + i, argc - i);
    }
usage:
    printf("usage: kermit -r [-t SECS]\n");
    printf("       kermit -s [-t SECS] [-h A.B.C.D] PORT FILE...\n");
    printf("  -t SECS  how long the peer should wait for us (TIME, default %d; 0 = forever)\n", DEFTIME);
    printf("  -x N     testing: lose our Nth outgoing packet\n");
    return 1;
}
