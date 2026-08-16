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

#define SOH   0x01
#define MAXL  94            /* max LEN field value we offer */
#define QCTL  '#'
#define MAXTRY 10

#define tochar(x) ((x) + 32)
#define unchar(c) ((c) - 32)
#define ctl(c)    ((c) ^ 64)

typedef struct {
    int fd;
    unsigned char rbuf[512];
    int rlen, rpos;
    int maxl;               /* peer's max packet length */
    int qctl;               /* peer's control-quote char */
} link_t;

static int link_getc(link_t *lk) {
    if (lk->rpos >= lk->rlen) {
        int n = recv(lk->fd, (char *)lk->rbuf, (int)sizeof(lk->rbuf), 0);
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
    unsigned char buf[128 + 8];
    int n = 0;
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
 * damaged packet (caller NAKs). */
static int read_pkt(link_t *lk, pkt_t *p) {
    unsigned char raw[128 + 4];
    int c, len, i, n;

    do {
        c = link_getc(lk);
        if (c < 0) {
            return -1;
        }
    } while (c != SOH);

    n = 0;
    c = link_getc(lk);
    if (c < 0) {
        return -1;
    }
    raw[n++] = (unsigned char)c;
    len = unchar(c);
    if (len < 3 || len > MAXL) {
        return 1;
    }
    for (i = 0; i < len; i++) {
        c = link_getc(lk);
        if (c < 0) {
            return -1;
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

/* ---- init parameters ---------------------------------------------- */

static void my_params(unsigned char *d, int *dlen) {
    d[0] = tochar(MAXL);    /* MAXL */
    d[1] = tochar(5);       /* TIME */
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
    if (p->dlen >= 6 && p->data[5] > 32 && p->data[5] < 127) {
        lk->qctl = p->data[5];
    }
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
            return 1;               /* damaged: resend */
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

    memset(&lk, 0, sizeof(lk));
    lk.maxl = MAXL;
    lk.qctl = QCTL;
    lk.fd = socket(AF_INET, SOCK_STREAM, 0);
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
    int expect = 0, done = 0;
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

    memset(&lk, 0, sizeof(lk));
    lk.maxl = MAXL;
    lk.qctl = QCTL;
    lk.fd = accept(listen_fd, 0, 0);
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
            send_pkt(&lk, expect, 'N', NULL, 0);
            continue;
        }
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

int main(int argc, char **argv) {
    if (argc >= 2 && strcmp(argv[1], "-r") == 0) {
        return do_receive();
    }
    if (argc >= 2 && strcmp(argv[1], "-s") == 0) {
        const char *host = "127.0.0.1";
        int i = 2, port;
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
    printf("usage: kermit -r\n");
    printf("       kermit -s [-h A.B.C.D] PORT FILE...\n");
    return 1;
}
