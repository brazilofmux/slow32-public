/* poll, select, fcntl and the socket options, over a loopback connection
 * the program makes to itself.  Every line is what glibc and macOS print
 * for the same program: readiness of a listener with a caller waiting, of
 * a fresh connection for writing and, once bytes are sent, for reading;
 * a timeout that runs out and a wait that is really waited; O_NONBLOCK
 * making an empty read EAGAIN; getpeername naming the other end; options
 * set and read back; a non-blocking connect to a port nobody listens on
 * ending in ECONNREFUSED; a closed descriptor POLLNVAL to poll and EBADF
 * to select. */
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <sys/select.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

static long long now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (long long)ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}

static int one_ready(int fd, short ev, int ms) {
    struct pollfd p = { fd, ev, 0 };
    int r = poll(&p, 1, ms);
    return r == 1 ? p.revents : r == 0 ? 0 : -1;
}

int main(void) {
    int lis = socket(AF_INET, SOCK_STREAM, 0);
    int yes = 1;
    setsockopt(lis, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes);
    struct sockaddr_in a;
    memset(&a, 0, sizeof a);
    a.sin_family = AF_INET;
    a.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    a.sin_port = 0;
    if (bind(lis, (struct sockaddr *)&a, sizeof a) < 0 || listen(lis, 4) < 0) {
        printf("cannot listen: %d\n", errno);
        return 1;
    }
    socklen_t al = sizeof a;
    getsockname(lis, (struct sockaddr *)&a, &al);
    int port = ntohs(a.sin_port);

    long long t0 = now_ms();
    int r = one_ready(lis, POLLIN, 60);
    long long waited = now_ms() - t0;
    printf("idle listener: poll %d, waited the timeout %d\n", r, waited >= 55);

    int cli = socket(AF_INET, SOCK_STREAM, 0);
    if (connect(cli, (struct sockaddr *)&a, sizeof a) < 0) {
        printf("cannot connect: %d\n", errno);
        return 1;
    }
    printf("listener with a caller: POLLIN %d\n", (one_ready(lis, POLLIN, 1000) & POLLIN) != 0);
    struct sockaddr_in peer;
    socklen_t pl = sizeof peer;
    int srv = accept(lis, (struct sockaddr *)&peer, &pl);
    printf("accepted %d\n", srv >= 0);

    struct sockaddr_in mine, theirs;
    socklen_t ml = sizeof mine, tl = sizeof theirs;
    getsockname(cli, (struct sockaddr *)&mine, &ml);
    r = getpeername(srv, (struct sockaddr *)&theirs, &tl);
    printf("getpeername %d: the client's port %d, loopback %d, len %d\n", r,
           theirs.sin_port == mine.sin_port, theirs.sin_addr.s_addr == htonl(INADDR_LOOPBACK),
           (int)tl == (int)sizeof theirs);
    tl = sizeof theirs;
    getpeername(cli, (struct sockaddr *)&theirs, &tl);
    printf("client's peer is the listening port %d\n", ntohs(theirs.sin_port) == port);

    printf("fresh connection: POLLOUT %d, POLLIN %d\n",
           (one_ready(cli, POLLOUT, 1000) & POLLOUT) != 0, one_ready(srv, POLLIN, 0));

    int fl = fcntl(srv, F_GETFL);
    printf("F_GETFL: nonblocking %d, read-write %d\n", (fl & O_NONBLOCK) != 0,
           (fl & O_ACCMODE) == O_RDWR);
    r = fcntl(srv, F_SETFL, fl | O_NONBLOCK);
    printf("F_SETFL O_NONBLOCK: %d, now %d\n", r, (fcntl(srv, F_GETFL) & O_NONBLOCK) != 0);
    char buf[64];
    errno = 0;
    ssize_t n = read(srv, buf, sizeof buf);
    int e = errno;
    printf("empty non-blocking read: %d EAGAIN %d\n", (int)n, e == EAGAIN || e == EWOULDBLOCK);

    write(cli, "lantern", 7);
    r = one_ready(srv, POLLIN, 1000);
    printf("after a send: POLLIN %d\n", (r & POLLIN) != 0);
    n = read(srv, buf, sizeof buf);
    printf("read %d \"%.*s\"\n", (int)n, n > 0 ? (int)n : 0, buf);

    fd_set rs, ws;
    FD_ZERO(&rs);
    FD_ZERO(&ws);
    FD_SET(srv, &rs);
    FD_SET(cli, &rs);
    FD_SET(cli, &ws);
    struct timeval tv = { 0, 0 };
    r = select((srv > cli ? srv : cli) + 1, &rs, &ws, NULL, &tv);
    printf("select, nothing to read: %d, srv %d cli %d, cli writable %d\n", r,
           !!FD_ISSET(srv, &rs), !!FD_ISSET(cli, &rs), !!FD_ISSET(cli, &ws));
    write(srv, "moth", 4);
    FD_ZERO(&rs);
    FD_SET(srv, &rs);
    FD_SET(cli, &rs);
    tv.tv_sec = 2;
    tv.tv_usec = 0;
    r = select((srv > cli ? srv : cli) + 1, &rs, NULL, NULL, &tv);
    printf("select, the client has bytes: %d, srv %d cli %d\n", r,
           !!FD_ISSET(srv, &rs), !!FD_ISSET(cli, &rs));
    n = read(cli, buf, sizeof buf);
    printf("client read %d \"%.*s\"\n", (int)n, n > 0 ? (int)n : 0, buf);
    t0 = now_ms();
    FD_ZERO(&rs);
    FD_SET(cli, &rs);
    tv.tv_sec = 0;
    tv.tv_usec = 50000;
    r = select(cli + 1, &rs, NULL, NULL, &tv);
    printf("select times out: %d, waited %d\n", r, now_ms() - t0 >= 45);

    int v = 1;
    r = setsockopt(cli, IPPROTO_TCP, TCP_NODELAY, &v, sizeof v);
    v = 0;
    socklen_t vl = sizeof v;
    int r2 = getsockopt(cli, IPPROTO_TCP, TCP_NODELAY, &v, &vl);
    printf("TCP_NODELAY set %d, get %d, on %d, len %d\n", r, r2, v != 0, (int)vl);
    v = 1;
    r = setsockopt(cli, SOL_SOCKET, SO_KEEPALIVE, &v, sizeof v);
    v = 0;
    vl = sizeof v;
    getsockopt(cli, SOL_SOCKET, SO_KEEPALIVE, &v, &vl);
    printf("SO_KEEPALIVE set %d, on %d\n", r, v != 0);
    struct linger lg = { 1, 3 };
    r = setsockopt(cli, SOL_SOCKET, SO_LINGER, &lg, sizeof lg);
    memset(&lg, 0, sizeof lg);
    vl = sizeof lg;
    getsockopt(cli, SOL_SOCKET, SO_LINGER, &lg, &vl);
    printf("SO_LINGER set %d, on %d, %d seconds\n", r, lg.l_onoff != 0, lg.l_linger);
    v = -1;
    vl = sizeof v;
    r = getsockopt(cli, SOL_SOCKET, SO_ERROR, &v, &vl);
    printf("SO_ERROR %d: %d\n", r, v);
    v = 0;
    vl = sizeof v;
    getsockopt(cli, SOL_SOCKET, SO_TYPE, &v, &vl);
    printf("SO_TYPE stream %d\n", v == SOCK_STREAM);
    errno = 0;
    r = setsockopt(cli, SOL_SOCKET, 0x7777, &v, sizeof v);
    e = errno;
    printf("unknown option: %d ENOPROTOOPT %d\n", r, e == ENOPROTOOPT);

    close(cli);
    r = one_ready(srv, POLLIN, 1000);
    n = read(srv, buf, sizeof buf);
    printf("peer closed: readable %d, read %d\n", (r & (POLLIN | POLLHUP)) != 0, (int)n);
    close(srv);

    /* a port nobody listens on: the listener's own, once it is closed */
    close(lis);
    int nb = socket(AF_INET, SOCK_STREAM, 0);
    fcntl(nb, F_SETFL, fcntl(nb, F_GETFL) | O_NONBLOCK);
    errno = 0;
    r = connect(nb, (struct sockaddr *)&a, sizeof a);
    e = errno;
    int refused = 0;
    if (r < 0 && e == ECONNREFUSED) {
        refused = 1;
    } else if (r < 0 && e == EINPROGRESS) {
        one_ready(nb, POLLOUT, 2000);
        v = 0;
        vl = sizeof v;
        getsockopt(nb, SOL_SOCKET, SO_ERROR, &v, &vl);
        refused = v == ECONNREFUSED;
    }
    printf("non-blocking connect to a closed port: refused %d\n", refused);
    close(nb);

    struct pollfd p = { nb, POLLIN, 0 };
    r = poll(&p, 1, 0);
    printf("closed fd: poll %d, POLLNVAL %d\n", r, (p.revents & POLLNVAL) != 0);
    p.fd = -1;
    p.revents = 77;
    r = poll(&p, 1, 0);
    printf("negative fd skipped: poll %d, revents %d\n", r, p.revents);
    FD_ZERO(&rs);
    FD_SET(nb, &rs);
    tv.tv_sec = 0;
    tv.tv_usec = 0;
    errno = 0;
    r = select(nb + 1, &rs, NULL, NULL, &tv);
    e = errno;
    printf("closed fd: select %d EBADF %d\n", r, e == EBADF);
    t0 = now_ms();
    r = poll(NULL, 0, 30);
    printf("poll of nothing sleeps: %d, waited %d\n", r, now_ms() - t0 >= 25);
    return 0;
}
