#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(int argc, char **argv) {
    int fd;
    struct sockaddr_in addr;
    unsigned port;
    const char *msg = "hello-slow32\n";
    char buf[128];
    int n;
    int msglen;

    if (argc < 2) {
        printf("usage: echo_client <port>\n");
        return 1;
    }
    port = (unsigned)atoi(argv[1]);
    if (port == 0 || port > 65535u) {
        printf("bad-port\n");
        return 1;
    }

    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        printf("socket-fail\n");
        return 1;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons((uint16_t)port);
    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        printf("connect-fail\n");
        close(fd);
        return 1;
    }

    msglen = (int)strlen(msg);
    if (send(fd, msg, msglen, 0) != msglen) {
        printf("send-fail\n");
        close(fd);
        return 1;
    }
    n = recv(fd, buf, (int)sizeof(buf) - 1, 0);
    if (n <= 0) {
        printf("recv-fail\n");
        close(fd);
        return 1;
    }
    buf[n] = '\0';
    printf("%s", buf);
    if (n > 0 && buf[n - 1] != '\n') {
        printf("\n");
    }
    close(fd);
    return 0;
}
