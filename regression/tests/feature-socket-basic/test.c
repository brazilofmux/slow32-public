#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(void) {
    int fd;
    struct sockaddr_in addr;
    socklen_t alen;

    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        printf("socket-fail\n");
        return 1;
    }
    printf("socket-ok\n");

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons(1);
    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
        printf("connect-unexpected-ok\n");
        close(fd);
        return 1;
    }
    printf("connect-fail-ok\n");
    close(fd);

    fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        printf("socket2-fail\n");
        return 1;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    inet_aton("127.0.0.1", &addr.sin_addr);
    addr.sin_port = htons(0);
    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        printf("bind-fail\n");
        close(fd);
        return 1;
    }
    if (listen(fd, 4) < 0) {
        printf("listen-fail\n");
        close(fd);
        return 1;
    }

    memset(&addr, 0, sizeof(addr));
    alen = sizeof(addr);
    if (getsockname(fd, (struct sockaddr *)&addr, &alen) < 0 ||
        ntohs(addr.sin_port) == 0) {
        printf("getsockname-fail\n");
        close(fd);
        return 1;
    }
    printf("listen-ok\n");
    close(fd);
    return 0;
}
