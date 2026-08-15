#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

int main(void) {
    int listen_fd;
    int conn;
    struct sockaddr_in addr;
    socklen_t alen;
    char buf[128];
    int n;
    FILE *portf;

    listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        printf("socket-fail\n");
        return 1;
    }

    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons(0);
    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        printf("bind-fail\n");
        return 1;
    }
    if (listen(listen_fd, 1) < 0) {
        printf("listen-fail\n");
        return 1;
    }

    alen = sizeof(addr);
    memset(&addr, 0, sizeof(addr));
    if (getsockname(listen_fd, (struct sockaddr *)&addr, &alen) < 0) {
        printf("getsockname-fail\n");
        return 1;
    }

    portf = fopen("echo.port", "w");
    if (!portf) {
        printf("portfile-fail\n");
        return 1;
    }
    fprintf(portf, "%u\n", (unsigned)ntohs(addr.sin_port));
    fclose(portf);
    printf("ready\n");
    fflush(stdout);

    conn = accept(listen_fd, 0, 0);
    if (conn < 0) {
        printf("accept-fail\n");
        return 1;
    }

    n = recv(conn, buf, (int)sizeof(buf) - 1, 0);
    if (n <= 0) {
        printf("recv-fail\n");
        return 1;
    }
    if (send(conn, buf, n, 0) != n) {
        printf("send-fail\n");
        return 1;
    }
    close(conn);
    close(listen_fd);
    return 0;
}
