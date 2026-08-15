/*
 * SLOW-32 BBS v0.2 — sockets + two .DBF files.
 *
 * USERS.DBF: NAME / PASS.  MSG.DBF: FROM / TO / SUBJ / TEXT.
 * Menu: L list, R read, P post, W who, G goodbye.
 */

#include "dbfuser.h"
#include "dbfmsg.h"

#include <stdlib.h>

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static int sock_puts(int fd, const char *s) {
    int n = (int)strlen(s);
    return send(fd, s, n, 0) == n ? 0 : -1;
}

static int sock_gets(int fd, char *buf, int cap) {
    int n = 0;
    while (n < cap - 1) {
        char ch;
        int r = recv(fd, &ch, 1, 0);
        if (r <= 0) {
            return -1;
        }
        if (ch == '\n') {
            break;
        }
        if (ch == '\r') {
            continue;
        }
        buf[n++] = ch;
    }
    buf[n] = '\0';
    return n;
}

static void cmd_list(int fd, msgdb_t *msg) {
    uint32_t i, n;
    char line[128];
    n = msgdb_count(msg);
    if (n == 0) {
        sock_puts(fd, "No messages.\r\n");
        return;
    }
    sock_puts(fd, "  #  From             To               Subject\r\n");
    for (i = 1; i <= n; i++) {
        msg_t m;
        if (msgdb_get(msg, i, &m) != 0) {
            continue;
        }
        snprintf(line, sizeof(line), "%3lu  %-16s %-16s %s\r\n",
                 (unsigned long)i, m.from, m.to, m.subj);
        sock_puts(fd, line);
    }
}

static void cmd_read(int fd, msgdb_t *msg) {
    char buf[16];
    unsigned long n;
    msg_t m;
    sock_puts(fd, "Read #: ");
    if (sock_gets(fd, buf, (int)sizeof(buf)) < 0 || !buf[0]) {
        return;
    }
    n = (unsigned long)atoi(buf);
    if (msgdb_get(msg, (uint32_t)n, &m) != 0) {
        sock_puts(fd, "No such message.\r\n");
        return;
    }
    sock_puts(fd, "From: ");
    sock_puts(fd, m.from);
    sock_puts(fd, "\r\nTo: ");
    sock_puts(fd, m.to);
    sock_puts(fd, "\r\nSubj: ");
    sock_puts(fd, m.subj);
    sock_puts(fd, "\r\n\r\n");
    sock_puts(fd, m.text);
    sock_puts(fd, "\r\n");
}

static void cmd_post(int fd, msgdb_t *msg, const char *from) {
    char to[MSG_TO_MAX + 1];
    char subj[MSG_SUBJ_MAX + 1];
    char text[MSG_TEXT_MAX + 1];
    sock_puts(fd, "To: ");
    if (sock_gets(fd, to, (int)sizeof(to)) < 0) {
        return;
    }
    if (!to[0]) {
        strncpy(to, "all", sizeof(to) - 1);
    }
    sock_puts(fd, "Subj: ");
    if (sock_gets(fd, subj, (int)sizeof(subj)) < 0) {
        return;
    }
    sock_puts(fd, "Text: ");
    if (sock_gets(fd, text, (int)sizeof(text)) < 0) {
        return;
    }
    if (msgdb_post(msg, from, to, subj, text) != 0) {
        sock_puts(fd, "Post failed.\r\n");
        return;
    }
    sock_puts(fd, "Posted.\r\n");
}

static void session(int fd, userdb_t *db, msgdb_t *msg) {
    char name[64];
    char pass[64];
    char cmd[32];

    sock_puts(fd, "SLOW-32 BBS  v0.2\r\n\r\n");
    sock_puts(fd, "Name: ");
    if (sock_gets(fd, name, (int)sizeof(name)) < 0 || !name[0]) {
        return;
    }
    sock_puts(fd, "Password: ");
    if (sock_gets(fd, pass, (int)sizeof(pass)) < 0) {
        return;
    }
    if (!userdb_check(db, name, pass)) {
        sock_puts(fd, "\r\nLogon failed.\r\n");
        return;
    }
    sock_puts(fd, "\r\nWelcome, ");
    sock_puts(fd, name);
    sock_puts(fd, ".\r\n\r\n");

    for (;;) {
        sock_puts(fd, "[L]ist  [R]ead  [P]ost  [W]ho  [G]oodbye\r\n");
        if (sock_gets(fd, cmd, (int)sizeof(cmd)) < 0) {
            break;
        }
        if (cmd[0] == 'g' || cmd[0] == 'G') {
            sock_puts(fd, "Goodbye, ");
            sock_puts(fd, name);
            sock_puts(fd, ".\r\n");
            break;
        }
        if (cmd[0] == 'w' || cmd[0] == 'W') {
            sock_puts(fd, "You are the only caller.\r\n");
            continue;
        }
        if (cmd[0] == 'l' || cmd[0] == 'L') {
            cmd_list(fd, msg);
            continue;
        }
        if (cmd[0] == 'r' || cmd[0] == 'R') {
            cmd_read(fd, msg);
            continue;
        }
        if (cmd[0] == 'p' || cmd[0] == 'P') {
            cmd_post(fd, msg, name);
            continue;
        }
        sock_puts(fd, "Huh?\r\n");
    }
}

int main(int argc, char **argv) {
    const char *users = "USERS.DBF";
    const char *msgs = "MSG.DBF";
    userdb_t db;
    msgdb_t msg;
    int listen_fd;
    struct sockaddr_in addr;
    socklen_t alen;
    FILE *portf;

    if (argc > 1) {
        users = argv[1];
    }
    if (argc > 2) {
        msgs = argv[2];
    }
    if (userdb_open(&db, users) != 0) {
        printf("cannot open %s\n", users);
        return 1;
    }
    if (msgdb_open(&msg, msgs) != 0) {
        printf("cannot open %s\n", msgs);
        userdb_close(&db);
        return 1;
    }

    listen_fd = socket(AF_INET, SOCK_STREAM, 0);
    if (listen_fd < 0) {
        printf("socket-fail\n");
        msgdb_close(&msg);
        userdb_close(&db);
        return 1;
    }
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    addr.sin_port = htons(0);
    if (bind(listen_fd, (struct sockaddr *)&addr, sizeof(addr)) < 0 ||
        listen(listen_fd, 4) < 0) {
        printf("bind-fail\n");
        close(listen_fd);
        msgdb_close(&msg);
        userdb_close(&db);
        return 1;
    }
    alen = sizeof(addr);
    if (getsockname(listen_fd, (struct sockaddr *)&addr, &alen) < 0) {
        printf("getsockname-fail\n");
        close(listen_fd);
        msgdb_close(&msg);
        userdb_close(&db);
        return 1;
    }
    portf = fopen("bbs.port", "w");
    if (!portf) {
        printf("portfile-fail\n");
        close(listen_fd);
        msgdb_close(&msg);
        userdb_close(&db);
        return 1;
    }
    fprintf(portf, "%u\n", (unsigned)ntohs(addr.sin_port));
    fclose(portf);
    printf("listening 127.0.0.1:%u  users=%s  mail=%s\n",
           (unsigned)ntohs(addr.sin_port), users, msgs);
    fflush(stdout);

    for (;;) {
        int conn = accept(listen_fd, 0, 0);
        if (conn < 0) {
            break;
        }
        session(conn, &db, &msg);
        close(conn);
    }
    close(listen_fd);
    msgdb_close(&msg);
    userdb_close(&db);
    return 0;
}
