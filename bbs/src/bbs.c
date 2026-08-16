/*
 * SLOW-32 BBS v0.2 — sockets + two .DBF files.
 *
 * USERS.DBF: NAME / PASS.  MSG.DBF: FROM / TO / SUBJ / TEXT.
 * Menu: L list, R read, P post (blank line ends), W who, G goodbye.
 */

#include "dbfuser.h"
#include "dbfmsg.h"
#include "zmodem.h"

#include <dirent.h>

#include <stdlib.h>
#include <ctype.h>
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
    sock_puts(fd, "Text (blank line ends):\r\n");
    text[0] = '\0';
    for (;;) {
        char line[81];
        int used;
        if (sock_gets(fd, line, (int)sizeof(line)) < 0) {
            return;
        }
        if (!line[0]) {
            break;
        }
        used = (int)strlen(text);
        if (used > 0 && used + 1 < (int)sizeof(text)) {
            text[used++] = ' ';
            text[used] = '\0';
        }
        if (used + (int)strlen(line) >= (int)sizeof(text)) {
            sock_puts(fd, "(truncated)\r\n");
            break;
        }
        memcpy(text + used, line, strlen(line) + 1);
    }
    if (msgdb_post(msg, from, to, subj, text) != 0) {
        sock_puts(fd, "Post failed.\r\n");
        return;
    }
    sock_puts(fd, "Posted.\r\n");
}

static int looks_s32x(const char *s) {
    size_t n = strlen(s);
    if (n < 5) {
        return 0;
    }
    return toupper((unsigned char)s[n - 5]) == '.' &&
           toupper((unsigned char)s[n - 4]) == 'S' &&
           s[n - 3] == '3' && s[n - 2] == '2' &&
           toupper((unsigned char)s[n - 1]) == 'X';
}

static int safe_filename(const char *s);

static const char *resolve_door(const char *name, char *buf, int cap) {
    if (access(name, F_OK) == 0 && looks_s32x(name)) {
        return name;
    }
    snprintf(buf, (size_t)cap, "%s.s32x", name);
    if (access(buf, F_OK) == 0) {
        return buf;
    }
    snprintf(buf, (size_t)cap, "doors/%s.s32x", name);
    if (access(buf, F_OK) == 0) {
        return buf;
    }
    return NULL;
}

static void list_doors(int fd) {
    DIR *d = opendir("doors");
    struct dirent *e;
    int n = 0;
    if (d) {
        while ((e = readdir(d)) != NULL) {
            if (!looks_s32x(e->d_name)) {
                continue;
            }
            if (n == 0) {
                sock_puts(fd, "Doors:\r\n");
            }
            sock_puts(fd, "  ");
            e->d_name[strlen(e->d_name) - 5] = '\0';
            sock_puts(fd, e->d_name);
            sock_puts(fd, "\r\n");
            n++;
        }
        closedir(d);
    }
    if (n == 0) {
        sock_puts(fd, "No doors installed.\r\n");
    }
}

static void cmd_door(int fd, const char *name) {
    char asked[64];
    char pathbuf[128];
    const char *path;
    char *av[2];
    int rc;

    sock_puts(fd, "Door (? lists): ");
    if (sock_gets(fd, asked, (int)sizeof(asked)) < 0) {
        return;
    }
    if (!asked[0] || strcmp(asked, "?") == 0) {
        list_doors(fd);
        return;
    }
    /* The door name is network-supplied. Gate it exactly as cmd_transfer
     * does: no leading '.' and no path separators, so resolve_door cannot be
     * steered outside doors/ to run an arbitrary host .s32x. */
    if (!safe_filename(asked)) {
        sock_puts(fd, "No such door.\r\n");
        return;
    }
    path = resolve_door(asked, pathbuf, (int)sizeof(pathbuf));
    if (!path) {
        sock_puts(fd, "No such door.\r\n");
        return;
    }
    av[0] = (char *)path;
    av[1] = NULL;
    rc = s32_execv_fd(path, av, fd);
    if (rc < 0) {
        sock_puts(fd, "Door failed.\r\n");
    }
    (void)name;
}

static int safe_filename(const char *s) {
    if (!s || !s[0]) {
        return 0;
    }
    if (s[0] == '.') {
        return 0;
    }
    while (*s) {
        if (*s == '/' || *s == '\\') {
            return 0;
        }
        s++;
    }
    return 1;
}

static void cmd_files(int fd) {
    const char *dir = "files";
    DIR *d;
    struct dirent *ent;
    int n = 0;
    char line[128];

    d = opendir(dir);
    if (!d) {
        dir = ".";
        d = opendir(dir);
    }
    if (!d) {
        sock_puts(fd, "No files.\r\n");
        return;
    }
    sock_puts(fd, "Files:\r\n");
    while ((ent = readdir(d)) != NULL) {
        if (!safe_filename(ent->d_name)) {
            continue;
        }
        snprintf(line, sizeof(line), "  %s\r\n", ent->d_name);
        sock_puts(fd, line);
        n++;
    }
    closedir(d);
    if (n == 0) {
        sock_puts(fd, "  (none)\r\n");
    }
}

static void cmd_transfer(int fd) {
    char asked[64];
    char path[128];
    const char *use = NULL;

    sock_puts(fd, "File: ");
    if (sock_gets(fd, asked, (int)sizeof(asked)) < 0 || !asked[0]) {
        return;
    }
    if (!safe_filename(asked)) {
        sock_puts(fd, "Bad name.\r\n");
        return;
    }
    snprintf(path, sizeof(path), "files/%s", asked);
    if (access(path, F_OK) == 0) {
        use = path;
    } else if (access(asked, F_OK) == 0) {
        use = asked;
    }
    if (!use) {
        sock_puts(fd, "No such file.\r\n");
        return;
    }
    sock_puts(fd, "Sending via ZMODEM...\r\n");
    if (zmodem_send(fd, use) == 0) {
        sock_puts(fd, "Done.\r\n");
    } else {
        sock_puts(fd, "Transfer failed.\r\n");
    }
}

static void session(int fd, userdb_t *db, msgdb_t *msg) {
    char name[64];
    char pass[64];
    char cmd[32];

    sock_puts(fd, "SLOW-32 BBS  v0.5\r\n\r\n");
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
        sock_puts(fd, "[L]ist  [R]ead  [P]ost  [D]oor  [F]iles  [T]ransfer  [W]ho  [G]oodbye\r\n");
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
        if (cmd[0] == 'd' || cmd[0] == 'D') {
            cmd_door(fd, name);
            continue;
        }
        if (cmd[0] == 'f' || cmd[0] == 'F') {
            cmd_files(fd);
            continue;
        }
        if (cmd[0] == 't' || cmd[0] == 'T') {
            cmd_transfer(fd);
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
