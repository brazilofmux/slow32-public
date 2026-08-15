/*
 * command.c — A COMMAND.COM-shaped shell for SLOW-32.
 *
 * File commands only in v1. No .BAT, no PATH, no launching .s32x
 * (that waits on the host exec service). The prompt is theater;
 * DIR is the product.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <dirent.h>
#include <sys/stat.h>

#define LINE_MAX     256
#define PATH_MAX_CMD 512
#define DIR_MAX      128
#define PROMPT       "C:\\>"

static void normalize_slashes(char *s) {
    if (!s) {
        return;
    }
    while (*s) {
        if (*s == '\\') {
            *s = '/';
        }
        s++;
    }
}

static void trim(char *s) {
    char *end;
    char *p = s;

    while (*p && isspace((unsigned char)*p)) {
        p++;
    }
    if (p != s) {
        memmove(s, p, strlen(p) + 1);
    }
    end = s + strlen(s);
    while (end > s && isspace((unsigned char)end[-1])) {
        end--;
    }
    *end = '\0';
}

static int icmp(const char *a, const char *b) {
    while (*a && *b) {
        int ca = toupper((unsigned char)*a);
        int cb = toupper((unsigned char)*b);
        if (ca != cb) {
            return ca - cb;
        }
        a++;
        b++;
    }
    return (unsigned char)toupper((unsigned char)*a) -
           (unsigned char)toupper((unsigned char)*b);
}

static int cmd_is(const char *word, const char *name) {
    return icmp(word, name) == 0;
}

/* Split the first token off `line`. Returns pointer to the remainder. */
static char *split_cmd(char *line, char *cmd, size_t cmd_cap) {
    char *p = line;
    size_t n = 0;

    while (*p && !isspace((unsigned char)*p)) {
        if (n + 1 < cmd_cap) {
            cmd[n++] = *p;
        }
        p++;
    }
    cmd[n] = '\0';
    while (*p && isspace((unsigned char)*p)) {
        p++;
    }
    return p;
}

static void split_two(char *args, char *a, size_t a_cap, char *b, size_t b_cap) {
    char *rest = split_cmd(args, a, a_cap);
    split_cmd(rest, b, b_cap);
    normalize_slashes(a);
    normalize_slashes(b);
}

static void print_dos_error(void) {
    printf("File not found\n");
}

static void cmd_ver(void) {
    printf("SLOW-32 Command   Version 0.1\n");
}

static void cmd_help(void) {
    printf("DIR     list files          TYPE    show a file\n");
    printf("COPY    copy a file         REN     rename a file\n");
    printf("DEL     delete a file       CD      change directory\n");
    printf("MD      make directory      RD      remove directory\n");
    printf("ECHO    print text          CLS     clear screen\n");
    printf("VER     version             HELP    this list\n");
    printf("EXIT    leave COMMAND\n");
}

static void cmd_cls(void) {
    printf("\033[2J\033[H");
}

static void cmd_echo(const char *args) {
    printf("%s\n", args);
}

static void cmd_cd(const char *args) {
    char cwd[PATH_MAX_CMD];

    if (!args[0]) {
        if (getcwd(cwd, sizeof(cwd))) {
            printf("%s\n", cwd);
        } else {
            print_dos_error();
        }
        return;
    }

    {
        char path[PATH_MAX_CMD];
        strncpy(path, args, sizeof(path) - 1);
        path[sizeof(path) - 1] = '\0';
        normalize_slashes(path);
        if (chdir(path) != 0) {
            print_dos_error();
        }
    }
}

static int name_cmp(const void *a, const void *b) {
    return icmp((const char *)a, (const char *)b);
}

static void print_dir_entry(const char *dirpath, const char *name) {
    char full[PATH_MAX_CMD];
    struct stat st;
    size_t n;

    n = (size_t)snprintf(full, sizeof(full), "%s/%s", dirpath, name);
    if (n >= sizeof(full)) {
        printf("%-16s\n", name);
        return;
    }
    if (stat(full, &st) == 0 && S_ISDIR(st.st_mode)) {
        printf("%-16s <DIR>\n", name);
    } else if (stat(full, &st) == 0) {
        printf("%-16s %8ld\n", name, (long)st.st_size);
    } else {
        printf("%-16s\n", name);
    }
}

static void cmd_dir(const char *args) {
    char path[PATH_MAX_CMD];
    char names[DIR_MAX][256];
    int count = 0;
    int i;
    DIR *d;
    struct dirent *ent;
    struct stat st;

    if (args[0]) {
        strncpy(path, args, sizeof(path) - 1);
        path[sizeof(path) - 1] = '\0';
        normalize_slashes(path);
    } else if (!getcwd(path, sizeof(path))) {
        print_dos_error();
        return;
    }

    if (stat(path, &st) != 0) {
        print_dos_error();
        return;
    }
    if (S_ISREG(st.st_mode)) {
        const char *slash = strrchr(path, '/');
        printf("%-16s %8ld\n", slash ? slash + 1 : path, (long)st.st_size);
        return;
    }
    if (!S_ISDIR(st.st_mode)) {
        print_dos_error();
        return;
    }

    d = opendir(path);
    if (!d) {
        print_dos_error();
        return;
    }
    while ((ent = readdir(d)) != NULL) {
        if (count >= DIR_MAX) {
            break;
        }
        strncpy(names[count], ent->d_name, 255);
        names[count][255] = '\0';
        count++;
    }
    closedir(d);

    qsort(names, (size_t)count, sizeof(names[0]), name_cmp);
    printf(" Directory of %s\n\n", path);
    for (i = 0; i < count; i++) {
        print_dir_entry(path, names[i]);
    }
    printf("        %d file(s)\n", count);
}

static void cmd_type(const char *args) {
    FILE *f;
    char buf[256];
    char path[PATH_MAX_CMD];
    size_t n;

    if (!args[0]) {
        printf("Required parameter missing\n");
        return;
    }
    strncpy(path, args, sizeof(path) - 1);
    path[sizeof(path) - 1] = '\0';
    normalize_slashes(path);

    f = fopen(path, "r");
    if (!f) {
        print_dos_error();
        return;
    }
    while ((n = fread(buf, 1, sizeof(buf), f)) > 0) {
        fwrite(buf, 1, n, stdout);
    }
    fclose(f);
}

static void cmd_copy(char *args) {
    char src[PATH_MAX_CMD];
    char dst[PATH_MAX_CMD];
    FILE *in;
    FILE *out;
    char buf[512];
    size_t n;
    size_t total = 0;

    split_two(args, src, sizeof(src), dst, sizeof(dst));
    if (!src[0] || !dst[0]) {
        printf("Required parameter missing\n");
        return;
    }

    in = fopen(src, "r");
    if (!in) {
        print_dos_error();
        return;
    }
    out = fopen(dst, "w");
    if (!out) {
        fclose(in);
        print_dos_error();
        return;
    }
    while ((n = fread(buf, 1, sizeof(buf), in)) > 0) {
        if (fwrite(buf, 1, n, out) != n) {
            printf("File creation error\n");
            fclose(in);
            fclose(out);
            return;
        }
        total += n;
    }
    fclose(in);
    fclose(out);
    printf("        1 file(s) copied\n");
    (void)total;
}

static void cmd_ren(char *args) {
    char src[PATH_MAX_CMD];
    char dst[PATH_MAX_CMD];

    split_two(args, src, sizeof(src), dst, sizeof(dst));
    if (!src[0] || !dst[0]) {
        printf("Required parameter missing\n");
        return;
    }
    if (rename(src, dst) != 0) {
        print_dos_error();
    }
}

static void cmd_del(const char *args) {
    char path[PATH_MAX_CMD];

    if (!args[0]) {
        printf("Required parameter missing\n");
        return;
    }
    strncpy(path, args, sizeof(path) - 1);
    path[sizeof(path) - 1] = '\0';
    normalize_slashes(path);
    if (remove(path) != 0) {
        print_dos_error();
    }
}

static void cmd_md(const char *args) {
    char path[PATH_MAX_CMD];

    if (!args[0]) {
        printf("Required parameter missing\n");
        return;
    }
    strncpy(path, args, sizeof(path) - 1);
    path[sizeof(path) - 1] = '\0';
    normalize_slashes(path);
    if (mkdir(path, 0777) != 0) {
        printf("Unable to create directory\n");
    }
}

static void cmd_rd(const char *args) {
    char path[PATH_MAX_CMD];

    if (!args[0]) {
        printf("Required parameter missing\n");
        return;
    }
    strncpy(path, args, sizeof(path) - 1);
    path[sizeof(path) - 1] = '\0';
    normalize_slashes(path);
    if (rmdir(path) != 0) {
        printf("Invalid path, not directory,\nor directory not empty\n");
    }
}

/* Returns 1 to keep looping, 0 to exit. */
static int dispatch(char *line) {
    char cmd[32];
    char *args;

    trim(line);
    if (!line[0]) {
        return 1;
    }
    args = split_cmd(line, cmd, sizeof(cmd));

    if (cmd_is(cmd, "EXIT") || cmd_is(cmd, "QUIT")) {
        return 0;
    }
    if (cmd_is(cmd, "VER")) {
        cmd_ver();
    } else if (cmd_is(cmd, "HELP") || cmd_is(cmd, "?")) {
        cmd_help();
    } else if (cmd_is(cmd, "CLS")) {
        cmd_cls();
    } else if (cmd_is(cmd, "ECHO")) {
        cmd_echo(args);
    } else if (cmd_is(cmd, "CD") || cmd_is(cmd, "CHDIR")) {
        cmd_cd(args);
    } else if (cmd_is(cmd, "DIR")) {
        cmd_dir(args);
    } else if (cmd_is(cmd, "TYPE")) {
        cmd_type(args);
    } else if (cmd_is(cmd, "COPY")) {
        cmd_copy(args);
    } else if (cmd_is(cmd, "REN") || cmd_is(cmd, "RENAME")) {
        cmd_ren(args);
    } else if (cmd_is(cmd, "DEL") || cmd_is(cmd, "ERASE")) {
        cmd_del(args);
    } else if (cmd_is(cmd, "MD") || cmd_is(cmd, "MKDIR")) {
        cmd_md(args);
    } else if (cmd_is(cmd, "RD") || cmd_is(cmd, "RMDIR")) {
        cmd_rd(args);
    } else if (cmd_is(cmd, "REM")) {
        /* comment */
    } else {
        printf("Bad command or file name\n");
    }
    return 1;
}

int main(int argc, char **argv) {
    char line[LINE_MAX];

    (void)argc;
    (void)argv;

    printf("SLOW-32 Command   Version 0.1\n");
    printf("(C) SLOW-32 desk\n\n");

    for (;;) {
        printf("%s", PROMPT);
        fflush(stdout);
        if (!fgets(line, (int)sizeof(line), stdin)) {
            printf("\n");
            break;
        }
        if (!dispatch(line)) {
            break;
        }
    }
    return 0;
}
