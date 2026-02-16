#include <stdio.h>
#include <string.h>

#define MAX_TOK 8

static void strip_comment(char *s) {
    while (*s) {
        if (*s == '#') { *s = 0; return; }
        s++;
    }
}

static char *trim(char *s) {
    int n;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') s++;
    n = (int)strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t' || s[n - 1] == '\r' || s[n - 1] == '\n')) {
        s[n - 1] = 0;
        n--;
    }
    return s;
}

static int split(char *s, char *tok[]) {
    int n = 0;
    while (*s && n < MAX_TOK) {
        while (*s == ' ' || *s == '\t' || *s == ',') s++;
        if (*s == 0) break;
        tok[n++] = s;
        while (*s && *s != ' ' && *s != '\t' && *s != ',') s++;
        if (*s == 0) break;
        *s = 0;
        s++;
    }
    return n;
}

int main(int argc, char **argv) {
    FILE *in;
    FILE *out;
    char line[1024];
    char *tok[MAX_TOK];
    int n;
    if (argc != 3) return 1;
    in = fopen(argv[1], "rb");
    if (!in) return 2;
    out = fopen(argv[2], "wb");
    if (!out) { fclose(in); return 3; }
    while (fgets(line, sizeof(line), in)) {
        char *p;
        strip_comment(line);
        p = trim(line);
        if (!*p) continue;
        n = split(p, tok);
        if (n > 0) {
            fputs(tok[0], out);
            fputc('\n', out);
        }
    }
    fclose(out);
    fclose(in);
    return 0;
}
