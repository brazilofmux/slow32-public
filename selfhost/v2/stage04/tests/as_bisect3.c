#include <stdio.h>
#include <string.h>

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

int main(int argc, char **argv) {
    FILE *in;
    FILE *out;
    char line[1024];
    unsigned n = 0;
    if (argc != 3) return 1;
    in = fopen(argv[1], "rb");
    if (!in) return 2;
    out = fopen(argv[2], "wb");
    if (!out) { fclose(in); return 3; }
    while (fgets(line, sizeof(line), in)) {
        char *p;
        n++;
        strip_comment(line);
        p = trim(line);
        if (*p) {
            fputc('X', out);
        }
    }
    fclose(out);
    fclose(in);
    return (n == 0) ? 4 : 0;
}
