#include <stdio.h>
#include <stdint.h>
#include <string.h>

#define MAX_TOK 8
#define MAX_TEXT 65536
#define MAX_DATA 65536

static uint8_t g_text[MAX_TEXT];
static uint8_t g_data[MAX_DATA];
static uint32_t g_tsz = 0;
static uint32_t g_dsz = 0;
static uint32_t g_sec = 0;

static int emit8(uint8_t b) {
    if (g_sec == 0) {
        if (g_tsz >= MAX_TEXT) return -1;
        g_text[g_tsz++] = b;
    } else {
        if (g_dsz >= MAX_DATA) return -1;
        g_data[g_dsz++] = b;
    }
    return 0;
}

static int emit32(uint32_t w) {
    if (emit8((uint8_t)(w & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 8) & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 16) & 255)) != 0) return -1;
    if (emit8((uint8_t)((w >> 24) & 255)) != 0) return -1;
    return 0;
}

static void strip_comment(char *s) {
    while (*s) { if (*s == '#') { *s = 0; return; } s++; }
}

static char *trim(char *s) {
    int n;
    while (*s == ' ' || *s == '\t' || *s == '\r' || *s == '\n') s++;
    n = (int)strlen(s);
    while (n > 0 && (s[n - 1] == ' ' || s[n - 1] == '\t' || s[n - 1] == '\r' || s[n - 1] == '\n')) { s[n - 1] = 0; n--; }
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
        if (n <= 0) continue;
        if (tok[0][strlen(tok[0]) - 1] == ':') continue;
        if (strcmp(tok[0], ".text") == 0) { g_sec = 0; continue; }
        if (strcmp(tok[0], ".data") == 0) { g_sec = 1; continue; }
        if (strcmp(tok[0], ".byte") == 0) {
            int i;
            for (i = 1; i < n; i++) {
                int ok;
                int v = (int)strtol(tok[i], (char **)0, 0);
                ok = 1;
                if (!ok) { fclose(out); fclose(in); return 4; }
                if (emit8((uint8_t)(v & 255)) != 0) { fclose(out); fclose(in); return 5; }
            }
            continue;
        }
        if (emit32(0x12345678u) != 0) { fclose(out); fclose(in); return 6; }
    }

    fprintf(out, "text=%u data=%u\n", g_tsz, g_dsz);
    fclose(out);
    fclose(in);
    return 0;
}
