#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <s32_formats.h>

#define MAX_LINE 1024
#define MAX_TOK 8
#define MAX_LBL 512

enum { SEC_TEXT = 0, SEC_DATA = 1 };

static char g_lbl_name_pool[32768];
static uint32_t g_lbl_name_off[MAX_LBL];
static uint32_t g_lbl_name_ptr = 0;
static uint32_t g_lbl_sec[MAX_LBL];
static uint32_t g_lbl_val[MAX_LBL];
static uint8_t g_lbl_defd[MAX_LBL];
static uint8_t g_lbl_glob[MAX_LBL];
static uint8_t g_lbl_refd[MAX_LBL];
static uint32_t g_nlbl = 0;
static uint32_t g_sec = SEC_TEXT;

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

static void strip_comment(char *s) {
    while (*s) {
        if (*s == '#') { *s = 0; return; }
        s++;
    }
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

static int parse_num(const char *s, int *ok) {
    char *e;
    long v = strtol(s, &e, 0);
    if (*s == 0 || *e != 0) { *ok = 0; return 0; }
    *ok = 1;
    return (int)v;
}

static int find_lbl(const char *name) {
    uint32_t i;
    for (i = 0; i < g_nlbl; i++) {
        if (strcmp(g_lbl_name_pool + g_lbl_name_off[i], name) == 0) return (int)i;
    }
    return -1;
}

static int get_lbl(const char *name) {
    int i = find_lbl(name);
    uint32_t n;
    if (i >= 0) return i;
    if (g_nlbl >= MAX_LBL) return -1;
    n = (uint32_t)strlen(name) + 1u;
    if (g_lbl_name_ptr + n > 32768u) return -1;
    g_lbl_name_off[g_nlbl] = g_lbl_name_ptr;
    memcpy(g_lbl_name_pool + g_lbl_name_ptr, name, n);
    g_lbl_name_ptr += n;
    g_lbl_sec[g_nlbl] = 0;
    g_lbl_val[g_nlbl] = 0;
    g_lbl_defd[g_nlbl] = 0;
    g_lbl_glob[g_nlbl] = 0;
    g_lbl_refd[g_nlbl] = 0;
    i = (int)g_nlbl;
    g_nlbl++;
    return i;
}

static int do_align(int p) {
    (void)p;
    return 0;
}

static int handle(char *line) {
    char *tok[MAX_TOK];
    int n;
    char *c;
    int ok;

    strip_comment(line);
    line = trim(line);
    if (*line == 0) return 0;

    c = strchr(line, ':');
    if (c) {
        int li;
        *c = 0;
        li = get_lbl(trim(line));
        if (li < 0) return -1;
        g_lbl_defd[li] = 1;
        g_lbl_sec[li] = g_sec;
        g_lbl_val[li] = 0;
        line = trim(c + 1);
        if (*line == 0) return 0;
    }

    n = split(line, tok);
    if (n == 0) return 0;

    if (tok[0][0] == '.') {
        if (strcmp(tok[0], ".text") == 0) { g_sec = SEC_TEXT; return 0; }
        if (strcmp(tok[0], ".data") == 0) { g_sec = SEC_DATA; return 0; }
        if (strcmp(tok[0], ".global") == 0) { if (n < 2) return -1; ok = get_lbl(tok[1]); if (ok < 0) return -1; g_lbl_glob[ok] = 1; return 0; }
        if (strcmp(tok[0], ".align") == 0) { if (n < 2) return -1; ok = parse_num(tok[1], &n); if (!n) return -1; return do_align(ok); }
        if (strcmp(tok[0], ".byte") == 0) {
            int i;
            for (i = 1; i < n; i++) {
                int v = parse_num(tok[i], &ok);
                if (!ok) return -1;
                (void)v;
            }
            return 0;
        }
        return 0;
    }

    return 0;
}

int main(int argc, char **argv) {
    char line[MAX_LINE];
    FILE *in;
    uint32_t lno = 0;

    if (argc != 3) return 1;
    in = fopen(argv[1], "rb");
    if (!in) return 2;
    while (fgets(line, sizeof(line), in)) {
        lno++;
        if (handle(line) != 0) {
            fclose(in);
            return 3;
        }
    }
    fclose(in);
    return 0;
}
