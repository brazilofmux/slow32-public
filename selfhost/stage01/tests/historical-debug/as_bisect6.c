#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <s32_formats.h>

#define MAX_TOK 8
#define MAX_LBL 512
#define MAX_REL 1024

static char g_lbl_name_pool[32768];
static uint32_t g_lbl_name_off[MAX_LBL];
static uint32_t g_lbl_name_ptr = 0;
static uint32_t g_nlbl = 0;
static uint8_t g_lbl_glob[MAX_LBL];

static uint32_t g_rel_sec[MAX_REL];
static uint32_t g_rel_off[MAX_REL];
static uint32_t g_rel_typ[MAX_REL];
static uint32_t g_rel_sym[MAX_REL];
static int32_t g_rel_add[MAX_REL];
static uint32_t g_nrel = 0;

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
    i = (int)g_nlbl;
    g_nlbl++;
    return i;
}

static int add_reloc(uint32_t typ, uint32_t off, const char *name) {
    int li;
    if (g_nrel >= MAX_REL) return -1;
    li = get_lbl(name);
    if (li < 0) return -1;
    g_rel_sec[g_nrel] = 0;
    g_rel_off[g_nrel] = off;
    g_rel_typ[g_nrel] = typ;
    g_rel_sym[g_nrel] = (uint32_t)li;
    g_rel_add[g_nrel] = 0;
    g_nrel++;
    return 0;
}

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
    int idx;
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
        if (strcmp(tok[0], ".global") == 0 && n >= 2) {
            idx = get_lbl(tok[1]);
            if (idx < 0) { fclose(out); fclose(in); return 4; }
            g_lbl_glob[idx] = 1;
        } else if (strcmp(tok[0], "la") == 0 && n >= 3) {
            if (add_reloc(S32O_REL_HI20, 0, tok[2]) != 0) { fclose(out); fclose(in); return 5; }
            if (add_reloc(S32O_REL_LO12, 4, tok[2]) != 0) { fclose(out); fclose(in); return 6; }
        } else if (strcmp(tok[0], "call") == 0 && n >= 2) {
            if (add_reloc(S32O_REL_HI20, 8, tok[1]) != 0) { fclose(out); fclose(in); return 7; }
            if (add_reloc(S32O_REL_LO12, 12, tok[1]) != 0) { fclose(out); fclose(in); return 8; }
        }
    }

    fprintf(out, "labels=%u relocs=%u\n", g_nlbl, g_nrel);
    fclose(out);
    fclose(in);
    return 0;
}
