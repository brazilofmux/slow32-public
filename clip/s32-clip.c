/*
 * s32-clip — Clipper experiment: .prg to C.
 *
 * Control flow becomes C (IF / ELSEIF / DO WHILE / DO CASE / EXIT / LOOP).
 * FUNCTION / PROCEDURE bodies become C functions registered with the
 * expression evaluator (PAD_ZERO() in INDEX/SEEK hits C).
 * SET PROCEDURE TO <file> inlines <file>.prg from the source directory
 * when it exists; otherwise the command rides clip_cmd() at runtime.
 * Other commands we do not lower become clip_cmd() (cmd_execute).
 */

#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE  512
#define MAX_NEST  32
#define MAX_LINES 4000
#define MAX_FUNCS 64
#define MAX_FILES 16
#define MAX_PARAMS 16
#define MAX_NAME  32

typedef struct {
    char text[MAX_LINE];
    char src[256];
    int lineno;
} src_line_t;

typedef struct {
    char name[MAX_NAME];
    int header;
    int start;
    int end;
    char params[MAX_PARAMS][MAX_NAME];
    int nparams;
} func_t;

typedef struct {
    FILE *out;
    const char *inpath;
    int lineno;
    int depth;
    char nest[MAX_NEST];
    int in_fn;
    int param_i;
    int ended;
} emit_t;

static src_line_t g_lines[MAX_LINES];
static int g_nlines;
static char g_loaded[MAX_FILES][256];
static int g_nloaded;
static char g_inlined[MAX_FILES][MAX_NAME];
static int g_ninlined;

static int icmp_n(const char *a, const char *b, int n) {
    int i;
    for (i = 0; i < n; i++) {
        int ca = toupper((unsigned char)a[i]);
        int cb = toupper((unsigned char)b[i]);
        if (ca != cb) {
            return ca - cb;
        }
        if (a[i] == '\0' || b[i] == '\0') {
            break;
        }
    }
    return 0;
}

static int is_kw(const char *p, const char *kw) {
    int n = (int)strlen(kw);
    if (icmp_n(p, kw, n) != 0) {
        return 0;
    }
    return p[n] == '\0' || isspace((unsigned char)p[n]) || p[n] == '(';
}

static const char *skip_ws(const char *p) {
    while (*p && isspace((unsigned char)*p)) {
        p++;
    }
    return p;
}

static void strip_comment(char *line) {
    char *p = line;
    int in_str = 0;
    char q = 0;
    if (*skip_ws(line) == '*') {
        *line = '\0';
        return;
    }
    if (is_kw(skip_ws(line), "NOTE")) {
        *line = '\0';
        return;
    }
    while (*p) {
        if (!in_str && p[0] == '&' && p[1] == '&') {
            *p = '\0';
            return;
        }
        if (!in_str && (*p == '"' || *p == '\'')) {
            in_str = 1;
            q = *p;
        } else if (in_str && *p == q) {
            in_str = 0;
        }
        p++;
    }
}

static void rtrim(char *s) {
    int n = (int)strlen(s);
    while (n > 0 && isspace((unsigned char)s[n - 1])) {
        s[--n] = '\0';
    }
}

static void c_escape(FILE *out, const char *s) {
    fputc('"', out);
    while (*s) {
        if (*s == '\\' || *s == '"') {
            fputc('\\', out);
        }
        fputc(*s, out);
        s++;
    }
    fputc('"', out);
}

static void emit_indent(FILE *out, int depth) {
    int i;
    for (i = 0; i <= depth; i++) {
        fputs("    ", out);
    }
}

static void emit_cmd(FILE *out, int depth, const char *p) {
    emit_indent(out, depth);
    fprintf(out, "clip_cmd(");
    c_escape(out, p);
    fprintf(out, "); /* interp */\n");
}

static int find_assign(const char *p, char *name, int nsz, const char **expr) {
    const char *s;
    int n;
    p = skip_ws(p);
    if (!isalpha((unsigned char)*p) && *p != '_') {
        return 0;
    }
    s = p;
    while (isalnum((unsigned char)*s) || *s == '_') {
        s++;
    }
    n = (int)(s - p);
    if (n <= 0 || n >= nsz) {
        return 0;
    }
    s = skip_ws(s);
    if (*s != '=') {
        return 0;
    }
    if (s[1] == '=') {
        return 0;
    }
    memcpy(name, p, (size_t)n);
    name[n] = '\0';
    *expr = skip_ws(s + 1);
    return 1;
}

static int find_store(const char *p, char *name, int nsz, char *expr, int esz) {
    const char *to;
    const char *q;
    int elen;
    p = skip_ws(p);
    if (!is_kw(p, "STORE")) {
        return 0;
    }
    p = skip_ws(p + 5);
    to = NULL;
    for (q = p; *q; q++) {
        if ((q == p || isspace((unsigned char)q[-1])) &&
            is_kw(q, "TO") && (q[2] == '\0' || isspace((unsigned char)q[2]))) {
            to = q;
        }
    }
    if (!to) {
        return 0;
    }
    elen = (int)(to - p);
    while (elen > 0 && isspace((unsigned char)p[elen - 1])) {
        elen--;
    }
    if (elen <= 0 || elen >= esz) {
        return 0;
    }
    memcpy(expr, p, (size_t)elen);
    expr[elen] = '\0';
    to = skip_ws(to + 2);
    q = to;
    while (isalnum((unsigned char)*q) || *q == '_') {
        q++;
    }
    elen = (int)(q - to);
    if (elen <= 0 || elen >= nsz) {
        return 0;
    }
    memcpy(name, to, (size_t)elen);
    name[elen] = '\0';
    return 1;
}

static int is_unlowered_block(const char *p) {
    return is_kw(p, "FOR") || is_kw(p, "SCAN") || is_kw(p, "TEXT") ||
           is_kw(p, "BEGIN");
}

static int parse_namelist(const char *p, char names[][MAX_NAME], int maxn) {
    int n = 0;
    p = skip_ws(p);
    while (*p && n < maxn) {
        int i = 0;
        if (!isalpha((unsigned char)*p) && *p != '_') {
            break;
        }
        while (isalnum((unsigned char)*p) || *p == '_') {
            if (i < MAX_NAME - 1) {
                names[n][i++] = *p;
            }
            p++;
        }
        names[n][i] = '\0';
        n++;
        p = skip_ws(p);
        if (*p == ',') {
            p = skip_ws(p + 1);
            continue;
        }
        break;
    }
    return n;
}

static int parse_func_header(const char *p, char *name, int nsz,
                             char params[][MAX_NAME], int *nparams) {
    int i = 0;
    *nparams = 0;
    if (is_kw(p, "FUNCTION")) {
        p = skip_ws(p + 8);
    } else if (is_kw(p, "PROCEDURE")) {
        p = skip_ws(p + 9);
    } else {
        return 0;
    }
    if (!isalpha((unsigned char)*p) && *p != '_') {
        return 0;
    }
    while (isalnum((unsigned char)*p) || *p == '_') {
        if (i < nsz - 1) {
            name[i++] = (char)toupper((unsigned char)*p);
        }
        p++;
    }
    name[i] = '\0';
    p = skip_ws(p);
    if (*p == '(') {
        char inner[MAX_LINE];
        int k = 0;
        p++;
        while (*p && *p != ')' && k < MAX_LINE - 1) {
            inner[k++] = *p++;
        }
        inner[k] = '\0';
        *nparams = parse_namelist(inner, params, MAX_PARAMS);
    }
    return name[0] != '\0';
}

static int parse_set_proc(const char *p, char *name, int nsz) {
    const char *q;
    int i = 0;
    if (!is_kw(p, "SET")) {
        return 0;
    }
    q = skip_ws(p + 3);
    if (is_kw(q, "PROCEDURE")) {
        q = skip_ws(q + 9);
    } else if (is_kw(q, "PROC")) {
        q = skip_ws(q + 4);
    } else {
        return 0;
    }
    if (!is_kw(q, "TO")) {
        return 0;
    }
    q = skip_ws(q + 2);
    if (*q == '"' || *q == '\'') {
        char quote = *q++;
        while (*q && *q != quote && i < nsz - 1) {
            name[i++] = *q++;
        }
    } else {
        while (*q && !isspace((unsigned char)*q) && i < nsz - 1) {
            name[i++] = *q++;
        }
    }
    name[i] = '\0';
    if (i >= 4 && icmp_n(name + i - 4, ".PRG", 4) == 0) {
        name[i - 4] = '\0';
    }
    return name[0] != '\0';
}

static int was_inlined(const char *name) {
    int i;
    for (i = 0; i < g_ninlined; i++) {
        if (icmp_n(g_inlined[i], name, MAX_NAME) == 0 &&
            (int)strlen(g_inlined[i]) == (int)strlen(name)) {
            return 1;
        }
    }
    return 0;
}

static void mark_inlined(const char *name) {
    if (g_ninlined >= MAX_FILES || was_inlined(name)) {
        return;
    }
    strncpy(g_inlined[g_ninlined], name, MAX_NAME - 1);
    g_inlined[g_ninlined][MAX_NAME - 1] = '\0';
    g_ninlined++;
}

static int already_loaded(const char *path) {
    int i;
    for (i = 0; i < g_nloaded; i++) {
        if (strcmp(g_loaded[i], path) == 0) {
            return 1;
        }
    }
    return 0;
}

static void mark_loaded(const char *path) {
    if (g_nloaded >= MAX_FILES || already_loaded(path)) {
        return;
    }
    strncpy(g_loaded[g_nloaded], path, 255);
    g_loaded[g_nloaded][255] = '\0';
    g_nloaded++;
}

static void path_dir(const char *path, char *dir, int n) {
    const char *slash = strrchr(path, '/');
    int len;
    if (!slash) {
        strncpy(dir, ".", (size_t)n - 1);
        dir[n - 1] = '\0';
        return;
    }
    len = (int)(slash - path);
    if (len <= 0) {
        strncpy(dir, "/", (size_t)n - 1);
        dir[n - 1] = '\0';
        return;
    }
    if (len >= n) {
        len = n - 1;
    }
    memcpy(dir, path, (size_t)len);
    dir[len] = '\0';
}

static int load_prg(const char *path) {
    FILE *in;
    char buf[MAX_LINE];
    int lineno = 0;

    if (already_loaded(path)) {
        return 0;
    }
    in = fopen(path, "r");
    if (!in) {
        perror(path);
        return -1;
    }
    mark_loaded(path);
    while (fgets(buf, sizeof(buf), in)) {
        const char *p;
        lineno++;
        rtrim(buf);
        strip_comment(buf);
        p = skip_ws(buf);
        if (*p == '\0') {
            continue;
        }
        if (g_nlines >= MAX_LINES) {
            fprintf(stderr, "%s: too many lines\n", path);
            fclose(in);
            return -1;
        }
        strncpy(g_lines[g_nlines].text, p, MAX_LINE - 1);
        g_lines[g_nlines].text[MAX_LINE - 1] = '\0';
        strncpy(g_lines[g_nlines].src, path, 255);
        g_lines[g_nlines].src[255] = '\0';
        g_lines[g_nlines].lineno = lineno;
        g_nlines++;
    }
    fclose(in);
    return 0;
}

static int try_load_proc(const char *from_src, const char *name) {
    char dir[256];
    char path[512];
    const char *exts[] = {".prg", ".PRG", "", NULL};
    int i;

    path_dir(from_src, dir, (int)sizeof(dir));
    for (i = 0; exts[i]; i++) {
        FILE *fp;
        snprintf(path, sizeof(path), "%s/%s%s", dir, name, exts[i]);
        fp = fopen(path, "r");
        if (!fp) {
            continue;
        }
        fclose(fp);
        if (already_loaded(path)) {
            mark_inlined(name);
            return 0;
        }
        if (load_prg(path) != 0) {
            return -1;
        }
        mark_inlined(name);
        return 0;
    }
    return 0;
}

static int line_in_func(int idx, const func_t *fns, int nfns) {
    int i;
    for (i = 0; i < nfns; i++) {
        if (idx >= fns[i].header && idx < fns[i].end) {
            return 1;
        }
    }
    return 0;
}

static int emit_stmt(emit_t *e, const char *p) {
    char name[MAX_NAME];
    char expr[MAX_LINE];
    char names[MAX_PARAMS][MAX_NAME];
    int n, i;

    if (is_kw(p, "PARAMETERS") || is_kw(p, "PARAMETER")) {
        if (!e->in_fn) {
            fprintf(stderr, "%s:%d: PARAMETERS outside FUNCTION\n",
                    e->inpath, e->lineno);
            return -1;
        }
        p = skip_ws(p + (is_kw(p, "PARAMETERS") ? 10 : 9));
        n = parse_namelist(p, names, MAX_PARAMS);
        for (i = 0; i < n; i++) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_param(&scope, ");
            c_escape(e->out, names[i]);
            fprintf(e->out, ", args, nargs, %d);\n", e->param_i++);
        }
        return 0;
    }
    if (e->in_fn && (is_kw(p, "PRIVATE") || is_kw(p, "LOCAL"))) {
        p = skip_ws(p + (is_kw(p, "PRIVATE") ? 7 : 5));
        n = parse_namelist(p, names, MAX_PARAMS);
        for (i = 0; i < n; i++) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_private(&scope, ");
            c_escape(e->out, names[i]);
            fprintf(e->out, ");\n");
        }
        return 0;
    }
    if (is_kw(p, "RETURN")) {
        const char *q = skip_ws(p + 6);
        if (e->in_fn) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "clip_return(&scope, ");
            c_escape(e->out, q);
            fprintf(e->out, ", result);\n");
            emit_indent(e->out, e->depth);
            fprintf(e->out, "return 0;\n");
            return 0;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_fini();\n");
        emit_indent(e->out, e->depth);
        fprintf(e->out, "return 0;\n");
        if (e->depth == 0) {
            e->ended = 1;
        }
        return 0;
    }
    if (is_kw(p, "QUIT")) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_fini();\n");
        emit_indent(e->out, e->depth);
        if (e->in_fn) {
            fprintf(e->out, "exit(0);\n");
        } else {
            fprintf(e->out, "return 0;\n");
            if (e->depth == 0) {
                e->ended = 1;
            }
        }
        return 0;
    }
    if (is_kw(p, "EXIT")) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "break;\n");
        return 0;
    }
    if (is_kw(p, "LOOP")) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "continue;\n");
        return 0;
    }
    if (is_kw(p, "ENDDO")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'W') {
            fprintf(stderr, "%s:%d: ENDDO without DO WHILE\n", e->inpath, e->lineno);
            return -1;
        }
        e->depth--;
        emit_indent(e->out, e->depth);
        fprintf(e->out, "}\n");
        return 0;
    }
    if (is_kw(p, "ENDIF")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ENDIF without IF\n", e->inpath, e->lineno);
            return -1;
        }
        e->depth--;
        emit_indent(e->out, e->depth);
        fprintf(e->out, "}\n");
        return 0;
    }
    if (is_kw(p, "ENDCASE")) {
        if (e->depth < 1 ||
            (e->nest[e->depth - 1] != 'C' && e->nest[e->depth - 1] != 'K' &&
             e->nest[e->depth - 1] != 'O')) {
            fprintf(stderr, "%s:%d: ENDCASE without DO CASE\n", e->inpath, e->lineno);
            return -1;
        }
        if (e->nest[e->depth - 1] == 'K' || e->nest[e->depth - 1] == 'O') {
            e->depth--;
            emit_indent(e->out, e->depth);
            fprintf(e->out, "}\n");
        } else {
            e->depth--;
        }
        return 0;
    }
    if (is_kw(p, "ELSEIF") ||
        (is_kw(p, "ELSE") && is_kw(skip_ws(p + 4), "IF"))) {
        const char *q;
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ELSEIF without IF\n", e->inpath, e->lineno);
            return -1;
        }
        if (is_kw(p, "ELSEIF")) {
            q = skip_ws(p + 6);
        } else {
            q = skip_ws(skip_ws(p + 4) + 2);
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else if (clip_truth(");
        c_escape(e->out, q);
        fprintf(e->out, ")) {\n");
        return 0;
    }
    if (is_kw(p, "ELSE")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'I') {
            fprintf(stderr, "%s:%d: ELSE without IF\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else {\n");
        return 0;
    }
    if (is_kw(p, "CASE")) {
        const char *q = skip_ws(p + 4);
        if (e->depth < 1) {
            fprintf(stderr, "%s:%d: CASE without DO CASE\n", e->inpath, e->lineno);
            return -1;
        }
        if (e->nest[e->depth - 1] == 'C') {
            emit_indent(e->out, e->depth - 1);
            fprintf(e->out, "if (clip_truth(");
            c_escape(e->out, q);
            fprintf(e->out, ")) {\n");
            e->nest[e->depth - 1] = 'K';
            return 0;
        }
        if (e->nest[e->depth - 1] == 'K') {
            emit_indent(e->out, e->depth - 1);
            fprintf(e->out, "} else if (clip_truth(");
            c_escape(e->out, q);
            fprintf(e->out, ")) {\n");
            return 0;
        }
        fprintf(stderr, "%s:%d: CASE after OTHERWISE\n", e->inpath, e->lineno);
        return -1;
    }
    if (is_kw(p, "OTHERWISE")) {
        if (e->depth < 1 || e->nest[e->depth - 1] != 'K') {
            fprintf(stderr, "%s:%d: OTHERWISE without CASE\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth - 1);
        fprintf(e->out, "} else {\n");
        e->nest[e->depth - 1] = 'O';
        return 0;
    }
    if (is_kw(p, "DO")) {
        const char *q = skip_ws(p + 2);
        if (is_kw(q, "WHILE")) {
            q = skip_ws(q + 5);
            if (e->depth >= MAX_NEST) {
                fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
                return -1;
            }
            emit_indent(e->out, e->depth);
            fprintf(e->out, "while (clip_truth(");
            c_escape(e->out, q);
            fprintf(e->out, ")) {\n");
            e->nest[e->depth++] = 'W';
            return 0;
        }
        if (is_kw(q, "CASE")) {
            if (e->depth >= MAX_NEST) {
                fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
                return -1;
            }
            e->nest[e->depth++] = 'C';
            return 0;
        }
        emit_cmd(e->out, e->depth, p);
        return 0;
    }
    if (is_kw(p, "IF")) {
        const char *q = skip_ws(p + 2);
        if (e->depth >= MAX_NEST) {
            fprintf(stderr, "%s:%d: nesting too deep\n", e->inpath, e->lineno);
            return -1;
        }
        emit_indent(e->out, e->depth);
        fprintf(e->out, "if (clip_truth(");
        c_escape(e->out, q);
        fprintf(e->out, ")) {\n");
        e->nest[e->depth++] = 'I';
        return 0;
    }
    if (is_unlowered_block(p) || is_kw(p, "NEXT") || is_kw(p, "ENDSCAN") ||
        is_kw(p, "ENDTEXT") || is_kw(p, "RECOVER") || is_kw(p, "END")) {
        fprintf(stderr, "%s:%d: unlowered block: %s\n", e->inpath, e->lineno, p);
        return -1;
    }
    if (is_kw(p, "SET")) {
        char proc[MAX_NAME];
        if (parse_set_proc(p, proc, MAX_NAME) && was_inlined(proc)) {
            emit_indent(e->out, e->depth);
            fprintf(e->out, "/* SET PROCEDURE TO %s — compiled in */\n", proc);
            return 0;
        }
        emit_cmd(e->out, e->depth, p);
        return 0;
    }
    if (p[0] == '?' && p[1] == '?') {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_q(");
        c_escape(e->out, skip_ws(p + 2));
        fprintf(e->out, ", 0);\n");
        return 0;
    }
    if (p[0] == '?') {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_q(");
        c_escape(e->out, skip_ws(p + 1));
        fprintf(e->out, ", 1);\n");
        return 0;
    }
    if (find_store(p, name, (int)sizeof(name), expr, (int)sizeof(expr))) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_let(");
        c_escape(e->out, name);
        fprintf(e->out, ", ");
        c_escape(e->out, expr);
        fprintf(e->out, ");\n");
        return 0;
    }
    if (find_assign(p, name, (int)sizeof(name), &p)) {
        emit_indent(e->out, e->depth);
        fprintf(e->out, "clip_let(");
        c_escape(e->out, name);
        fprintf(e->out, ", ");
        c_escape(e->out, p);
        fprintf(e->out, ");\n");
        return 0;
    }

    emit_cmd(e->out, e->depth, p);
    return 0;
}

static int emit_function(FILE *out, const func_t *fn) {
    emit_t e;
    int i;

    memset(&e, 0, sizeof(e));
    e.out = out;
    e.in_fn = 1;
    e.param_i = 0;

    fprintf(out, "static int clip_fn_%s(value_t *args, int nargs, value_t *result) {\n",
            fn->name);
    fprintf(out, "    clip_scope_t scope;\n");
    fprintf(out, "    clip_enter(&scope);\n");
    for (i = 0; i < fn->nparams; i++) {
        fprintf(out, "    clip_param(&scope, ");
        c_escape(out, fn->params[i]);
        fprintf(out, ", args, nargs, %d);\n", e.param_i++);
    }
    for (i = fn->start; i < fn->end; i++) {
        e.inpath = g_lines[i].src;
        e.lineno = g_lines[i].lineno;
        if (emit_stmt(&e, g_lines[i].text) != 0) {
            return -1;
        }
    }
    if (e.depth != 0) {
        fprintf(stderr, "%s:%d: unclosed block in FUNCTION %s\n",
                g_lines[fn->header].src, g_lines[fn->header].lineno, fn->name);
        return -1;
    }
    fprintf(out, "    clip_end(&scope, result);\n");
    fprintf(out, "    return 0;\n");
    fprintf(out, "}\n\n");
    return 0;
}

int main(int argc, char **argv) {
    const char *outpath = NULL;
    const char *inputs[MAX_FILES];
    int ninputs = 0;
    FILE *out;
    func_t fns[MAX_FUNCS];
    int nfns = 0;
    emit_t e;
    int i, j;
    int scan;

    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-o") == 0 && i + 1 < argc) {
            outpath = argv[++i];
        } else if (argv[i][0] != '-') {
            if (ninputs >= MAX_FILES) {
                fprintf(stderr, "too many input files\n");
                return 1;
            }
            inputs[ninputs++] = argv[i];
        }
    }
    if (ninputs < 1 || !outpath) {
        fprintf(stderr, "Usage: s32-clip input.prg [more.prg ...] -o output.c\n");
        return 1;
    }

    for (i = 0; i < ninputs; i++) {
        if (load_prg(inputs[i]) != 0) {
            return 1;
        }
    }

    /* Pull in SET PROCEDURE files sitting next to any loaded source. */
    for (scan = 0; scan < g_nlines; scan++) {
        char proc[MAX_NAME];
        if (parse_set_proc(g_lines[scan].text, proc, MAX_NAME)) {
            if (try_load_proc(g_lines[scan].src, proc) != 0) {
                return 1;
            }
        }
    }

    i = 0;
    while (i < g_nlines) {
        if (is_kw(g_lines[i].text, "FUNCTION") ||
            is_kw(g_lines[i].text, "PROCEDURE")) {
            if (nfns >= MAX_FUNCS) {
                fprintf(stderr, "%s:%d: too many functions\n",
                        g_lines[i].src, g_lines[i].lineno);
                return 1;
            }
            if (!parse_func_header(g_lines[i].text, fns[nfns].name, MAX_NAME,
                                   fns[nfns].params, &fns[nfns].nparams)) {
                fprintf(stderr, "%s:%d: bad FUNCTION/PROCEDURE header\n",
                        g_lines[i].src, g_lines[i].lineno);
                return 1;
            }
            fns[nfns].header = i;
            fns[nfns].start = i + 1;
            j = i + 1;
            while (j < g_nlines &&
                   !is_kw(g_lines[j].text, "FUNCTION") &&
                   !is_kw(g_lines[j].text, "PROCEDURE")) {
                j++;
            }
            fns[nfns].end = j;
            nfns++;
            i = j;
            continue;
        }
        i++;
    }

    out = fopen(outpath, "w");
    if (!out) {
        perror(outpath);
        return 1;
    }

    fprintf(out, "/* generated by s32-clip from %s */\n", inputs[0]);
    fprintf(out, "#include \"cliprt.h\"\n");
    fprintf(out, "#include <stdlib.h>\n\n");

    for (i = 0; i < nfns; i++) {
        if (emit_function(out, &fns[i]) != 0) {
            fclose(out);
            return 1;
        }
    }

    fprintf(out, "int main(void) {\n");
    fprintf(out, "    clip_init();\n");
    for (i = 0; i < nfns; i++) {
        fprintf(out, "    clip_register_fn(\"%s\", clip_fn_%s);\n",
                fns[i].name, fns[i].name);
    }

    memset(&e, 0, sizeof(e));
    e.out = out;
    for (i = 0; i < g_nlines; i++) {
        if (line_in_func(i, fns, nfns)) {
            continue;
        }
        e.inpath = g_lines[i].src;
        e.lineno = g_lines[i].lineno;
        if (emit_stmt(&e, g_lines[i].text) != 0) {
            fclose(out);
            return 1;
        }
    }
    if (e.depth != 0) {
        fprintf(stderr, "%s: unclosed block\n", inputs[0]);
        fclose(out);
        return 1;
    }
    if (!e.ended) {
        fprintf(out, "    clip_fini();\n");
        fprintf(out, "    return 0;\n");
    }
    fprintf(out, "}\n");
    fclose(out);
    return 0;
}
