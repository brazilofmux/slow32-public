/* s12cc.c -- Stage 12 C compiler
 *
 * Phase 1 narrow spike: int arithmetic, if/while, function calls.
 * Ragel lexer → recursive-descent parser → AST → tree-walk codegen.
 *
 * Usage: s12cc input.c output.s
 *
 * Compiled by stage11 s32-cc.
 */

#include "c_lexer_gen.c"

/* --- fd-based I/O wrappers (stderr=2 from lexer) --- */
int write(int fd, char *buf, int len);
int read(int fd, char *buf, int len);
int open(char *path, int flags);
int close(int fd);

int fputs(char *s, int fd) {
    int len;
    len = strlen(s);
    write(fd, s, len);
    return 0;
}

int fputc(int c, int fd) {
    char buf[1];
    buf[0] = c;
    write(fd, buf, 1);
    return c;
}

void fput_uint(int fd, int val) {
    char buf[11];
    int i;
    int d;
    int started;
    if (val == 0) { fputc(48, fd); return; }
    i = 0;
    d = 1000000000;
    started = 0;
    while (d > 0) {
        if (val >= d || started) {
            buf[i] = 48 + val / d;
            val = val - (val / d) * d;
            i = i + 1;
            started = 1;
        }
        d = d / 10;
    }
    write(fd, buf, i);
}

#include "pp.h"
#include "ast.h"
#include "parser.h"
#include "codegen.h"

int main(int argc, char **argv) {
    int fd;
    int n;
    int i;
    int last_slash;
    char *src;
    struct Node *prog;

    if (argc < 3) {
        fputs("Usage: s12cc input.c output.s\n", stderr);
        return 1;
    }

    /* Extract source directory for #include resolution */
    last_slash = -1;
    i = 0;
    while (argv[1][i] != 0) {
        if (argv[1][i] == 47) last_slash = i;  /* '/' */
        i = i + 1;
    }
    if (last_slash >= 0) {
        i = 0;
        while (i <= last_slash) {
            pp_sdir[i] = argv[1][i];
            i = i + 1;
        }
        pp_sdir[i] = 0;
    } else {
        pp_sdir[0] = 0;
    }

    /* Init preprocessor state */
    pp_ndefs = 0;
    pp_skip = 0;
    pp_dep = 0;

    /* Read source file */
    fd = open(argv[1], 0);
    if (fd < 0) {
        fputs("s12cc: cannot open input: ", stderr);
        fputs(argv[1], stderr);
        fputc(10, stderr);
        return 1;
    }
    src = malloc(LEX_SRC_SZ);
    if (!src) {
        fputs("s12cc: out of memory\n", stderr);
        return 1;
    }
    n = read(fd, src, LEX_SRC_SZ - 1);
    close(fd);
    if (n < 0) n = 0;
    src[n] = 0;

    /* Lex + Parse */
    lex_init(src, n);
    prog = parse_program();

    /* Codegen */
    cg_olen = 0;
    cg_lbl = 0;
    cg_loop_depth = 0;
    cg_sw_depth = 0;
    gen_program(prog);

    /* Write output */
    fd = open(argv[2], 26);  /* O_WRONLY | O_CREAT | O_TRUNC */
    if (fd < 0) {
        fputs("s12cc: cannot open output: ", stderr);
        fputs(argv[2], stderr);
        fputc(10, stderr);
        return 1;
    }
    write(fd, cg_out, cg_olen);
    close(fd);

    return 0;
}
