/* cc-x64.c -- Cross-compiler: C → x86-64 ELF
 *
 * Uses the same frontend as cc (Stage 06) but emits native
 * x86-64 Linux ELF binaries via x64_encode.h + elf_writer.h.
 *
 * Usage: cc-x64 input.c output
 *
 * NOTE: The lexer (c_lexer_gen.c) declares its own libc prototypes
 * for SLOW-32 compatibility. We must NOT include standard headers
 * before it. Instead, we provide the minimal declarations needed.
 */

/* The frontend headers (lexer, parser, ast, etc.) are designed for SLOW-32's
 * cc compiler, which is lenient about redeclarations and return types.
 * When compiling on a host GCC/Clang, we need compatibility shims.
 *
 * Strategy: suppress host libc builtins, let the frontend headers provide
 * all declarations, and compile with -fno-builtin.
 */

/* Libc functions - declared with the SLOW-32 frontend's signatures.
 * These must come FIRST, before any headers that might re-declare them. */
int open(char *path, int flags, ...);
int close(int fd);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);
char *malloc(int size);
void free(char *ptr);
char *memcpy(char *dst, char *src, int n);
char *memset(char *dst, int c, int n);
int strcmp(char *a, char *b);
int strncmp(char *a, char *b, int n);
int strlen(char *s);
char *strdup(char *s);
char *calloc(int n, int size);
int fputs(char *s, int f);
int fputc(int c, int f);
void exit(int status);
int stderr;
#define NULL 0

/* fput_uint: used by lexer/parser for error reporting */
static void fput_uint(int f, int v) {
    char buf[12];
    int i;
    int neg;
    if (v == 0) { write(f, "0", 1); return; }
    neg = 0;
    if (v < 0) { neg = 1; v = 0 - v; }
    i = 0;
    while (v > 0) {
        buf[i] = 48 + (v % 10);
        v = v / 10;
        i = i + 1;
    }
    if (neg) write(f, "-", 1);
    while (i > 0) {
        i = i - 1;
        write(f, buf + i, 1);
    }
}

/* Skip the frontend's own libc declarations since we provided them above */
#define S12CC_X64_HOST 1

/* Include generated lexer — skip its libc preamble */
/* We handle this by defining the symbols it would declare */
#include "c_lexer_gen.c"

#include "pp.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"
#include "optimize.h"

/* x86-64 backend */
#include "x64_encode.h"
#include "elf_writer.h"
#include "codegen_x64.h"

int main(int argc, char **argv) {
    int fd;
    int n;
    int i;
    int j;
    int last_slash;
    int argi;
    char *infile;
    char *outfile;
    char *src;
    Node *prog;

    /* Parse arguments: [-I dir] input.c output */
    pp_idir[0] = 0;
    infile = 0;
    outfile = 0;
    argi = 1;
    while (argi < argc) {
        if (argv[argi][0] == 45 && argv[argi][1] == 73) {  /* "-I" */
            if (argv[argi][2] != 0) {
                i = 0; j = 2;
                while (argv[argi][j] != 0) {
                    pp_idir[i] = argv[argi][j];
                    i = i + 1; j = j + 1;
                }
                if (i > 0 && pp_idir[i - 1] != 47) { pp_idir[i] = 47; i = i + 1; }
                pp_idir[i] = 0;
            } else if (argi + 1 < argc) {
                argi = argi + 1;
                i = 0; j = 0;
                while (argv[argi][j] != 0) {
                    pp_idir[i] = argv[argi][j];
                    i = i + 1; j = j + 1;
                }
                if (i > 0 && pp_idir[i - 1] != 47) { pp_idir[i] = 47; i = i + 1; }
                pp_idir[i] = 0;
            }
        } else if (infile == 0) {
            infile = argv[argi];
        } else if (outfile == 0) {
            outfile = argv[argi];
        }
        argi = argi + 1;
    }

    if (infile == 0 || outfile == 0) {
        write(2, "Usage: cc-x64 [-I dir] input.c output\n", 41);
        return 1;
    }

    /* Extract source directory for #include resolution */
    last_slash = -1;
    i = 0;
    while (infile[i] != 0) {
        if (infile[i] == 47) last_slash = i;
        i = i + 1;
    }
    if (last_slash >= 0) {
        i = 0;
        while (i <= last_slash) {
            pp_sdir[i] = infile[i];
            i = i + 1;
        }
        pp_sdir[i] = 0;
    } else {
        pp_sdir[0] = 0;
    }

    /* Init preprocessor */
    pp_ndefs = 0;
    pp_skip = 0;
    pp_dep = 0;
    ps_ntypedefs = 0;

    /* Read source */
    fd = open(infile, 0);
    if (fd < 0) {
        write(2, "cc-x64: cannot open input\n", 29);
        return 1;
    }
    src = malloc(LEX_SRC_SZ);
    n = read(fd, src, LEX_SRC_SZ - 1);
    close(fd);
    if (n < 0) n = 0;
    src[n] = 0;

    /* Lex + Parse */
    lex_init(src, n);
    prog = parse_program();

    /* Semantic analysis */
    sema(prog);

    /* AST optimization */
    optimize(prog);

    /* x86-64 code generation + ELF output */
    gen_program(prog);

    /* Write ELF */
    if (elf_write_file(outfile) < 0) {
        write(2, "cc-x64: cannot write output\n", 31);
        return 1;
    }

    write(2, "cc-x64: done\n", 16);

    return 0;
}
