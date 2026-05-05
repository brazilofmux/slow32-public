/* cc-a64.c — Cross-compiler: C → AArch64 ELF.
 *
 * Mirror of stage07-cross/cc-x64.c targeting AArch64. Uses the same
 * shared frontend (lex/parse/sema/AST/HIR/optimize) and a tree-walk
 * AArch64 backend (codegen_a64.h).
 *
 * Usage:
 *   cc-a64 [-c] [-o output] [-I dir] input.c
 *   cc-a64 --crt0 [-o output]            (emit crt0.o)
 *
 * NOTE: The lexer (c_lexer_gen.c) declares its own libc prototypes for
 * SLOW-32 compatibility. We must NOT include standard headers before it.
 * Instead, we pre-declare what we need with the frontend's signatures.
 */

/* Libc — declared with the SLOW-32 frontend's signatures so the lexer's
 * own redeclarations match. */
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
void exit(int status);
#define NULL 0

/* fdputs / fdputc / fdputuint — used by the parser/lexer for error output. */
static int fdputs(char *s, int f) {
    int len;
    len = 0;
    while (s[len]) len = len + 1;
    write(f, s, len);
    return 0;
}

static int fdputc(int c, int f) {
    char ch;
    ch = c;
    write(f, &ch, 1);
    return c;
}

static void fdputuint(int f, int v) {
    char buf[12];
    int i; int neg;
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

/* Tell the frontend not to redeclare libc — we did it. */
#define S12CC_X64_HOST 1
#define S12CC_TARGET_A64 1

#include "c_lexer_gen.c"

#include "pp.h"
#include "ast.h"
#include "parser.h"
#include "sema.h"
#include "optimize.h"

/* AArch64 backend.  Include order matters. */
#include "a64_encode.h"
#include "a64_reloc_kinds.h"
#include "elf_writer.h"
#include "codegen_a64.h"
#include "obj_writer.h"
#include "crt0_emit.h"

/* HIR pipeline pieces — pulled in for the optional --hir codegen path. */
#include "../stage07/hir.h"
#include "../stage07/hir_lower.h"
#include "../stage07/hir_ssa.h"
#include "../stage07/hir_opt.h"
#include "../stage07/hir_licm.h"
#include "hir_burg_a64.h"
#include "hir_regalloc_a64.h"
#include "hir_codegen_a64.h"

int main(int argc, char **argv) {
    int fd; int n; int i; int j;
    int last_slash; int argi;
    int compile_only;
    int crt0_mode;
    int hir_mode;
    char *infile;
    char *outfile;
    char *src;
    Node *prog;

    /* AArch64: pointers are 8 bytes — align the frontend's type system. */
    ty_ptr_size = 8;

    pp_nidirs = 0;
    infile = 0;
    outfile = 0;
    compile_only = 0;
    crt0_mode = 0;
    hir_mode = 0;
    argi = 1;

    while (argi < argc) {
        if (argv[argi][0] == 45 && argv[argi][1] == 45) {
            if (strcmp(argv[argi], "--crt0") == 0) crt0_mode = 1;
            else if (strcmp(argv[argi], "--hir") == 0) hir_mode = 1;
        } else if (argv[argi][0] == 45 && argv[argi][1] == 99 && argv[argi][2] == 0) {
            compile_only = 1;
        } else if (argv[argi][0] == 45 && argv[argi][1] == 73) {
            if (argv[argi][2] != 0) {
                pp_add_idir(argv[argi] + 2);
            } else if (argi + 1 < argc) {
                argi = argi + 1;
                pp_add_idir(argv[argi]);
            }
        } else if (argv[argi][0] == 45 && argv[argi][1] == 111 && argv[argi][2] == 0) {
            if (argi + 1 < argc) { argi = argi + 1; outfile = argv[argi]; }
        } else if (infile == 0) {
            infile = argv[argi];
        } else if (outfile == 0) {
            outfile = argv[argi];
        }
        argi = argi + 1;
    }

    /* Fallback selfhost headers used by stage07 tests and bootstrap code. */
    pp_add_idir("selfhost/stage07/include");
    pp_add_idir("../stage07/include");

    if (crt0_mode) {
        if (outfile == 0) outfile = "crt0.o";
        if (emit_crt0_object(outfile) < 0) {
            write(2, "cc-a64: cannot write crt0\n", 26);
            return 1;
        }
        write(2, "cc-a64: wrote crt0\n", 19);
        return 0;
    }

    if (infile == 0 || outfile == 0) {
        write(2, "Usage: cc-a64 [-c] [-o output] [-I dir] input.c\n", 49);
        return 1;
    }

    /* Extract source dir for #include resolution. */
    last_slash = -1;
    i = 0;
    while (infile[i] != 0) {
        if (infile[i] == 47) last_slash = i;
        i = i + 1;
    }
    if (last_slash >= 0) {
        i = 0;
        while (i <= last_slash) { pp_sdir[i] = infile[i]; i = i + 1; }
        pp_sdir[i] = 0;
    } else {
        pp_sdir[0] = 0;
    }

    pp_ndefs = 0;
    pp_skip = 0;
    pp_dep = 0;
    ps_ntypedefs = 0;

    fd = open(infile, 0);
    if (fd < 0) {
        write(2, "cc-a64: cannot open input\n", 26);
        return 1;
    }
    src = malloc(LEX_SRC_SZ);
    n = read(fd, src, LEX_SRC_SZ - 1);
    close(fd);
    if (n < 0) n = 0;
    src[n] = 0;

    lex_init(src, n);
    prog = parse_program();
    sema(prog);
    optimize(prog);

    cg_object_mode = compile_only;
    if (hir_mode) hx_gen_program(prog, compile_only);
    else          gen_program(prog);

    if (compile_only) {
        if (obj_write_file(outfile) < 0) {
            write(2, "cc-a64: cannot write object\n", 28);
            return 1;
        }
        write(2, "cc-a64: wrote object\n", 21);
    } else {
        if (elf_write_file(outfile) < 0) {
            write(2, "cc-a64: cannot write output\n", 28);
            return 1;
        }
        write(2, "cc-a64: done\n", 13);
    }

    return 0;
}
