/* cc-x64.c -- Cross-compiler: C → x86-64 ELF
 *
 * Uses the same frontend as cc (Stage 07) but emits native
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
void exit(int status);
#define NULL 0

/* fd-based I/O: the frontend uses fdputs/fdputc/fdputuint with int fd.
 * Implement via write() so they work on both host and SLOW-32. */
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

/* fput_uint: used by lexer/parser for error reporting */
static void fdputuint(int f, int v) {
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
#include "../stage07/hir.h"
#include "../stage07/hir_lower.h"
#include "../stage07/hir_ssa.h"
#include "../stage07/hir_opt.h"
#include "../stage07/hir_licm.h"

/* x86-64 backend */
#include "x64_encode.h"
#include "elf_writer.h"
#include "codegen_x64.h"
#include "obj_writer.h"
#include "hir_codegen_x64.h"

static char *hir_op_name(int op) {
    if (op == HI_NOP) return "nop";
    if (op == HI_ICONST) return "iconst";
    if (op == HI_PARAM) return "param";
    if (op == HI_ADD) return "add";
    if (op == HI_SUB) return "sub";
    if (op == HI_MUL) return "mul";
    if (op == HI_DIV) return "div";
    if (op == HI_REM) return "rem";
    if (op == HI_AND) return "and";
    if (op == HI_OR) return "or";
    if (op == HI_XOR) return "xor";
    if (op == HI_SLL) return "sll";
    if (op == HI_SRA) return "sra";
    if (op == HI_SRL) return "srl";
    if (op == HI_SEQ) return "seq";
    if (op == HI_SNE) return "sne";
    if (op == HI_SLT) return "slt";
    if (op == HI_SGT) return "sgt";
    if (op == HI_SLE) return "sle";
    if (op == HI_SGE) return "sge";
    if (op == HI_SLTU) return "sltu";
    if (op == HI_SGTU) return "sgtu";
    if (op == HI_SLEU) return "sleu";
    if (op == HI_SGEU) return "sgeu";
    if (op == HI_NEG) return "neg";
    if (op == HI_NOT) return "not";
    if (op == HI_BNOT) return "bnot";
    if (op == HI_LOAD) return "load";
    if (op == HI_STORE) return "store";
    if (op == HI_ALLOCA) return "alloca";
    if (op == HI_GADDR) return "gaddr";
    if (op == HI_SADDR) return "saddr";
    if (op == HI_FADDR) return "faddr";
    if (op == HI_BR) return "br";
    if (op == HI_BRC) return "brc";
    if (op == HI_RET) return "ret";
    if (op == HI_CALL) return "call";
    if (op == HI_CALLP) return "callp";
    if (op == HI_PHI) return "phi";
    if (op == HI_COPY) return "copy";
    if (op == HI_ADDI) return "addi";
    if (op == HI_GETFP) return "getfp";
    if (op == HI_CALLHI) return "callhi";
    if (op == HI_FADD) return "fadd";
    if (op == HI_FSUB) return "fsub";
    if (op == HI_FMUL) return "fmul";
    if (op == HI_FDIV) return "fdiv";
    if (op == HI_FNEG) return "fneg";
    if (op == HI_FEQ) return "feq";
    if (op == HI_FLT) return "flt";
    if (op == HI_FLE) return "fle";
    if (op == HI_FCVT_ItoF) return "fcvt_itof";
    if (op == HI_FCVT_FtoI) return "fcvt_ftoi";
    if (op == HI_FCVT_FtoD) return "fcvt_ftod";
    if (op == HI_FCVT_DtoF) return "fcvt_dtof";
    if (op == HI_FCONST) return "fconst";
    return "op?";
}

static void hir_dump_one_inst(int idx) {
    int j;
    fdputs("    i", 2);
    fdputuint(2, idx);
    fdputs(": ", 2);
    fdputs(hir_op_name(h_kind[idx]), 2);
    fdputs(" ty=", 2);
    fdputuint(2, h_ty[idx]);
    if (h_src1[idx] >= 0) {
        fdputs(" s1=i", 2);
        fdputuint(2, h_src1[idx]);
    }
    if (h_src2[idx] >= 0) {
        fdputs(" s2=i", 2);
        fdputuint(2, h_src2[idx]);
    }
    if (h_val[idx] != 0 || h_kind[idx] == HI_ICONST || h_kind[idx] == HI_PARAM ||
        h_kind[idx] == HI_ADDI || h_kind[idx] == HI_BR || h_kind[idx] == HI_BRC) {
        fdputs(" val=", 2);
        fdputuint(2, h_val[idx]);
    }
    if (h_name[idx]) {
        fdputs(" name=", 2);
        fdputs(h_name[idx], 2);
    }
    if ((h_kind[idx] == HI_CALL || h_kind[idx] == HI_CALLP) && h_cbase[idx] >= 0) {
        fdputs(" args=[", 2);
        j = 0;
        while (j < h_val[idx]) {
            if (j > 0) fdputs(", ", 2);
            fdputs("i", 2);
            fdputuint(2, h_carg[h_cbase[idx] + j]);
            j = j + 1;
        }
        fdputs("]", 2);
    }
    if (h_kind[idx] == HI_PHI && h_pbase[idx] >= 0) {
        fdputs(" phi=[", 2);
        j = 0;
        while (j < h_pcnt[idx]) {
            if (j > 0) fdputs(", ", 2);
            fdputs("b", 2);
            fdputuint(2, h_pblk[h_pbase[idx] + j]);
            fdputs(":i", 2);
            fdputuint(2, h_pval[h_pbase[idx] + j]);
            j = j + 1;
        }
        fdputs("]", 2);
    }
    fdputs("\n", 2);
}

static void hir_dump_function(Node *fn) {
    int b;
    int i;

    fdputs("hir_x64: function ", 2);
    fdputs(fn->name, 2);
    fdputs("\n", 2);
    fdputs("  blocks=", 2);
    fdputuint(2, bb_nblk);
    fdputs(" inst=", 2);
    fdputuint(2, h_ninst);
    fdputs(" promos=", 2);
    fdputuint(2, ssa_npromo);
    fdputs(" rpo=", 2);
    fdputuint(2, ssa_rpo_cnt);
    fdputs(" licm_hoisted=", 2);
    fdputuint(2, licm_stat_hoisted);
    fdputs("\n", 2);

    b = 0;
    while (b < bb_nblk) {
        fdputs("  block b", 2);
        fdputuint(2, b);
        fdputs(":", 2);
        if (ssa_rpo[b] >= 0) {
            fdputs(" rpo=", 2);
            fdputuint(2, ssa_rpo[b]);
        }
        if (ssa_idom[b] >= 0) {
            fdputs(" idom=b", 2);
            fdputuint(2, ssa_idom[b]);
        }
        fdputs("\n", 2);

        i = ssa_phi_head[b];
        while (i >= 0) {
            hir_dump_one_inst(i);
            i = ssa_phi_next[i];
        }

        i = bb_start[b];
        while (i < bb_end[b]) {
            if (h_kind[i] != HI_NOP && h_kind[i] != HI_PHI) hir_dump_one_inst(i);
            i = i + 1;
        }

        i = licm_head[b];
        while (i >= 0) {
            fdputs("    hoisted ", 2);
            hir_dump_one_inst(i);
            i = licm_next[i];
        }

        b = b + 1;
    }
}

static void hir_dump_program(Node *prog) {
    Node *fn;

    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) {
            licm_stat_hoisted = 0;
            hl_func(fn);
            hir_ssa_construct();
            hir_opt();
            hir_licm();
            hir_dump_function(fn);
        }
        fn = fn->next;
    }
}

/* Emit a crt0.o with the hand-coded _start stub.
 * This is the proper kernel-ABI _start that reads argc/argv/envp
 * from the stack and calls main + __save_envp + sys_exit. */
static int emit_crt0(char *outfile) {
    x64_off = 0;
    cg_nfuncs = 0;
    cg_ncpatches = 0;
    cg_ndrelocs = 0;
    cg_nstrings = 0;
    cg_str_pool_used = 0;
    cg_nglobals = 0;
    cg_rodata_len = 0;
    cg_data_len = 0;
    cg_bss_size = 0;
    cg_object_mode = 1;

    /* Record _start function */
    cg_func_name[cg_nfuncs] = "_start";
    cg_func_off[cg_nfuncs] = x64_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* _start:
     *   xor ebp, ebp
     *   mov edi, [rsp]        ; argc
     *   lea rsi, [rsp+8]      ; argv
     *   movsxd rax, edi        ; sign-extend argc
     *   inc rax
     *   shl rax, 3
     *   add rax, rsi
     *   mov rdx, rax          ; envp
     *   push rsi              ; save argv
     *   push rdi              ; save argc
     *   mov rdi, rdx
     *   call __save_envp
     *   pop rdi               ; argc
     *   pop rsi               ; argv
     *   call main
     *   mov edi, eax
     *   mov eax, 60
     *   syscall
     */
    x64_xor_rr(X64_RBP, X64_RBP);
    x64_mov_rm(X64_RDI, X64_RSP, 0);
    x64_lea(X64_RSI, X64_RSP, 8);
    x64_movsxd(X64_RAX, X64_RDI);
    x64_add_ri64(X64_RAX, 1);
    x64_byte(0x48); x64_byte(0xC1); x64_byte(0xE0); x64_byte(0x03);
    x64_add_rr64(X64_RAX, X64_RSI);
    x64_mov_rr64(X64_RDX, X64_RAX);
    x64_push(X64_RSI);
    x64_push(X64_RDI);
    x64_mov_rr64(X64_RDI, X64_RDX);
    x64_byte(0xE8);
    cg_cpatch_name[cg_ncpatches] = "__save_envp";
    cg_cpatch_off[cg_ncpatches] = x64_off;
    cg_ncpatches = cg_ncpatches + 1;
    x64_dword(0);
    x64_pop(X64_RDI);
    x64_pop(X64_RSI);
    x64_byte(0xE8);
    cg_cpatch_name[cg_ncpatches] = "main";
    cg_cpatch_off[cg_ncpatches] = x64_off;
    cg_ncpatches = cg_ncpatches + 1;
    x64_dword(0);
    x64_mov_rr(X64_RDI, X64_RAX);
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    return obj_write_file(outfile);
}

int main(int argc, char **argv) {
    int fd;
    int n;
    int i;
    int j;
    int last_slash;
    int argi;
    int compile_only;
    int crt0_mode;
    int hir_dump_mode;
    int hir_mode;
    char *infile;
    char *outfile;
    char *src;
    Node *prog;

    /* x86-64 target: pointers are 8 bytes */
    ty_ptr_size = 8;

    /* Parse arguments: [--crt0] [-c] [-I dir] input.c output */
    pp_idir[0] = 0;
    infile = 0;
    outfile = 0;
    compile_only = 0;
    crt0_mode = 0;
    hir_dump_mode = 0;
    hir_mode = 0;
    argi = 1;
    while (argi < argc) {
        if (argv[argi][0] == 45 && argv[argi][1] == 45) {
            /* Long option starting with "--" */
            if (strcmp(argv[argi], "--crt0") == 0) {
                crt0_mode = 1;
            } else if (strcmp(argv[argi], "--hir-dump") == 0) {
                hir_dump_mode = 1;
            } else if (strcmp(argv[argi], "--hir") == 0) {
                hir_mode = 1;
            }
        } else if (argv[argi][0] == 45 && argv[argi][1] == 99 && argv[argi][2] == 0) {
            /* "-c" */
            compile_only = 1;
        } else if (argv[argi][0] == 45 && argv[argi][1] == 73) {  /* "-I" */
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
        } else if (argv[argi][0] == 45 && argv[argi][1] == 111 && argv[argi][2] == 0) {
            /* "-o" followed by output filename */
            if (argi + 1 < argc) {
                argi = argi + 1;
                outfile = argv[argi];
            }
        } else if (infile == 0) {
            infile = argv[argi];
        } else if (outfile == 0) {
            outfile = argv[argi];
        }
        argi = argi + 1;
    }

    /* --crt0 mode: emit crt0.o, no source file needed */
    if (crt0_mode) {
        if (outfile == 0) outfile = "crt0.o";
        if (emit_crt0(outfile) < 0) {
            write(2, "cc-x64: cannot write crt0\n", 25);
            return 1;
        }
        write(2, "cc-x64: wrote crt0\n", 19);
        return 0;
    }

    if (infile == 0 || outfile == 0) {
        write(2, "Usage: cc-x64 [--hir|--hir-dump] [-c] [-o output] [-I dir] input.c\n", 67);
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

    if (hir_dump_mode) {
        hir_dump_program(prog);
    }

    /* x86-64 code generation */
    if (hir_mode) {
        hx_gen_program(prog, compile_only);
    } else {
        cg_object_mode = compile_only;
        gen_program(prog);
    }

    if (compile_only) {
        /* Write relocatable object (.o) */
        if (obj_write_file(outfile) < 0) {
            write(2, "cc-x64: cannot write object\n", 28);
            return 1;
        }
        write(2, "cc-x64: wrote object\n", 21);
    } else {
        /* Write ELF executable */
        if (elf_write_file(outfile) < 0) {
            write(2, "cc-x64: cannot write output\n", 31);
            return 1;
        }
        write(2, "cc-x64: done\n", 13);
    }

    return 0;
}
