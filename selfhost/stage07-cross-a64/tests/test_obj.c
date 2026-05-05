/* test_obj.c — smoke test for obj_writer.h.
 *
 * Hand-build a relocatable AArch64 ELF object containing two functions:
 *
 *   _start:                                  ; entry
 *       bl   helper                          ; -> R_AARCH64_CALL26
 *       mov  w8, #93
 *       svc  #0
 *
 *   helper:
 *       mov  w0, #42                         ; return value
 *       ret
 *
 * After writing test_obj.o, the runner script links it with system ld
 * (no libc) and execs the result. Pass = exit code 42.
 */

int open(char *path, int flags, int mode);
int close(int fd);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);

#include "../a64_encode.h"

/* ----- Stub the cg_* globals the writer expects ------------------------- */

/* String comparison — a freestanding strcmp clone for the writer. */
static int cg_strcmp(char *a, char *b) {
    int i;
    i = 0;
    while (a[i] == b[i]) {
        if (a[i] == 0) return 0;
        i = i + 1;
    }
    return (a[i] & 0xFF) - (b[i] & 0xFF);
}

#define DRELOC_STRING 0
#define DRELOC_GLOBAL 1

#define CG_MAX_FUNCS    256
#define CG_MAX_GLOBALS  256
#define CG_MAX_PATCHES  4096
#define CG_MAX_DRELOCS  4096
#define CG_MAX_STRINGS  4096

static char *cg_func_name[CG_MAX_FUNCS];
static int   cg_func_off[CG_MAX_FUNCS];
static int   cg_nfuncs;

static char *cg_glob_name[CG_MAX_GLOBALS];
static int   cg_glob_data_off[CG_MAX_GLOBALS];
static int   cg_glob_in_bss[CG_MAX_GLOBALS];
static int   cg_nglobals;

static char *cg_cpatch_name[CG_MAX_PATCHES];
static int   cg_cpatch_off[CG_MAX_PATCHES];
static int   cg_cpatch_kind[CG_MAX_PATCHES];
static int   cg_ncpatches;

static int   cg_dreloc_off[CG_MAX_DRELOCS];
static int   cg_dreloc_kind[CG_MAX_DRELOCS];
static int   cg_dreloc_idx[CG_MAX_DRELOCS];
static int   cg_ndrelocs;

static int   cg_str_rodata_off[CG_MAX_STRINGS];

static unsigned char cg_rodata[1024];
static int           cg_rodata_len;
static unsigned char cg_data[1024];
static int           cg_data_len;
static int           cg_bss_size;

#include "../obj_writer.h"

/* --------------------------------------------------------------------- */

#include <stdio.h>

int main(void) {
    int helper_off;
    int call_site;

    /* Reset everything */
    a64_off = 0;
    cg_nfuncs = 0;
    cg_nglobals = 0;
    cg_ncpatches = 0;
    cg_ndrelocs = 0;
    cg_rodata_len = 0;
    cg_data_len = 0;
    cg_bss_size = 0;

    /* === _start (entry) at offset 0 === */
    cg_func_name[cg_nfuncs] = "_start";
    cg_func_off[cg_nfuncs]  = a64_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* bl helper — instruction emitted with imm26 = 0; linker fills in */
    call_site = a64_off;
    a64_bl(0);

    /* Record the call patch */
    cg_cpatch_name[cg_ncpatches] = "helper";
    cg_cpatch_off[cg_ncpatches]  = call_site;
    cg_cpatch_kind[cg_ncpatches] = A64K_CALL26;
    cg_ncpatches = cg_ncpatches + 1;

    /* mov w8, #93 ; svc #0 */
    a64_mov_w_imm(A64_X8, 93);
    a64_svc(0);

    /* === helper === */
    helper_off = a64_off;
    cg_func_name[cg_nfuncs] = "helper";
    cg_func_off[cg_nfuncs]  = helper_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* mov w0, #42 ; ret */
    a64_mov_w_imm(A64_X0, 42);
    a64_ret();

    /* Write .o */
    if (obj_write_file("out/test_obj.o") < 0) {
        fprintf(stderr, "obj_write_file failed\n");
        return 1;
    }
    printf("wrote out/test_obj.o (%d code bytes, %d patches)\n",
           a64_off, cg_ncpatches);
    return 0;
}
