/* test_obj_global.c — smoke test for ADRP+LDR relocations to a .data global.
 *
 *   _start:
 *       adrp x0, the_answer              ; R_AARCH64_ADR_PREL_PG_HI21
 *       ldr  w0, [x0, #:lo12:the_answer] ; R_AARCH64_LDST32_ABS_LO12_NC
 *       mov  w8, #93
 *       svc  #0
 *
 *   .data
 *   the_answer:
 *       .word 42
 *
 * Pass = exit code 42 after `ld out/test_obj_global.o -o ...`.
 */

int open(char *path, int flags, int mode);
int close(int fd);
int read(int fd, char *buf, int len);
int write(int fd, char *buf, int len);

#include "../a64_encode.h"

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

#include <stdio.h>

int main(void) {
    int adrp_off; int ldr_off;

    a64_off = 0;
    cg_nfuncs = 0; cg_nglobals = 0; cg_ncpatches = 0; cg_ndrelocs = 0;
    cg_rodata_len = 0; cg_data_len = 0; cg_bss_size = 0;

    /* Define _start at .text offset 0 */
    cg_func_name[cg_nfuncs] = "_start";
    cg_func_off[cg_nfuncs]  = a64_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* adrp x0, the_answer  (imm21 patched by linker) */
    adrp_off = a64_off;
    a64_adrp(A64_X0, 0);

    cg_cpatch_name[cg_ncpatches] = "the_answer";
    cg_cpatch_off[cg_ncpatches]  = adrp_off;
    cg_cpatch_kind[cg_ncpatches] = A64K_ADR_HI21;
    cg_ncpatches = cg_ncpatches + 1;

    /* ldr w0, [x0, #0]  (imm12 patched by linker) */
    ldr_off = a64_off;
    a64_ldr_w_imm(A64_X0, A64_X0, 0);

    cg_cpatch_name[cg_ncpatches] = "the_answer";
    cg_cpatch_off[cg_ncpatches]  = ldr_off;
    cg_cpatch_kind[cg_ncpatches] = A64K_LDST32_LO12;
    cg_ncpatches = cg_ncpatches + 1;

    /* mov w8, #93 ; svc #0 */
    a64_mov_w_imm(A64_X8, 93);
    a64_svc(0);

    /* Define `the_answer` global at .data offset 0, value 42 */
    cg_glob_name[cg_nglobals]     = "the_answer";
    cg_glob_data_off[cg_nglobals] = 0;
    cg_glob_in_bss[cg_nglobals]   = 0;
    cg_nglobals = cg_nglobals + 1;

    cg_data[0] = 42; cg_data[1] = 0; cg_data[2] = 0; cg_data[3] = 0;
    cg_data_len = 4;

    if (obj_write_file("out/test_obj_global.o") < 0) {
        fprintf(stderr, "obj_write_file failed\n");
        return 1;
    }
    printf("wrote out/test_obj_global.o (%d code bytes, %d patches, %d data)\n",
           a64_off, cg_ncpatches, cg_data_len);
    return 0;
}
