/* cg_stubs.h — host-gcc-buildable mock of the codegen globals that
 * obj_writer.h consumes. Used by tests that exercise the writer (and
 * crt0 emitter) before the real codegen header exists.
 */

#ifndef CG_STUBS_H
#define CG_STUBS_H

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

#endif /* CG_STUBS_H */
