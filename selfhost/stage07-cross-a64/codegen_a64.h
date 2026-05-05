/* codegen_a64.h — Tree-walk code generator targeting AArch64 ELF.
 *
 * Walks AST, emits machine code via a64_encode.h.
 * Stack-machine style: expressions evaluate into x0 (or w0 for 32-bit
 * results); binary ops push lhs, evaluate rhs, pop lhs into x1.
 *
 * Pushes use 16-byte slots (str x0, [sp, #-16]!) so SP stays 16-byte
 * aligned across the whole expression — that's what the AArch64 ABI
 * requires at every BL/BLR/RET.
 *
 * After codegen, wraps output in ELF via elf_writer.h, or emits a
 * relocatable .o via obj_writer.h (cg_object_mode = 1).
 *
 * --- Required includes (in this order) ---
 *   a64_encode.h        (a64_buf[], a64_off, a64_* encoders)
 *   a64_reloc_kinds.h   (A64K_* kind constants for cg_cpatch_kind[])
 *   elf_writer.h        (elf_* address helpers — for resolve_relocations)
 */

#ifndef CODEGEN_A64_H
#define CODEGEN_A64_H

/* Forward declaration: crt0_emit.h provides this and is included after us. */
static void emit_crt0_to_buf(void);

/* ============================================================================
 * State arrays
 * ============================================================================ */

#define CG_MAX_LABELS  16384
#define CG_MAX_PATCHES 16384

/* Forward-reference patches for branches whose target hasn't been seen yet.
 * Each entry records: which label this patch is for, and the byte offset of
 * the (placeholder) instruction. When the label is defined, we walk the patch
 * list and rewrite each instruction's immediate field. The "kind" is
 * encoded in the instruction word itself — we read it back to know which
 * imm field to patch. */
static int cg_lbl_off[CG_MAX_LABELS];   /* -1 = not yet defined */
static int cg_patch_lbl[CG_MAX_PATCHES];
static int cg_patch_off[CG_MAX_PATCHES];
static int cg_npatches;

static int cg_olen;   /* final code size (for the driver) */

/* String pool */
#define CG_MAX_STRINGS    1024
#define CG_STRING_POOL_SZ 65536

static char cg_str_pool[CG_STRING_POOL_SZ];
static int  cg_str_off[CG_MAX_STRINGS];
static int  cg_str_len[CG_MAX_STRINGS];
static int  cg_nstrings;
static int  cg_str_pool_used;

/* Global variables */
#define CG_MAX_GLOBALS 1024

static char *cg_glob_name[CG_MAX_GLOBALS];
static int   cg_glob_size[CG_MAX_GLOBALS];
static int   cg_glob_init[CG_MAX_GLOBALS];
static int   cg_glob_has_init[CG_MAX_GLOBALS];   /* 0=BSS, 1=scalar, 2=string */
static int   cg_glob_extern[CG_MAX_GLOBALS];     /* 1=undefined object symbol */
static int   cg_nglobals;

/* Function symbols */
#define CG_MAX_FUNCS 512

static char *cg_func_name[CG_MAX_FUNCS];
static int   cg_func_off[CG_MAX_FUNCS];
static int   cg_nfuncs;

/* Call/address patches.  Each entry needs a kind (A64K_*) so the writer
 * knows which AArch64 ELF reloc type to emit.
 *
 * The "name" field has special encoding for string-literal references:
 * the magic name "@@str" means "this is a relocation against .rodata
 * with addend = cg_str_rodata_off[cg_cpatch_addend[i]]".  For normal
 * (function/global) entries, addend is unused and stays 0. */
#define CG_MAX_CALL_PATCHES 8192

static char *cg_cpatch_name[CG_MAX_CALL_PATCHES];
static int   cg_cpatch_off[CG_MAX_CALL_PATCHES];
static int   cg_cpatch_kind[CG_MAX_CALL_PATCHES];
static int   cg_cpatch_addend[CG_MAX_CALL_PATCHES];
static int   cg_ncpatches;

/* Data-section relocations (8-byte ABS64 fixups). */
#define CG_MAX_DATA_RELOCS 16384

#define DRELOC_STRING 0
#define DRELOC_GLOBAL 1
#define DRELOC_BSS    2
#define DRELOC_SYMBOL 3

static int cg_dreloc_off[CG_MAX_DATA_RELOCS];
static int cg_dreloc_kind[CG_MAX_DATA_RELOCS];
static int cg_dreloc_idx[CG_MAX_DATA_RELOCS];
static char *cg_dreloc_name[CG_MAX_DATA_RELOCS];
static int cg_ndrelocs;

/* Object mode: write a .o instead of an executable. */
static int cg_object_mode;

/* Per-function state */
static int cg_epilog;
static int cg_frame_size;
static int cg_is_varargs;
static int cg_nparams;
static int cg_va_save_off;

#define CG_MAX_LOOP 32
static int cg_break_lbl[CG_MAX_LOOP];
static int cg_cont_lbl[CG_MAX_LOOP];
static int cg_loop_depth;

/* Switch state */
#define CG_MAX_CASE     256
#define CG_MAX_SW_DEPTH 8
static int cg_sw_val[CG_MAX_CASE];
static int cg_sw_lbl[CG_MAX_CASE];
static int cg_sw_base[CG_MAX_SW_DEPTH];
static int cg_sw_count[CG_MAX_SW_DEPTH];
static int cg_sw_def[CG_MAX_SW_DEPTH];
static int cg_sw_cur[CG_MAX_SW_DEPTH];
static int cg_sw_depth;

/* Data sections */
static unsigned char cg_rodata[65536];
static int           cg_rodata_len;
static unsigned char cg_data[65536];
static int           cg_data_len;
static int           cg_bss_size;

static int cg_glob_data_off[CG_MAX_GLOBALS];
static int cg_glob_in_bss[CG_MAX_GLOBALS];
static int cg_str_rodata_off[CG_MAX_STRINGS];

/* ============================================================================
 * Utilities
 * ============================================================================ */

static int cg_strcmp(char *a, char *b) {
    int i;
    i = 0;
    while (a[i] && a[i] == b[i]) i = i + 1;
    return a[i] - b[i];
}

/* AArch64 caller-uses x0 for return value; we use w0/x0 as our "result"
 * register and w1/x1 as the popped lhs in binary ops.  x9..x15 are scratch. */
#define CGR_RES   A64_X0    /* expression result lives here */
#define CGR_RHS   A64_X0    /* RHS into result reg before pop */
#define CGR_LHS   A64_X1    /* popped lhs */
#define CGR_TMP   A64_X9    /* general scratch */
#define CGR_TMP2  A64_X10
#define CGR_TMP3  A64_X11

/* A 32/64-bit selector based on a SLOW-32 type code. */
static int cg_is_64bit(int ty) {
    return ty_is_ptr(ty) || ty_is_llong(ty);
}

static int cg_is_ptr_type(int ty) {
    return cg_is_64bit(ty);
}

/* SLOW-32 frontend uses 4-byte slots; on AArch64 (64-bit), pointers and
 * long long are 8 bytes. We double the offset like the x86-64 sibling. */
static int cg_a64_offset(int slow32_offset) {
    return slow32_offset * 2;
}

/* ============================================================================
 * Stack-machine push/pop (16-byte slots to keep SP aligned at all times)
 * ============================================================================ */

static void cg_push(void) {
    /* str x0, [sp, #-16]!   (pre-index, allocate 16 bytes, store x0 at [sp]) */
    a64_str_x_pre(A64_X0, A64_SP, -16);
}

/* Pop into LHS register (x1). */
static void cg_pop(void) {
    /* ldr x1, [sp], #16     (post-index, load then deallocate) */
    a64_ldr_x_post(A64_X1, A64_SP, 16);
}

/* Pop into an arbitrary register (used by call sites that take args in xN). */
static void cg_pop_into(int xreg) {
    a64_ldr_x_post(xreg, A64_SP, 16);
}

/* ============================================================================
 * Label management
 *
 * Labels live as code-buffer offsets (cg_lbl_off[l]).  Backwards references
 * are emitted directly with the right offset; forward references emit a
 * placeholder and append to cg_patch_*.  When the label is later defined
 * (cg_ldef), the patch list is walked and each instruction's immediate
 * field is rewritten in place by reading back the opcode bits.
 *
 * We support patching:
 *   - B / BL (imm26)
 *   - B.cond, CBZ, CBNZ (imm19)
 * by sniffing the top 8 bits of the instruction word.
 * ============================================================================ */

static void cg_patch_branch(int site, int target) {
    int word; int diff; int top8; int kept;
    word = a64_read_inst(site);
    diff = target - site;
    top8 = (word >> 24) & 0xFF;
    /* Unconditional B (0x14) or BL (0x94) — imm26 in low bits. */
    if (top8 == 0x14 || top8 == 0x94) {
        int imm26;
        imm26 = (diff >> 2) & 0x03FFFFFF;
        kept  = word & 0xFC000000;
        a64_patch_inst(site, kept | imm26);
        return;
    }
    /* B.cond (0x54), CBZ (0x34/0xB4), CBNZ (0x35/0xB5) — imm19 at bits 23..5. */
    if (top8 == 0x54 || top8 == 0x34 || top8 == 0xB4
                     || top8 == 0x35 || top8 == 0xB5) {
        int imm19;
        imm19 = (diff >> 2) & 0x7FFFF;
        kept  = word & 0xFF00001F;
        a64_patch_inst(site, kept | (imm19 << 5));
        return;
    }
    /* Any other shape is a programming error. */
    fdputs("cc-a64: cg_patch_branch: unrecognised branch opcode\n", 2);
    exit(1);
}

static void cg_ldef(int l) {
    int i;
    cg_lbl_off[l] = a64_off;
    i = 0;
    while (i < cg_npatches) {
        if (cg_patch_lbl[i] == l) {
            cg_patch_branch(cg_patch_off[i], a64_off);
            cg_npatches = cg_npatches - 1;
            cg_patch_lbl[i] = cg_patch_lbl[cg_npatches];
            cg_patch_off[i] = cg_patch_off[cg_npatches];
        } else {
            i = i + 1;
        }
    }
}

/* Unconditional branch to label.  Backward = direct offset; forward = patch. */
static void cg_jmp_label(int l) {
    if (cg_lbl_off[l] >= 0) {
        a64_b(cg_lbl_off[l] - a64_off);
    } else {
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = a64_off;
        cg_npatches = cg_npatches + 1;
        a64_b(0);    /* placeholder (offset zero) */
    }
}

/* Conditional branch. */
static void cg_jcc_label(int cond, int l) {
    if (cg_lbl_off[l] >= 0) {
        a64_b_cond(cond, cg_lbl_off[l] - a64_off);
    } else {
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = a64_off;
        cg_npatches = cg_npatches + 1;
        a64_b_cond(cond, 0);
    }
}

/* CBZ Wt, label  — branch if w0 == 0. */
static void cg_cbz_label(int wt, int l) {
    if (cg_lbl_off[l] >= 0) {
        a64_cbz_w(wt, cg_lbl_off[l] - a64_off);
    } else {
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = a64_off;
        cg_npatches = cg_npatches + 1;
        a64_cbz_w(wt, 0);
    }
}

/* CBNZ Wt, label */
static void cg_cbnz_label(int wt, int l) {
    if (cg_lbl_off[l] >= 0) {
        a64_cbnz_w(wt, cg_lbl_off[l] - a64_off);
    } else {
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = a64_off;
        cg_npatches = cg_npatches + 1;
        a64_cbnz_w(wt, 0);
    }
}

/* ============================================================================
 * Memory helpers (load/store with type-correct width)
 * ============================================================================ */

/* Load local variable from [x29 + scaled_offset] into x0. */
static void cg_load_local(int ty, int offset) {
    int off;
    off = cg_a64_offset(offset);
    /* Negative offset from FP. AArch64 unsigned-imm12 only handles
     * positive offsets — for negative we emit ADD x9, x29, #-off (or
     * SUB x9, x29, #off) and load via x9. We can only guarantee small
     * frames for now, so use the SUB-then-load pattern. */
    if (off >= 0 && off <= 32760) {
        if (cg_is_ptr_type(ty)) { a64_ldr_x_imm(A64_X0, A64_X29, off); return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
            if (ty & TY_UNSIGNED) a64_ldrb_imm(A64_X0, A64_X29, off);
            else                  a64_ldrsb_w_imm(A64_X0, A64_X29, off);
            return;
        }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
            if (ty & TY_UNSIGNED) a64_ldrh_imm(A64_X0, A64_X29, off);
            else                  a64_ldrsh_w_imm(A64_X0, A64_X29, off);
            return;
        }
        a64_ldr_w_imm(A64_X0, A64_X29, off);
        return;
    }
    /* Negative offset (or too big): compute the address into x9 first.
     * Offsets are always negative in our frontend layout, so we use SUB. */
    if (off < 0) {
        int neg;
        neg = -off;
        /* x9 = x29 - neg.  If neg <= 4095 single SUB; else two SUBs (lsl12 then). */
        if (neg <= 4095) {
            a64_sub_x_imm(CGR_TMP, A64_X29, neg);
        } else {
            a64_sub_x_imm(CGR_TMP, A64_X29, neg & 0xFFF);
            a64_sub_x_imm_lsl12(CGR_TMP, CGR_TMP, (neg >> 12) & 0xFFF);
        }
        if (cg_is_ptr_type(ty)) { a64_ldr_x_imm(A64_X0, CGR_TMP, 0); return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
            if (ty & TY_UNSIGNED) a64_ldrb_imm(A64_X0, CGR_TMP, 0);
            else                  a64_ldrsb_w_imm(A64_X0, CGR_TMP, 0);
            return;
        }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
            if (ty & TY_UNSIGNED) a64_ldrh_imm(A64_X0, CGR_TMP, 0);
            else                  a64_ldrsh_w_imm(A64_X0, CGR_TMP, 0);
            return;
        }
        a64_ldr_w_imm(A64_X0, CGR_TMP, 0);
        return;
    }
    /* Big positive offset (rare in our layout) — handle with lsl12 ADDs.
     * We don't expect this path right now. */
    a64_add_x_imm_lsl12(CGR_TMP, A64_X29, (off >> 12) & 0xFFF);
    a64_add_x_imm(CGR_TMP, CGR_TMP, off & 0xFFF);
    if (cg_is_ptr_type(ty)) { a64_ldr_x_imm(A64_X0, CGR_TMP, 0); return; }
    a64_ldr_w_imm(A64_X0, CGR_TMP, 0);
}

/* Store w0/x0 to [x29 + scaled_offset] with correct width. */
static void cg_store_local(int ty, int offset, int src_xreg) {
    int off;
    off = cg_a64_offset(offset);
    if (off >= 0 && off <= 32760) {
        if (cg_is_ptr_type(ty))                   { a64_str_x_imm(src_xreg, A64_X29, off); return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)  { a64_strb_imm(src_xreg, A64_X29, off);  return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) { a64_strh_imm(src_xreg, A64_X29, off);  return; }
        a64_str_w_imm(src_xreg, A64_X29, off);
        return;
    }
    if (off < 0) {
        int neg;
        neg = -off;
        if (neg <= 4095) {
            a64_sub_x_imm(CGR_TMP2, A64_X29, neg);
        } else {
            a64_sub_x_imm(CGR_TMP2, A64_X29, neg & 0xFFF);
            a64_sub_x_imm_lsl12(CGR_TMP2, CGR_TMP2, (neg >> 12) & 0xFFF);
        }
        if (cg_is_ptr_type(ty))                                { a64_str_x_imm(src_xreg, CGR_TMP2, 0); return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)  { a64_strb_imm(src_xreg, CGR_TMP2, 0); return; }
        if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) { a64_strh_imm(src_xreg, CGR_TMP2, 0); return; }
        a64_str_w_imm(src_xreg, CGR_TMP2, 0);
        return;
    }
    /* Big positive */
    a64_add_x_imm_lsl12(CGR_TMP2, A64_X29, (off >> 12) & 0xFFF);
    a64_add_x_imm(CGR_TMP2, CGR_TMP2, off & 0xFFF);
    if (cg_is_ptr_type(ty)) { a64_str_x_imm(src_xreg, CGR_TMP2, 0); return; }
    a64_str_w_imm(src_xreg, CGR_TMP2, 0);
}

/* Compute address: x0 = x29 + scaled_offset (for &local). */
static void cg_frame_off(int slow32_offset) {
    int off;
    off = cg_a64_offset(slow32_offset);
    if (off >= 0 && off <= 4095)            a64_add_x_imm(A64_X0, A64_X29, off);
    else if (off < 0 && -off <= 4095)       a64_sub_x_imm(A64_X0, A64_X29, -off);
    else {
        /* Larger frames — split into hi/lo. */
        int neg;
        if (off < 0) {
            neg = -off;
            a64_sub_x_imm(A64_X0, A64_X29, neg & 0xFFF);
            a64_sub_x_imm_lsl12(A64_X0, A64_X0, (neg >> 12) & 0xFFF);
        } else {
            a64_add_x_imm(A64_X0, A64_X29, off & 0xFFF);
            a64_add_x_imm_lsl12(A64_X0, A64_X0, (off >> 12) & 0xFFF);
        }
    }
}

/* Move a 32-bit immediate into w0 (sign- and bit-aware). */
static void cg_li(int v) {
    if (v == 0) {
        a64_mov_w(A64_X0, A64_XZR);
        return;
    }
    /* Pick the cheapest encoding among MOVZ/MOVN/MOVZ+MOVK. */
    if ((v & 0xFFFF0000) == 0) {
        /* Fits in 16 bits, low half. */
        a64_movz_w(A64_X0, v & 0xFFFF, 0);
        return;
    }
    if ((v & 0xFFFF) == 0) {
        a64_movz_w(A64_X0, (v >> 16) & 0xFFFF, 16);
        return;
    }
    /* MOVN can encode values where the inverse fits in 16 bits, e.g. -1, -42. */
    {
        int neg;
        neg = ~v;
        if ((neg & 0xFFFF0000) == 0) {
            a64_movn_w(A64_X0, neg & 0xFFFF, 0);
            return;
        }
    }
    /* General case: MOVZ low + MOVK high. */
    a64_movz_w(A64_X0, v & 0xFFFF, 0);
    a64_movk_w(A64_X0, (v >> 16) & 0xFFFF, 16);
}

/* Magic cpatch name for string-literal references (see cg_cpatch_addend). */
#define CG_STR_PSEUDO_NAME "@@str"

/* Append a cpatch entry. Use this everywhere — never write to the arrays
 * directly — so we never forget to populate a parallel field. */
static void cg_cpatch_add(char *name, int off, int kind, int addend) {
    cg_cpatch_name[cg_ncpatches]   = name;
    cg_cpatch_off[cg_ncpatches]    = off;
    cg_cpatch_kind[cg_ncpatches]   = kind;
    cg_cpatch_addend[cg_ncpatches] = addend;
    cg_ncpatches = cg_ncpatches + 1;
}

/* Record an ADRP+ADD pair as two cpatches.  The pair shares the same name,
 * kind sequence (ADR_HI21 then ADD_LO12), and addend.  When the writer
 * emits relocations, it dispatches on kind for the imm field, and on
 * the name (whether it's "@@str" or a real symbol) for the target. */
static void cg_emit_adrp_add_to(int dst_reg, char *sym_name, int addend) {
    int adrp_off; int add_off;

    adrp_off = a64_off;
    a64_adrp(dst_reg, 0);
    cg_cpatch_add(sym_name, adrp_off, A64K_ADR_HI21, addend);

    add_off = a64_off;
    a64_add_x_imm(dst_reg, dst_reg, 0);
    cg_cpatch_add(sym_name, add_off, A64K_ADD_LO12, addend);
}

static void cg_emit_adrp_add(char *sym_name, int addend) {
    cg_emit_adrp_add_to(A64_X0, sym_name, addend);
}

/* Load address of a string literal or global into x0 via ADRP+ADD. */
static void cg_data_addr(int kind, int idx) {
    if (kind == DRELOC_STRING) {
        /* String literal — addend will be resolved as cg_str_rodata_off[idx]. */
        cg_emit_adrp_add(CG_STR_PSEUDO_NAME, idx);
    } else {
        cg_emit_adrp_add(cg_glob_name[idx], 0);
    }
}

static void cg_emit_a64_dbt_trampoline(void) {
    a64_stp_x_pre(A64_X29, A64_X30, A64_SP, -16);
    a64_stp_x_pre(A64_X27, A64_X28, A64_SP, -16);
    a64_stp_x_pre(A64_X25, A64_X26, A64_SP, -16);
    a64_stp_x_pre(A64_X23, A64_X24, A64_SP, -16);
    a64_stp_x_pre(A64_X21, A64_X22, A64_SP, -16);
    a64_stp_x_pre(A64_X19, A64_X20, A64_SP, -16);

    a64_mov_x(A64_X20, A64_X0);
    a64_mov_x(A64_X21, A64_X1);
    a64_mov_x(A64_X22, A64_X3);

    a64_blr(A64_X2);

    a64_ldp_x_post(A64_X19, A64_X20, A64_SP, 16);
    a64_ldp_x_post(A64_X21, A64_X22, A64_SP, 16);
    a64_ldp_x_post(A64_X23, A64_X24, A64_SP, 16);
    a64_ldp_x_post(A64_X25, A64_X26, A64_SP, 16);
    a64_ldp_x_post(A64_X27, A64_X28, A64_SP, 16);
    a64_ldp_x_post(A64_X29, A64_X30, A64_SP, 16);
}

/* Load value at [x0] into w0 / x0 with correct width for type. */
static void cg_load(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) a64_ldrb_imm(A64_X0, A64_X0, 0);
        else                  a64_ldrsb_w_imm(A64_X0, A64_X0, 0);
        return;
    }
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) a64_ldrh_imm(A64_X0, A64_X0, 0);
        else                  a64_ldrsh_w_imm(A64_X0, A64_X0, 0);
        return;
    }
    if (cg_is_ptr_type(ty)) { a64_ldr_x_imm(A64_X0, A64_X0, 0); return; }
    a64_ldr_w_imm(A64_X0, A64_X0, 0);
}

/* Store x1/w1 (LHS) to [x0] with correct width. */
static void cg_store(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        a64_strb_imm(A64_X1, A64_X0, 0);
        return;
    }
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        a64_strh_imm(A64_X1, A64_X0, 0);
        return;
    }
    if (cg_is_ptr_type(ty)) { a64_str_x_imm(A64_X1, A64_X0, 0); return; }
    a64_str_w_imm(A64_X1, A64_X0, 0);
}

/* ============================================================================
 * Expression generation — result in w0/x0
 * ============================================================================ */

static void gen_expr(Node *n);
static void gen_stmt(Node *n);

static void gen_addr(Node *n) {
    if (n->kind == ND_VAR) {
        if (n->is_local) {
            cg_frame_off(n->offset);
        } else {
            int gi;
            gi = 0;
            while (gi < cg_nglobals) {
                if (cg_strcmp(cg_glob_name[gi], n->name) == 0) break;
                gi = gi + 1;
            }
            cg_data_addr(DRELOC_GLOBAL, gi);
        }
        return;
    }
    if (n->kind == ND_UNARY && n->op == TK_STAR) {
        gen_expr(n->lhs);
        return;
    }
    if (n->kind == ND_MEMBER) {
        gen_addr(n->lhs);
        if (n->val != 0) a64_add_x_imm(A64_X0, A64_X0, n->val);
        return;
    }
    p_error("not an lvalue");
}

/* Apply a sign/zero-extend (or no-op) on x0 to widen 32-bit -> 64-bit. */
static void cg_widen_to_64(int ty) {
    if (ty & TY_UNSIGNED) {
        /* Zero-extend low 32 bits via mov w0, w0 (writes clear upper). */
        a64_mov_w(A64_X0, A64_X0);
    } else {
        a64_sxtw(A64_X0, A64_X0);
    }
}

static void cg_widen_to_64_reg(int xreg, int ty) {
    if (ty & TY_UNSIGNED) {
        a64_mov_w(xreg, xreg);
    } else {
        a64_sxtw(xreg, xreg);
    }
}

static void gen_expr(Node *n) {
    int l1; int l2;
    int wide;
    int elem_sz;
    int i;
    Node *a;

    if (!n) { cg_li(0); return; }
    wide = ty_is_llong(n->ty);

    if (n->kind == ND_NUM) {
        if (wide) {
            /* 64-bit literal: split into lo/hi halfwords for MOVZ+MOVK seq. */
            int lo; int hi;
            lo = n->val;
            hi = n->val_hi;
            if (lo == 0 && hi == 0) {
                a64_mov_x(A64_X0, A64_XZR);
            } else {
                a64_mov_x_imm64(A64_X0, lo, hi);
            }
        } else {
            cg_li(n->val);
        }
        return;
    }

    if (n->kind == ND_STRING) {
        cg_data_addr(DRELOC_STRING, n->val);
        return;
    }

    if (n->kind == ND_ASM) {
        if (n->val == ASM_A64_MRS_CNTVCT) {
            a64_mrs_cntvct_el0(A64_X0);
            if (n->lhs) {
                cg_push();
                gen_addr(n->lhs);
                cg_pop();
                cg_store(n->lhs->ty);
                if (cg_is_64bit(n->lhs->ty)) a64_mov_x(A64_X0, A64_X1);
                else                         a64_mov_w(A64_X0, A64_X1);
            }
            return;
        }
        if (n->val == ASM_A64_DBT_TRAMPOLINE) {
            int nargs;
            int arg_reg[4];
            Node *arg;
            arg_reg[0] = A64_X0;
            arg_reg[1] = A64_X1;
            arg_reg[2] = A64_X2;
            arg_reg[3] = A64_X3;
            nargs = 0;
            arg = n->args;
            while (arg) {
                gen_expr(arg);
                cg_push();
                nargs = nargs + 1;
                arg = arg->next;
            }
            i = 0;
            while (i < nargs && i < 4) {
                int off;
                off = (nargs - 1 - i) * 16;
                a64_ldr_x_imm(arg_reg[i], A64_SP, off);
                i = i + 1;
            }
            if (nargs > 0) a64_add_x_imm(A64_SP, A64_SP, nargs * 16);
            cg_emit_a64_dbt_trampoline();
            return;
        }
        p_error("unsupported inline asm");
        return;
    }

    if (n->kind == ND_VAR) {
        if (n->is_array || ty_is_struct(n->ty)) { gen_addr(n); return; }
        if (n->is_local) {
            cg_load_local(n->ty, n->offset);
        } else {
            gen_addr(n);
            cg_load(n->ty);
        }
        return;
    }

    if (n->kind == ND_ASSIGN) {
        gen_expr(n->rhs);
        cg_push();
        gen_addr(n->lhs);
        cg_pop();              /* x1 = value, x0 = addr */
        cg_store(n->ty);
        /* Result = the stored value (move x1 -> x0). */
        if (cg_is_64bit(n->ty)) a64_mov_x(A64_X0, A64_X1);
        else                    a64_mov_w(A64_X0, A64_X1);
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS) {
            gen_expr(n->lhs);
            if (wide) a64_sub_x(A64_X0, A64_XZR, A64_X0);
            else      a64_neg_w(A64_X0, A64_X0);
            return;
        }
        if (n->op == TK_BANG) {
            gen_expr(n->lhs);
            if (cg_is_64bit(n->lhs->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                         a64_cmp_w_imm(A64_X0, 0);
            a64_cset_w(A64_X0, A64_COND_EQ);
            return;
        }
        if (n->op == TK_STAR) {
            gen_expr(n->lhs);
            if (!ty_is_struct(n->ty)) cg_load(n->ty);
            return;
        }
        if (n->op == TK_AMP) {
            gen_addr(n->lhs);
            return;
        }
        if (n->op == TK_TILDE) {
            gen_expr(n->lhs);
            if (wide) {
                /* MVN Xd, Xm  -> ORN Xd, XZR, Xm */
                /* We have a64_mvn_w only — for X form we use eor with -1 imm
                 * encoded as eor x0, x0, #0xFFFFFFFFFFFFFFFF (logical-imm).
                 * Simpler: emit raw word.  ORN (no encoder yet) opcode:
                 *   1 01 01010 00 1 Rm 000000 11111 Rd
                 * = 0xAA200000 | (rm<<16) | (0x1F<<5) | rd     (Rn = XZR)
                 */
                a64_inst(0xAA2003E0 | ((A64_X0 & 0x1F) << 16) | (A64_X0 & 0x1F));
            } else {
                a64_mvn_w(A64_X0, A64_X0);
            }
            return;
        }
        p_error("unknown unary op");
        return;
    }

    if (n->kind == ND_BINOP) {
        /* Short-circuit && */
        if (n->op == TK_LAND) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            if (cg_is_64bit(n->lhs->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                         a64_cmp_w_imm(A64_X0, 0);
            cg_jcc_label(A64_COND_EQ, l1);
            gen_expr(n->rhs);
            if (cg_is_64bit(n->rhs->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                         a64_cmp_w_imm(A64_X0, 0);
            a64_cset_w(A64_X0, A64_COND_NE);
            cg_jmp_label(l2);
            cg_ldef(l1);
            a64_mov_w(A64_X0, A64_XZR);
            cg_ldef(l2);
            return;
        }
        /* Short-circuit || */
        if (n->op == TK_LOR) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            if (cg_is_64bit(n->lhs->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                         a64_cmp_w_imm(A64_X0, 0);
            cg_jcc_label(A64_COND_NE, l1);
            gen_expr(n->rhs);
            if (cg_is_64bit(n->rhs->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                         a64_cmp_w_imm(A64_X0, 0);
            a64_cset_w(A64_X0, A64_COND_NE);
            cg_jmp_label(l2);
            cg_ldef(l1);
            a64_movz_w(A64_X0, 1, 0);
            cg_ldef(l2);
            return;
        }

        /* Pointer arithmetic */
        if (n->op == TK_PLUS || n->op == TK_MINUS) {
            if (ty_is_ptr(n->lhs->ty)) {
                elem_sz = ty_size(ty_deref(n->lhs->ty));
                gen_expr(n->lhs);
                cg_push();
                gen_expr(n->rhs);
                if (elem_sz > 1) {
                    /* x0 *= elem_sz.  Use shifts where possible, else MUL. */
                    if (elem_sz == 2)         a64_lsl_x_imm(A64_X0, A64_X0, 1);
                    else if (elem_sz == 4)    a64_lsl_x_imm(A64_X0, A64_X0, 2);
                    else if (elem_sz == 8)    a64_lsl_x_imm(A64_X0, A64_X0, 3);
                    else if (elem_sz == 16)   a64_lsl_x_imm(A64_X0, A64_X0, 4);
                    else {
                        /* General: x9 = elem_sz; x0 = x0 * x9 */
                        cg_li(elem_sz);
                        /* but cg_li wrote to x0; we need to swap.  Emit imm into x9 directly. */
                        /* Instead: we already have value in x0; load elem_sz into x9 */
                        a64_movz_x(CGR_TMP, elem_sz & 0xFFFF, 0);
                        if ((elem_sz >> 16) & 0xFFFF) a64_movk_x(CGR_TMP, (elem_sz >> 16) & 0xFFFF, 16);
                        a64_mul_x(A64_X0, A64_X0, CGR_TMP);
                    }
                }
                /* Sign-extend the (signed) 32-bit offset to 64 bits. */
                a64_sxtw(A64_X0, A64_X0);
                cg_pop();              /* x1 = pointer */
                if (n->op == TK_PLUS)  a64_add_x(A64_X0, A64_X1, A64_X0);
                else                   a64_sub_x(A64_X0, A64_X1, A64_X0);
                return;
            }
        }

        /* Generic binary: lhs in x1, rhs in x0. */
        gen_expr(n->lhs);
        cg_push();
        gen_expr(n->rhs);
        cg_pop();              /* x1 = lhs, x0 = rhs */

        /* Mixed-width promotion */
        {
            int lhs64; int rhs64;
            lhs64 = cg_is_64bit(n->lhs->ty);
            rhs64 = cg_is_64bit(n->rhs->ty);
            if (lhs64 && !rhs64) cg_widen_to_64(n->rhs->ty);
            if (rhs64 && !lhs64) cg_widen_to_64_reg(A64_X1, n->lhs->ty);
        }

        if (n->op == TK_PLUS) {
            if (wide) a64_add_x(A64_X0, A64_X1, A64_X0);
            else      a64_add_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_MINUS) {
            if (wide) a64_sub_x(A64_X0, A64_X1, A64_X0);
            else      a64_sub_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_STAR) {
            if (wide) a64_mul_x(A64_X0, A64_X1, A64_X0);
            else      a64_mul_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_SLASH) {
            if (wide) {
                if (n->ty & TY_UNSIGNED) a64_udiv_x(A64_X0, A64_X1, A64_X0);
                else                     a64_sdiv_x(A64_X0, A64_X1, A64_X0);
            } else {
                if (n->ty & TY_UNSIGNED) a64_udiv_w(A64_X0, A64_X1, A64_X0);
                else                     a64_sdiv_w(A64_X0, A64_X1, A64_X0);
            }
            return;
        }
        if (n->op == TK_PERCENT) {
            /* rem = lhs - (lhs/rhs)*rhs.  Compute quot in x9; MSUB into x0. */
            if (wide) {
                if (n->ty & TY_UNSIGNED) a64_udiv_x(CGR_TMP, A64_X1, A64_X0);
                else                     a64_sdiv_x(CGR_TMP, A64_X1, A64_X0);
                /* x0 = x1 - x9*x0  -> need MSUB X form; we have only W.
                 * Emit: MSUB Xd, Xn, Xm, Xa  = 1 00 11011 000 Rm 1 Ra Rn Rd */
                a64_inst(0x9B008000
                         | ((A64_X0 & 0x1F) << 16)     /* Rm = x0 (rhs) */
                         | ((A64_X1 & 0x1F) << 10)     /* Ra = x1 (lhs) */
                         | ((CGR_TMP & 0x1F) << 5)     /* Rn = x9 (quot) */
                         |  (A64_X0 & 0x1F));          /* Rd = x0 (rem) */
            } else {
                if (n->ty & TY_UNSIGNED) a64_udiv_w(CGR_TMP, A64_X1, A64_X0);
                else                     a64_sdiv_w(CGR_TMP, A64_X1, A64_X0);
                a64_msub_w(A64_X0, CGR_TMP, A64_X0, A64_X1);
            }
            return;
        }

        /* Comparisons.  Use 64-bit compare if EITHER operand is 64-bit. */
        {
            int cmp64;
            cmp64 = cg_is_64bit(n->lhs->ty) || cg_is_64bit(n->rhs->ty);
            if (n->op == TK_EQ || n->op == TK_NE
             || n->op == TK_LT || n->op == TK_GT
             || n->op == TK_LE || n->op == TK_GE) {
                if (cmp64) a64_cmp_x(A64_X1, A64_X0);
                else       a64_cmp_w(A64_X1, A64_X0);
                if (n->op == TK_EQ) { a64_cset_w(A64_X0, A64_COND_EQ); return; }
                if (n->op == TK_NE) { a64_cset_w(A64_X0, A64_COND_NE); return; }
                if (n->op == TK_LT) {
                    if (n->ty & TY_UNSIGNED) a64_cset_w(A64_X0, A64_COND_LO);
                    else                     a64_cset_w(A64_X0, A64_COND_LT);
                    return;
                }
                if (n->op == TK_GT) {
                    if (n->ty & TY_UNSIGNED) a64_cset_w(A64_X0, A64_COND_HI);
                    else                     a64_cset_w(A64_X0, A64_COND_GT);
                    return;
                }
                if (n->op == TK_LE) {
                    if (n->ty & TY_UNSIGNED) a64_cset_w(A64_X0, A64_COND_LS);
                    else                     a64_cset_w(A64_X0, A64_COND_LE);
                    return;
                }
                if (n->op == TK_GE) {
                    if (n->ty & TY_UNSIGNED) a64_cset_w(A64_X0, A64_COND_HS);
                    else                     a64_cset_w(A64_X0, A64_COND_GE);
                    return;
                }
            }
        }

        if (n->op == TK_AMP) {
            if (wide) a64_and_x(A64_X0, A64_X1, A64_X0);
            else      a64_and_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_PIPE) {
            if (wide) a64_orr_x(A64_X0, A64_X1, A64_X0);
            else      a64_orr_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_CARET) {
            if (wide) a64_eor_x(A64_X0, A64_X1, A64_X0);
            else      a64_eor_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_LSHIFT) {
            if (wide) a64_lslv_x(A64_X0, A64_X1, A64_X0);
            else      a64_lslv_w(A64_X0, A64_X1, A64_X0);
            return;
        }
        if (n->op == TK_RSHIFT) {
            if (wide) {
                if (n->ty & TY_UNSIGNED) a64_lsrv_x(A64_X0, A64_X1, A64_X0);
                else                     a64_asrv_x(A64_X0, A64_X1, A64_X0);
            } else {
                if (n->ty & TY_UNSIGNED) a64_lsrv_w(A64_X0, A64_X1, A64_X0);
                else                     a64_asrv_w(A64_X0, A64_X1, A64_X0);
            }
            return;
        }

        p_error("unknown binop");
        return;
    }

    if (n->kind == ND_TERNARY) {
        l1 = cg_label();
        l2 = cg_label();
        gen_expr(n->cond);
        if (n->cond && cg_is_64bit(n->cond->ty)) a64_cmp_x_imm(A64_X0, 0);
        else                                     a64_cmp_w_imm(A64_X0, 0);
        cg_jcc_label(A64_COND_EQ, l1);
        gen_expr(n->lhs);
        cg_jmp_label(l2);
        cg_ldef(l1);
        gen_expr(n->rhs);
        cg_ldef(l2);
        return;
    }

    if (n->kind == ND_CAST) {
        int dst_base; int src_base;
        gen_expr(n->lhs);
        dst_base = n->ty & TY_BASE_MASK;
        src_base = n->lhs->ty & TY_BASE_MASK;
        (void)src_base;

        if (!ty_is_ptr(n->ty) && dst_base == TY_CHAR) {
            if (n->ty & TY_UNSIGNED) a64_uxtb_w(A64_X0, A64_X0);
            else                     a64_sxtb_w(A64_X0, A64_X0);
            return;
        }
        if (!ty_is_ptr(n->ty) && dst_base == TY_SHORT) {
            if (n->ty & TY_UNSIGNED) a64_uxth_w(A64_X0, A64_X0);
            else                     a64_sxth_w(A64_X0, A64_X0);
            return;
        }
        if (ty_is_llong(n->ty) && !ty_is_llong(n->lhs->ty) && !ty_is_ptr(n->lhs->ty)) {
            if (n->lhs->ty & TY_UNSIGNED) a64_mov_w(A64_X0, A64_X0);  /* zero-ext */
            else                          a64_sxtw(A64_X0, A64_X0);
            return;
        }
        if (!ty_is_ptr(n->ty) && dst_base == TY_INT && ty_is_llong(n->lhs->ty)) {
            /* Truncate 64->32: low bits already in w0.  Force zero upper. */
            a64_mov_w(A64_X0, A64_X0);
            return;
        }
        if (ty_is_ptr(n->ty) && !ty_is_ptr(n->lhs->ty) && !ty_is_llong(n->lhs->ty)) {
            if (n->lhs->ty & TY_UNSIGNED) a64_mov_w(A64_X0, A64_X0);
            else                          a64_sxtw(A64_X0, A64_X0);
            return;
        }
        return;
    }

    if (n->kind == ND_COMMA) {
        gen_expr(n->lhs);
        gen_expr(n->rhs);
        return;
    }

    if (n->kind == ND_MEMBER) {
        gen_addr(n);
        if (!ty_is_struct(n->ty) && !n->is_array) cg_load(n->ty);
        return;
    }

    if (n->kind == ND_FUNC_REF) {
        /* Take address of a function: ADRP+ADD against its symbol. */
        cg_emit_adrp_add(n->name, 0);
        return;
    }

    /* __syscall(nr, a0..a5)  — built-in.  AArch64 ABI: x8=nr, x0..x5=args, svc #0. */
    if (n->kind == ND_CALL && n->name && cg_strcmp(n->name, "__syscall") == 0) {
        Node *arg;
        int nargs;
        int sc_regs[7];

        sc_regs[0] = A64_X8;    /* nr */
        sc_regs[1] = A64_X0;
        sc_regs[2] = A64_X1;
        sc_regs[3] = A64_X2;
        sc_regs[4] = A64_X3;
        sc_regs[5] = A64_X4;
        sc_regs[6] = A64_X5;

        /* Evaluate args left-to-right, each pushed onto our 16-byte slots. */
        nargs = 0;
        arg = n->args;
        while (arg) {
            gen_expr(arg);
            cg_push();
            nargs = nargs + 1;
            arg = arg->next;
        }

        /* Pop into the right syscall registers (peek via positive offsets). */
        i = 0;
        while (i < nargs && i < 7) {
            int off;
            off = (nargs - 1 - i) * 16;
            a64_ldr_x_imm(sc_regs[i], A64_SP, off);
            i = i + 1;
        }
        if (nargs > 0) a64_add_x_imm(A64_SP, A64_SP, nargs * 16);

        a64_svc(0);
        /* Result already in x0. */
        return;
    }

    if (n->kind == ND_CALL || n->kind == ND_CALL_PTR) {
        int nargs; int nreg; int nstack; int outgoing_size;
        int arg_reg[8];
        Node *arg;

        arg_reg[0] = A64_X0;
        arg_reg[1] = A64_X1;
        arg_reg[2] = A64_X2;
        arg_reg[3] = A64_X3;
        arg_reg[4] = A64_X4;
        arg_reg[5] = A64_X5;
        arg_reg[6] = A64_X6;
        arg_reg[7] = A64_X7;

        nargs = 0;
        arg = n->args;
        while (arg) { nargs = nargs + 1; arg = arg->next; }
        nreg = nargs < 8 ? nargs : 8;
        nstack = nargs > 8 ? nargs - 8 : 0;
        outgoing_size = nstack * 8;
        /* Round up to multiple of 16 for SP alignment at BL. */
        if (outgoing_size & 15) outgoing_size = (outgoing_size + 15) & ~15;

        /* Evaluate ALL args left-to-right, push to our 16-byte slots. */
        arg = n->args;
        while (arg) {
            gen_expr(arg);
            cg_push();
            arg = arg->next;
        }

        /* Indirect call: evaluate callee into x16 (a temp register that
         * the caller doesn't need to preserve across the call). */
        if (n->kind == ND_CALL_PTR) {
            gen_expr(n->lhs);
            a64_mov_x(A64_X16, A64_X0);
        }

        /* Load register args from their stack slots.
         * Stack layout (after pushes):
         *   [SP + (nargs-1-i)*16] holds arg i  (arg 0 deepest, arg N-1 on top). */
        i = 0;
        while (i < nreg) {
            int off;
            off = (nargs - 1 - i) * 16;
            a64_ldr_x_imm(arg_reg[i], A64_SP, off);
            i = i + 1;
        }

        /* For 9+ args, copy them into a fresh outgoing area below the temp
         * push area.  AArch64 expects each stack arg as 8 bytes on the
         * caller's stack at SP, SP+8, SP+16, ... at the moment of BL. */
        if (nstack > 0) {
            a64_sub_x_imm(A64_SP, A64_SP, outgoing_size);
            i = 0;
            while (i < nstack) {
                int src_off; int dst_off;
                /* Source arg i+8 is at the (nargs-1-(i+8))*16 in the temp
                 * region, which is now at offset outgoing_size + that. */
                src_off = outgoing_size + (nargs - 1 - (i + 8)) * 16;
                dst_off = i * 8;
                a64_ldr_x_imm(CGR_TMP, A64_SP, src_off);
                a64_str_x_imm(CGR_TMP, A64_SP, dst_off);
                i = i + 1;
            }
        }

        if (n->kind == ND_CALL_PTR) {
            a64_blr(A64_X16);
        } else {
            cg_cpatch_add(n->name, a64_off, A64K_CALL26, 0);
            a64_bl(0);
        }

        /* Tear down outgoing stack area + temp push area. */
        if (nargs > 0 || outgoing_size > 0) {
            a64_add_x_imm(A64_SP, A64_SP, nargs * 16 + outgoing_size);
        }
        /* Result already in x0/w0 by AAPCS64. */
        return;
    }

    /* Compound assignment, post-inc/dec, va_start, va_arg — TODO.
     * For initial bring-up, treat unsupported nodes as zero so we get
     * deterministic output. */

    if (n->kind == ND_COMP_ASSIGN || n->kind == ND_POST_INC
     || n->kind == ND_POST_DEC   || n->kind == ND_VA_START
     || n->kind == ND_VA_ARG) {
        /* Not yet implemented — emit zero so the caller still has a
         * usable value and we can bring up other features first. */
        cg_li(0);
        return;
    }

    /* Fallback */
    (void)a;
    cg_li(0);
}

/* ============================================================================
 * Statement generation
 * ============================================================================ */

static void gen_stmt(Node *n) {
    int l1; int l2; int l3;
    int saved_depth;
    int i; int ncase;
    Node *c;

    if (!n) return;

    if (n->kind == ND_EXPR_STMT) { gen_expr(n->lhs); return; }

    if (n->kind == ND_RETURN) {
        if (n->lhs) gen_expr(n->lhs);
        cg_jmp_label(cg_epilog);
        return;
    }

    if (n->kind == ND_IF) {
        l1 = cg_label();
        gen_expr(n->cond);
        if (n->cond && cg_is_64bit(n->cond->ty)) a64_cmp_x_imm(A64_X0, 0);
        else                                     a64_cmp_w_imm(A64_X0, 0);
        cg_jcc_label(A64_COND_EQ, l1);
        gen_stmt(n->body);
        if (n->els) {
            l2 = cg_label();
            cg_jmp_label(l2);
            cg_ldef(l1);
            gen_stmt(n->els);
            cg_ldef(l2);
        } else {
            cg_ldef(l1);
        }
        return;
    }

    if (n->kind == ND_WHILE) {
        l1 = cg_label();
        l2 = cg_label();
        cg_break_lbl[cg_loop_depth] = l2;
        cg_cont_lbl[cg_loop_depth]  = l1;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_expr(n->cond);
        if (n->cond && cg_is_64bit(n->cond->ty)) a64_cmp_x_imm(A64_X0, 0);
        else                                     a64_cmp_w_imm(A64_X0, 0);
        cg_jcc_label(A64_COND_EQ, l2);
        gen_stmt(n->body);
        cg_jmp_label(l1);
        cg_ldef(l2);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_DO_WHILE) {
        l1 = cg_label();
        l2 = cg_label();
        l3 = cg_label();
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth]  = l2;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_stmt(n->body);
        cg_ldef(l2);
        gen_expr(n->cond);
        if (n->cond && cg_is_64bit(n->cond->ty)) a64_cmp_x_imm(A64_X0, 0);
        else                                     a64_cmp_w_imm(A64_X0, 0);
        cg_jcc_label(A64_COND_NE, l1);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_FOR) {
        l1 = cg_label();
        l2 = cg_label();
        l3 = cg_label();
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth]  = l2;
        cg_loop_depth = cg_loop_depth + 1;
        if (n->init) gen_expr(n->init);
        cg_ldef(l1);
        if (n->cond) {
            gen_expr(n->cond);
            if (cg_is_64bit(n->cond->ty)) a64_cmp_x_imm(A64_X0, 0);
            else                          a64_cmp_w_imm(A64_X0, 0);
            cg_jcc_label(A64_COND_EQ, l3);
        }
        gen_stmt(n->body);
        cg_ldef(l2);
        if (n->step) gen_expr(n->step);
        cg_jmp_label(l1);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_SWITCH) {
        l1 = cg_label();
        cg_break_lbl[cg_loop_depth] = l1;
        cg_cont_lbl[cg_loop_depth]  = -1;
        cg_loop_depth = cg_loop_depth + 1;

        saved_depth = cg_sw_depth;
        cg_sw_base[cg_sw_depth] = 0;
        if (cg_sw_depth > 0)
            cg_sw_base[cg_sw_depth] = cg_sw_base[cg_sw_depth - 1] + cg_sw_count[cg_sw_depth - 1];
        cg_sw_count[cg_sw_depth] = 0;
        cg_sw_def[cg_sw_depth]   = -1;
        cg_sw_depth = cg_sw_depth + 1;

        c = n->body;
        if (c && c->kind == ND_BLOCK) c = c->body;
        while (c) {
            if (c->kind == ND_CASE) {
                i = cg_sw_base[saved_depth] + cg_sw_count[saved_depth];
                cg_sw_val[i] = c->val;
                cg_sw_lbl[i] = cg_label();
                cg_sw_count[saved_depth] = cg_sw_count[saved_depth] + 1;
            } else if (c->kind == ND_DEFAULT) {
                cg_sw_def[saved_depth] = cg_label();
            }
            c = c->next;
        }

        gen_expr(n->cond);
        a64_mov_w(CGR_TMP, A64_X0);     /* save switch value in w9 */

        ncase = cg_sw_count[saved_depth];
        i = 0;
        while (i < ncase) {
            int ci; int cv;
            ci = cg_sw_base[saved_depth] + i;
            cv = cg_sw_val[ci];
            /* CMP w9, #imm if it fits (0..4095); else materialize and CMP. */
            if (cv >= 0 && cv <= 4095) {
                a64_cmp_w_imm(CGR_TMP, cv);
            } else {
                cg_li(cv);          /* into w0 */
                a64_cmp_w(CGR_TMP, A64_X0);
            }
            cg_jcc_label(A64_COND_EQ, cg_sw_lbl[ci]);
            i = i + 1;
        }

        if (cg_sw_def[saved_depth] >= 0) cg_jmp_label(cg_sw_def[saved_depth]);
        else                             cg_jmp_label(l1);

        cg_sw_cur[saved_depth] = 0;
        gen_stmt(n->body);

        cg_ldef(l1);
        cg_loop_depth = cg_loop_depth - 1;
        cg_sw_depth   = cg_sw_depth - 1;
        return;
    }

    if (n->kind == ND_CASE) {
        i = cg_sw_base[cg_sw_depth - 1] + cg_sw_cur[cg_sw_depth - 1];
        cg_ldef(cg_sw_lbl[i]);
        cg_sw_cur[cg_sw_depth - 1] = cg_sw_cur[cg_sw_depth - 1] + 1;
        return;
    }

    if (n->kind == ND_DEFAULT) {
        cg_ldef(cg_sw_def[cg_sw_depth - 1]);
        return;
    }

    if (n->kind == ND_BREAK)    { cg_jmp_label(cg_break_lbl[cg_loop_depth - 1]); return; }
    if (n->kind == ND_CONTINUE) { cg_jmp_label(cg_cont_lbl[cg_loop_depth - 1]);  return; }
    if (n->kind == ND_GOTO)     { cg_jmp_label(n->val); return; }

    if (n->kind == ND_LABEL) {
        cg_ldef(n->val);
        if (n->body) gen_stmt(n->body);
        return;
    }

    if (n->kind == ND_BLOCK) {
        c = n->body;
        while (c) { gen_stmt(c); c = c->next; }
        return;
    }

    /* Fallback: treat as expression. */
    gen_expr(n);
}

/* ============================================================================
 * Function generation
 * ============================================================================ */

static int a64_arg_regs[8];

static void gen_func(Node *fn) {
    int fs;
    int i;
    Node *p;

    a64_arg_regs[0] = A64_X0;
    a64_arg_regs[1] = A64_X1;
    a64_arg_regs[2] = A64_X2;
    a64_arg_regs[3] = A64_X3;
    a64_arg_regs[4] = A64_X4;
    a64_arg_regs[5] = A64_X5;
    a64_arg_regs[6] = A64_X6;
    a64_arg_regs[7] = A64_X7;

    cg_func_name[cg_nfuncs] = fn->name;
    cg_func_off[cg_nfuncs]  = a64_off;
    cg_nfuncs = cg_nfuncs + 1;

    cg_epilog     = cg_label();
    cg_loop_depth = 0;
    cg_sw_depth   = 0;
    cg_is_varargs = fn->is_varargs;
    cg_nparams    = fn->nparams;

    /* Frame size: 16 bytes for saved FP/LR + 2x locals_size (slot doubling).
     * Round up to 16-byte alignment. */
    fs = 16 + fn->locals_size * 2;
    if (cg_is_varargs) fs = fs + 64;     /* 8 args * 8 bytes save area */
    fs = (fs + 15) & ~15;
    cg_frame_size = fs;
    if (cg_is_varargs) cg_va_save_off = -(fs);
    else               cg_va_save_off = 0;

    /* Prologue:
     *   sub  sp, sp, #fs
     *   stp  x29, x30, [sp, #fs-16]      ; save FP/LR at top of frame
     *   add  x29, sp, #fs-16             ; new FP points at saved-FP slot
     *
     * After this:  [x29] = saved FP, [x29+8] = saved LR.  Locals lie at
     * negative offsets from x29, matching the frontend's -12, -16, ...
     * scheme (after our 2x doubling). */
    if (fs <= 4095) {
        a64_sub_x_imm(A64_SP, A64_SP, fs);
    } else {
        /* Two-step subtract for big frames. */
        a64_sub_x_imm(A64_SP, A64_SP, fs & 0xFFF);
        a64_sub_x_imm_lsl12(A64_SP, A64_SP, (fs >> 12) & 0xFFF);
    }
    a64_stp_x_off(A64_X29, A64_X30, A64_SP, fs - 16);
    if ((fs - 16) <= 4095)
        a64_add_x_imm(A64_X29, A64_SP, fs - 16);
    else {
        a64_add_x_imm(A64_X29, A64_SP, (fs - 16) & 0xFFF);
        a64_add_x_imm_lsl12(A64_X29, A64_X29, ((fs - 16) >> 12) & 0xFFF);
    }

    /* Save register args to their assigned local slots. */
    i = 0;
    p = fn->args;
    while (p && i < 8) {
        cg_store_local(p->ty, p->offset, a64_arg_regs[i]);
        i = i + 1;
        p = p->next;
    }
    /* 9th+ args arrive on caller's stack at [old_SP+0], [old_SP+8], etc.
     * Old SP is at our [x29 + 16] (since [x29] is saved FP and the saved
     * area is at the top of our frame). */
    while (p) {
        a64_ldr_x_imm(CGR_TMP, A64_X29, 16 + (i - 8) * 8);
        cg_store_local(p->ty, p->offset, CGR_TMP);
        i = i + 1;
        p = p->next;
    }

    /* Varargs save area — TODO when we wire up va_start/va_arg. */

    gen_stmt(fn->body);

    /* Epilogue */
    cg_ldef(cg_epilog);
    a64_ldp_x_off(A64_X29, A64_X30, A64_SP, fs - 16);
    if (fs <= 4095) {
        a64_add_x_imm(A64_SP, A64_SP, fs);
    } else {
        a64_add_x_imm(A64_SP, A64_SP, fs & 0xFFF);
        a64_add_x_imm_lsl12(A64_SP, A64_SP, (fs >> 12) & 0xFFF);
    }
    a64_ret();
}

/* ============================================================================
 * Data section building
 * ============================================================================ */

static void gen_data_sections(Node *prog) {
    int i; int j;

    cg_rodata_len = 0;
    cg_data_len   = 0;
    cg_bss_size   = 0;
    (void)prog;

    /* Strings → .rodata (copy from lexer pool, including NUL). */
    i = 0;
    while (i < cg_nstrings) {
        cg_str_rodata_off[i] = cg_rodata_len;
        j = 0;
        while (j <= cg_str_len[i]) {
            if (cg_rodata_len < 65536)
                cg_rodata[cg_rodata_len] = cg_str_pool[cg_str_off[i] + j];
            cg_rodata_len = cg_rodata_len + 1;
            j = j + 1;
        }
        i = i + 1;
    }

    /* Globals → .data (initialized) or .bss (uninitialized). */
    i = 0;
    while (i < cg_nglobals) {
        cg_glob_in_bss[i] = 0;
        if (cg_glob_extern[i]) {
            cg_glob_data_off[i] = 0;
            i = i + 1;
            continue;
        }
        if (cg_glob_has_init[i] == 0) {
            cg_glob_in_bss[i] = 1;
            i = i + 1;
            continue;
        }
        if (cg_glob_size[i] >= 8) {
            while (cg_data_len & 7) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
            }
        } else if (cg_glob_size[i] >= 4) {
            while (cg_data_len & 3) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
            }
        }
        cg_glob_data_off[i] = cg_data_len;
        if (ps_ginit_start[i] >= 0) {
            int off;
            int relj;
            int relbase;
            int relend;
            off = 0;
            while (off < ps_ginit_count[i]) {
                if (cg_data_len < 65536)
                    cg_data[cg_data_len] = ps_ginit_pool[ps_ginit_start[i] + off];
                cg_data_len = cg_data_len + 1;
                off = off + 1;
            }
            relbase = ps_girel_start[i];
            relend = relbase + ps_girel_count[i];
            relj = relbase;
            while (relj < relend) {
                cg_dreloc_off[cg_ndrelocs]  = -(cg_glob_data_off[i] + ps_girel_off[relj] + 1);
                if (ps_girel_kind[relj] == GIRELOC_STRING) {
                    cg_dreloc_kind[cg_ndrelocs] = DRELOC_STRING;
                    cg_dreloc_name[cg_ndrelocs] = NULL;
                } else if (ps_girel_kind[relj] == GIRELOC_SYMBOL) {
                    cg_dreloc_kind[cg_ndrelocs] = DRELOC_SYMBOL;
                    cg_dreloc_name[cg_ndrelocs] = ps_girel_name[relj];
                } else {
                    cg_dreloc_kind[cg_ndrelocs] = DRELOC_GLOBAL;
                    cg_dreloc_name[cg_ndrelocs] = NULL;
                }
                cg_dreloc_idx[cg_ndrelocs]  = ps_girel_idx[relj];
                cg_ndrelocs = cg_ndrelocs + 1;
                relj = relj + 1;
            }
            while (off < cg_glob_size[i]) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
                off = off + 1;
            }
        } else if (cg_glob_has_init[i] == 2) {
            /* String-initialized pointer in .data needs an ABS64 reloc. */
            cg_dreloc_off[cg_ndrelocs]  = -(cg_data_len + 1);
            cg_dreloc_kind[cg_ndrelocs] = DRELOC_STRING;
            cg_dreloc_idx[cg_ndrelocs]  = cg_glob_init[i];
            cg_dreloc_name[cg_ndrelocs] = NULL;
            cg_ndrelocs = cg_ndrelocs + 1;
            j = 0;
            while (j < 8) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
                j = j + 1;
            }
        } else {
            j = 0;
            while (j < cg_glob_size[i]) {
                if (cg_data_len < 65536) {
                    if (j < 4 && cg_glob_has_init[i] == 1)
                        cg_data[cg_data_len] = (cg_glob_init[i] >> (j * 8)) & 0xFF;
                    else
                        cg_data[cg_data_len] = 0;
                }
                cg_data_len = cg_data_len + 1;
                j = j + 1;
            }
        }
        i = i + 1;
    }

    cg_bss_size = 0;
    i = 0;
    while (i < cg_nglobals) {
        if (cg_glob_in_bss[i]) {
            if (cg_glob_size[i] >= 8) while (cg_bss_size & 7) cg_bss_size = cg_bss_size + 1;
            else if (cg_glob_size[i] >= 4) while (cg_bss_size & 3) cg_bss_size = cg_bss_size + 1;
            cg_glob_data_off[i] = cg_bss_size;
            cg_bss_size = cg_bss_size + cg_glob_size[i];
        }
        i = i + 1;
    }
}

/* ============================================================================
 * Resolve relocations (executable mode only — for object mode the
 * obj_writer emits the AArch64 reloc records and the linker takes over).
 * ============================================================================ */

/* Patch an ADRP+ADD pair to point at an absolute target address.
 * site_adrp: offset of the ADRP instruction.
 * site_add:  offset of the ADD instruction.
 * target:    absolute virtual address of the symbol.
 * The instructions live in our text segment at vaddr (text_base + site).
 */
static void cg_resolve_adrp_add(int site_adrp, int site_add, int target,
                                int text_base) {
    int pc_page; int target_page; int page_off;
    int word; int kept;
    int lo12;

    /* ADRP imm21 = (target_page - pc_page) >> 12.
     * Both pages are 4KB-aligned. */
    pc_page     = (text_base + site_adrp) & ~0xFFF;
    target_page = target & ~0xFFF;
    page_off    = (target_page - pc_page) >> 12;

    word = a64_read_inst(site_adrp);
    kept = word & 0x9F00001F;     /* keep top 1 + opc + bottom 5 (Rd) and zero immlo/immhi */
    {
        int immlo; int immhi;
        immlo = page_off & 3;
        immhi = (page_off >> 2) & 0x7FFFF;
        word  = kept | (immlo << 29) | (immhi << 5);
    }
    a64_patch_inst(site_adrp, word);

    /* ADD imm12 = target & 0xFFF. */
    lo12 = target & 0xFFF;
    word = a64_read_inst(site_add);
    kept = word & 0xFFC003FF;     /* zero out imm12 bits 21..10 */
    word = kept | ((lo12 & 0xFFF) << 10);
    a64_patch_inst(site_add, word);
}

static void resolve_relocations(void) {
    int i;
    int text_base; int rodata_base; int data_base;

    text_base = elf_text_vaddr();
    rodata_base = elf_rodata_vaddr();
    data_base   = elf_data_vaddr();

    /* String/global ADRP+ADD pairs come in TWO consecutive cg_dreloc entries
     * (DRELOC_STRING) — one for the ADRP site and one for the ADD site, with
     * the same idx. We patch them together. */
    i = 0;
    while (i < cg_ndrelocs) {
        int kind; int idx; int off; int addr;
        kind = cg_dreloc_kind[i];
        idx  = cg_dreloc_idx[i];
        off  = cg_dreloc_off[i];

        if (kind == DRELOC_STRING) {
            addr = rodata_base + cg_str_rodata_off[idx];
        } else if (kind == DRELOC_SYMBOL) {
            int fi;
            addr = 0;
            fi = 0;
            while (fi < cg_nfuncs) {
                if (cg_strcmp(cg_dreloc_name[i], cg_func_name[fi]) == 0) {
                    addr = text_base + cg_func_off[fi];
                    break;
                }
                fi = fi + 1;
            }
        } else {
            int gidx;
            gidx = idx;
            if (gidx < cg_nglobals && cg_glob_extern[gidx])
                addr = 0;
            else if (gidx < cg_nglobals && cg_glob_in_bss[gidx])
                addr = data_base + cg_data_len + cg_glob_data_off[gidx];
            else
                addr = data_base + cg_glob_data_off[gidx];
        }

        if (off < 0) {
            /* Negative encoding = ABS64 in .data. Patch 8 bytes. */
            int doff;
            doff = -(off + 1);
            cg_data[doff]     = addr & 0xFF;
            cg_data[doff + 1] = (addr >> 8) & 0xFF;
            cg_data[doff + 2] = (addr >> 16) & 0xFF;
            cg_data[doff + 3] = (addr >> 24) & 0xFF;
            cg_data[doff + 4] = 0; cg_data[doff + 5] = 0;
            cg_data[doff + 6] = 0; cg_data[doff + 7] = 0;
            i = i + 1;
            continue;
        }

        /* Code dreloc: this should be an ADRP site immediately followed by
         * an ADD site for the same idx.  Patch the pair. */
        if (i + 1 < cg_ndrelocs
         && cg_dreloc_kind[i + 1] == kind
         && cg_dreloc_idx[i + 1]  == idx) {
            cg_resolve_adrp_add(off, cg_dreloc_off[i + 1], addr,
                                elf_text_vaddr());
            i = i + 2;
        } else {
            /* Lone ADRP without ADD — bug.  Skip with a warning. */
            fdputs("cc-a64: warning: orphan ADRP dreloc at offset ", 2);
            fdputuint(2, off);
            fdputs("\n", 2);
            i = i + 1;
        }
    }
}

/* Resolve all cg_cpatch entries (function references, ADRP+ADD on globals). */
static void resolve_calls(void) {
    int i; int j;
    int text_base; int data_base;

    text_base = elf_text_vaddr();
    data_base = elf_data_vaddr();

    i = 0;
    while (i < cg_ncpatches) {
        int kind; char *name; int off;
        int target_addr; int found;

        kind = cg_cpatch_kind[i];
        name = cg_cpatch_name[i];
        off  = cg_cpatch_off[i];

        target_addr = 0;
        found = 0;

        /* Look up in functions first. */
        j = 0;
        while (j < cg_nfuncs) {
            if (cg_strcmp(name, cg_func_name[j]) == 0) {
                target_addr = text_base + cg_func_off[j];
                found = 1;
                break;
            }
            j = j + 1;
        }
        /* Then in globals. */
        if (!found) {
            j = 0;
            while (j < cg_nglobals) {
                if (cg_strcmp(name, cg_glob_name[j]) == 0) {
                    if (cg_glob_extern[j])
                        target_addr = 0;
                    else if (cg_glob_in_bss[j])
                        target_addr = data_base + cg_data_len + cg_glob_data_off[j];
                    else
                        target_addr = data_base + cg_glob_data_off[j];
                    found = 1;
                    break;
                }
                j = j + 1;
            }
        }

        if (!found) {
            fdputs("cc-a64: error: unresolved symbol '", 2);
            fdputs(name, 2);
            fdputs("'\n", 2);
            exit(1);
        }

        if (kind == A64K_CALL26 || kind == A64K_JUMP26) {
            int word; int kept; int diff; int imm26;
            word = a64_read_inst(off);
            diff = target_addr - (text_base + off);
            imm26 = (diff >> 2) & 0x03FFFFFF;
            kept  = word & 0xFC000000;
            a64_patch_inst(off, kept | imm26);
        } else if (kind == A64K_ADR_HI21) {
            /* Patch ADRP imm21 (page diff). */
            int pc_page; int tgt_page; int page_off;
            int word; int kept; int immlo; int immhi;
            pc_page  = (text_base + off) & ~0xFFF;
            tgt_page = target_addr & ~0xFFF;
            page_off = (tgt_page - pc_page) >> 12;
            word = a64_read_inst(off);
            kept = word & 0x9F00001F;
            immlo = page_off & 3;
            immhi = (page_off >> 2) & 0x7FFFF;
            a64_patch_inst(off, kept | (immlo << 29) | (immhi << 5));
        } else if (kind == A64K_ADD_LO12) {
            int word; int kept; int lo12;
            lo12 = target_addr & 0xFFF;
            word = a64_read_inst(off);
            kept = word & 0xFFC003FF;
            a64_patch_inst(off, kept | ((lo12 & 0xFFF) << 10));
        } else if (kind == A64K_LDST32_LO12 || kind == A64K_LDST64_LO12
                || kind == A64K_LDST16_LO12 || kind == A64K_LDST8_LO12) {
            /* Patch LDR/STR's imm12.  The scaling depends on the load size,
             * which we encode via the kind.  imm12 = (lo12 >> shift). */
            int word; int kept; int lo12; int scaled; int shift;
            if (kind == A64K_LDST64_LO12)      shift = 3;
            else if (kind == A64K_LDST32_LO12) shift = 2;
            else if (kind == A64K_LDST16_LO12) shift = 1;
            else                               shift = 0;
            lo12   = target_addr & 0xFFF;
            scaled = lo12 >> shift;
            word = a64_read_inst(off);
            kept = word & 0xFFC003FF;
            a64_patch_inst(off, kept | ((scaled & 0xFFF) << 10));
        } else {
            fdputs("cc-a64: unknown cpatch kind\n", 2);
            exit(1);
        }
        i = i + 1;
    }
}

/* ============================================================================
 * Globals collection
 * ============================================================================ */

static int cg_a64_global_size(int ty, int s32_size) {
    if (s32_size > 0) {
        if (ty_is_ptr(ty) && ty_is_ptr(ty_deref(ty)))
            return s32_size * 2;
        return s32_size;
    }
    if (ty_is_ptr(ty)) return 8;
    return ty_size(ty);
}

static void collect_globals(Node *prog) {
    int i; int sz;
    (void)prog;

    cg_nglobals = 0;

    i = 0;
    while (i < ps_nglobals) {
        cg_glob_name[cg_nglobals] = ps_gname[i];
        cg_glob_extern[cg_nglobals] = ps_gextern[i];
        if (ps_gsize[i] > 0) sz = ps_gsize[i];
        else                 sz = cg_a64_global_size(ps_gtype[i], 0);
        cg_glob_size[cg_nglobals] = sz;

        if (ps_ginit_start[i] >= 0) {
            cg_glob_has_init[cg_nglobals] = 1;
            cg_glob_init[cg_nglobals]     = 0;
        } else if (ps_gstr[i] >= 0) {
            cg_glob_has_init[cg_nglobals] = 2;
            cg_glob_init[cg_nglobals]     = ps_gstr[i];
        } else if (ps_ginit[i] != 0) {
            cg_glob_has_init[cg_nglobals] = 1;
            cg_glob_init[cg_nglobals]     = ps_ginit[i];
        } else {
            cg_glob_has_init[cg_nglobals] = 0;
            cg_glob_init[cg_nglobals]     = 0;
        }

        cg_nglobals = cg_nglobals + 1;
        i = i + 1;
    }
}

/* ============================================================================
 * Top-level: gen_program
 * ============================================================================ */

static void gen_program(Node *prog) {
    Node *fn;
    int entry_off;
    int i;

    i = 0;
    while (i < CG_MAX_LABELS) { cg_lbl_off[i] = -1; i = i + 1; }
    cg_npatches  = 0;
    cg_nfuncs    = 0;
    cg_ncpatches = 0;
    cg_ndrelocs  = 0;
    cg_nstrings  = 0;
    cg_str_pool_used = 0;
    cg_nglobals  = 0;
    a64_off      = 0;

    collect_globals(prog);

    if (!cg_object_mode) {
        /* Provide a fallback __save_envp (just RET) so executables that don't
         * link in the real libc still link. */
        cg_func_name[cg_nfuncs] = "__save_envp";
        cg_func_off[cg_nfuncs]  = a64_off;
        cg_nfuncs = cg_nfuncs + 1;
        a64_ret();
    }

    entry_off = a64_off;

    if (!cg_object_mode) {
        /* Inline the crt0 from crt0_emit.h.  emit_crt0_to_buf does not record
         * _start as a function (we do that here so the entry-point address is
         * correctly attributed in the symbol table when executable mode is
         * extended later). */
        emit_crt0_to_buf();
    }

    /* Generate user functions */
    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) gen_func(fn);
        fn = fn->next;
    }

    /* Copy the lexer's string pool into our pool so cg_str_off/_len are
     * meaningful for gen_data_sections. */
    i = 0;
    while (i < lex_strpool_len && i < CG_STRING_POOL_SZ) {
        cg_str_pool[i] = lex_strpool[i];
        i = i + 1;
    }
    cg_str_pool_used = lex_strpool_len;
    cg_nstrings = 0;
    i = 0;
    while (i < LEX_POOL_MAX && lex_str_off[i] < lex_strpool_len) {
        cg_str_off[i] = lex_str_off[i];
        cg_str_len[i] = lex_str_len[i];
        cg_nstrings = cg_nstrings + 1;
        i = i + 1;
        if (i >= CG_MAX_STRINGS) break;
    }

    gen_data_sections(prog);

    cg_olen = a64_off;

    if (cg_object_mode) return;

    /* Executable mode: lay out ELF, then patch all relocs. */
    elf_init();
    elf_set_text(a64_buf, a64_off);
    if (cg_rodata_len > 0)                       elf_set_rodata(cg_rodata, cg_rodata_len);
    if (cg_data_len > 0 || cg_bss_size > 0)      elf_set_data(cg_data, cg_data_len);
    if (cg_bss_size > 0)                         elf_set_bss(cg_bss_size);
    elf_set_entry(entry_off);

    resolve_calls();
    resolve_relocations();
}

#endif /* CODEGEN_A64_H */
