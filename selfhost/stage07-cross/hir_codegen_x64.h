/* hir_codegen_x64.h -- Naive HIR to x86-64 code generator
 *
 * Spill-everything approach: every HIR value gets an 8-byte stack slot.
 * Sources are loaded into scratch registers (RAX, RCX, RDX, R10, R11),
 * computation is performed, and results are stored back to their slot.
 *
 * This is intentionally simple -- correctness first.  BURG instruction
 * selection and linear-scan register allocation will be layered on later.
 *
 * Requires: x64_encode.h, codegen_x64.h (for infrastructure arrays),
 *           hir.h, hir_lower.h, hir_ssa.h, hir_opt.h, hir_licm.h
 */

#ifndef HIR_CODEGEN_X64_H
#define HIR_CODEGEN_X64_H

/* ============================================================================
 * Per-function state
 * ============================================================================ */

static int hx_slot[HIR_MAX_INST];    /* RBP offset for each value, 0=none */
static int hx_nslots;                /* number of allocated slots */
static int hx_slot_base;             /* RBP offset where slots start (negative) */
static int hx_frame_size;            /* total frame size */
static int hx_epilog_patch;          /* patch offset for epilog jump */
static int hx_is_varargs;            /* 1 if current function is variadic */
static int hx_fn_nparams;            /* number of named params */

/* Block code offsets for jump patching */
static int hx_blk_off[HIR_MAX_BLOCK];  /* code offset of block start, -1=not yet */

/* Forward jump patches: jump at patch_off targets block patch_blk */
#define HX_MAX_BPATCH 8192
static int hx_bpatch_blk[HX_MAX_BPATCH];
static int hx_bpatch_off[HX_MAX_BPATCH];  /* offset of rel32 in x64_buf */
static int hx_nbpatch;

/* x86-64 SysV ABI argument registers */
static int hx_arg_reg[6];
static int hx_phi_insts[HIR_MAX_INST];
static int hx_phi_vals[HIR_MAX_INST];

/* ============================================================================
 * Helpers
 * ============================================================================ */

/* Is this type 64-bit on x86-64? */
static int hx_is_wide(int ty) {
    return ty_is_ptr(ty) || ty_is_llong(ty);
}

static void hx_die(char *msg, int idx) {
    fdputs("cc-x64 --hir: ", 2);
    fdputs(msg, 2);
    if (idx >= 0) {
        fdputs(" at HIR inst ", 2);
        fdputuint(2, idx);
    }
    fdputs("\n", 2);
    exit(1);
}

/* Find global variable index by name.  Returns -1 if not found. */
static int hx_find_global(char *name) {
    int i;
    i = 0;
    while (i < cg_nglobals) {
        if (cg_strcmp(cg_glob_name[i], name) == 0) return i;
        i = i + 1;
    }
    return -1;
}

/* Allocate a spill slot.  Returns the RBP offset (negative). */
static int hx_alloc_slot(void) {
    int off;
    off = hx_slot_base - (hx_nslots + 1) * 8;
    hx_nslots = hx_nslots + 1;
    return off;
}

/* ============================================================================
 * Value materialization -- load an HIR value into an x64 register
 * ============================================================================ */

static void hx_mat(int inst, int reg) {
    int k;
    int off;
    int gi;

    if (inst < 0) {
        x64_xor_rr(reg, reg);
        return;
    }

    k = h_kind[inst];

    /* Rematerializable: ICONST */
    if (k == HI_ICONST) {
        if (h_val[inst] == 0) {
            x64_xor_rr(reg, reg);
        } else {
            x64_mov_ri(reg, h_val[inst]);
        }
        return;
    }

    /* Rematerializable: ALLOCA -- address of local variable */
    if (k == HI_ALLOCA) {
        x64_lea(reg, X64_RBP, cg_x64_offset(h_val[inst]));
        return;
    }

    /* Rematerializable: GETFP -- frame pointer value */
    if (k == HI_GETFP) {
        x64_mov_rr64(reg, X64_RBP);
        return;
    }

    /* GADDR, SADDR, FADDR -- address constants that need relocations.
     * These are NOT rematerialized in the slot-allocation sense (each
     * occurrence would add a relocation) but we handle them here for
     * the case where the value had its slot optimized away or for
     * direct use.  Normally they go through the spill slot. */
    if (k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) {
        if (k == HI_GADDR) {
            gi = hx_find_global(h_name[inst]);
            if (gi >= 0) {
                /* Emit movabs into RAX (cg_data_addr hardcodes RAX) */
                cg_data_addr(DRELOC_GLOBAL, gi);
            } else {
                /* External -- use call patch as address */
                cg_cpatch_name[cg_ncpatches] = h_name[inst];
                cg_cpatch_off[cg_ncpatches] = -(x64_off + 2 + 1);
                cg_ncpatches = cg_ncpatches + 1;
                x64_mov_ri64(X64_RAX, 0, 0);
            }
        } else if (k == HI_SADDR) {
            cg_data_addr(DRELOC_STRING, h_val[inst]);
        } else {
            /* FADDR -- function address */
            cg_cpatch_name[cg_ncpatches] = h_name[inst];
            cg_cpatch_off[cg_ncpatches] = -(x64_off + 2 + 1);
            cg_ncpatches = cg_ncpatches + 1;
            x64_mov_ri64(X64_RAX, 0, 0);
        }
        if (reg != X64_RAX) x64_mov_rr64(reg, X64_RAX);
        return;
    }

    /* Default: load from spill slot */
    off = hx_slot[inst];
    if (off != 0) {
        x64_mov_rm64(reg, X64_RBP, off);
    } else {
        /* No slot (unused value or error) -- zero */
        x64_xor_rr(reg, reg);
    }
}

/* Store a register to an HIR value's spill slot. */
static void hx_spill(int inst, int reg) {
    int off;
    if (inst < 0) return;
    if (hi_is_remat(h_kind[inst]) &&
        h_kind[inst] != HI_GADDR &&
        h_kind[inst] != HI_SADDR &&
        h_kind[inst] != HI_FADDR) {
        return;
    }
    off = hx_slot[inst];
    if (off == 0) return;  /* no slot allocated */
    x64_mov_mr64(X64_RBP, off, reg);
}

/* ============================================================================
 * Block label management
 * ============================================================================ */

/* Record a forward jump to a block.  The rel32 at patch_off will be
 * resolved when the block's code offset is known. */
static void hx_jump_to_block(int blk) {
    if (hx_blk_off[blk] >= 0) {
        /* Backward: block already emitted, compute displacement */
        x64_jmp_rel32(hx_blk_off[blk] - (x64_off + 5));
    } else {
        /* Forward: emit placeholder, record patch */
        x64_byte(0xE9);
        hx_bpatch_blk[hx_nbpatch] = blk;
        hx_bpatch_off[hx_nbpatch] = x64_off;
        hx_nbpatch = hx_nbpatch + 1;
        x64_dword(0);
    }
}

/* Emit conditional jump (cc) to a block. */
static void hx_jcc_to_block(int cc, int blk) {
    if (hx_blk_off[blk] >= 0) {
        x64_jcc_rel32(cc, hx_blk_off[blk] - (x64_off + 6));
    } else {
        x64_byte(0x0F);
        x64_byte(cc);
        hx_bpatch_blk[hx_nbpatch] = blk;
        hx_bpatch_off[hx_nbpatch] = x64_off;
        hx_nbpatch = hx_nbpatch + 1;
        x64_dword(0);
    }
}

/* Define a block's code offset and resolve pending patches. */
static void hx_define_block(int blk) {
    int i;
    hx_blk_off[blk] = x64_off;
    i = 0;
    while (i < hx_nbpatch) {
        if (hx_bpatch_blk[i] == blk) {
            x64_patch_rel32(hx_bpatch_off[i], x64_off);
            /* Remove by swapping with last */
            hx_nbpatch = hx_nbpatch - 1;
            hx_bpatch_blk[i] = hx_bpatch_blk[hx_nbpatch];
            hx_bpatch_off[i] = hx_bpatch_off[hx_nbpatch];
        } else {
            i = i + 1;
        }
    }
}

/* ============================================================================
 * PHI copy emission
 *
 * Before branching from from_blk to to_blk, resolve all PHI nodes in
 * to_blk.  Since all values are spilled, we use a push/pop strategy
 * to avoid clobbering issues with swaps.
 * ============================================================================ */

static void hx_phi_copies(int from_blk, int to_blk) {
    int i;
    int n;
    int v;

    /* Collect PHI nodes and their source values for this edge */
    n = 0;
    i = ssa_phi_head[to_blk];
    while (i >= 0) {
        if (h_kind[i] == HI_PHI) {
            if (n >= HIR_MAX_INST) hx_die("too many PHI copies on edge", i);
            v = ssa_phi_find_arg(i, from_blk);
            hx_phi_insts[n] = i;
            hx_phi_vals[n] = v;
            n = n + 1;
        }
        i = ssa_phi_next[i];
    }
    if (n == 0) return;

    /* Phase 1: push all source values (left to right) */
    i = 0;
    while (i < n) {
        hx_mat(hx_phi_vals[i], X64_RAX);
        x64_push(X64_RAX);
        i = i + 1;
    }

    /* Phase 2: pop to destinations (right to left, LIFO) */
    i = n - 1;
    while (i >= 0) {
        x64_pop(X64_RAX);
        hx_spill(hx_phi_insts[i], X64_RAX);
        i = i - 1;
    }
}

/* ============================================================================
 * Typed memory access helpers
 * ============================================================================ */

/* Load from [addr_reg + 0] into dst_reg with appropriate width for ty. */
static void hx_load_typed(int dst, int addr, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm8(dst, addr, 0);
        else
            x64_movsx_rm8(dst, addr, 0);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm16(dst, addr, 0);
        else
            x64_movsx_rm16(dst, addr, 0);
    } else if (hx_is_wide(ty)) {
        x64_mov_rm64(dst, addr, 0);
    } else {
        x64_mov_rm(dst, addr, 0);
    }
}

/* Store src_reg to [addr_reg + 0] with appropriate width for ty. */
static void hx_store_typed(int addr, int src, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        x64_mov_mr8(addr, 0, src);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        x64_mov_mr16(addr, 0, src);
    } else if (hx_is_wide(ty)) {
        x64_mov_mr64(addr, 0, src);
    } else {
        x64_mov_mr(addr, 0, src);
    }
}

/* ============================================================================
 * Instruction emission
 * ============================================================================ */

static void hx_emit_inst(int idx) {
    int k;
    int ty;
    int wide;
    int target;
    int fall;
    int nargs;
    int nreg;
    int nstack;
    int outgoing;
    int j;
    int arg_idx;

    k = h_kind[idx];
    ty = h_ty[idx];
    wide = hx_is_wide(ty);

    /* NOP, PHI -- no code emitted (PHIs handled at block edges) */
    if (k == HI_NOP || k == HI_PHI) return;

    /* Rematerializable instructions -- no code emitted at definition site;
     * they are regenerated on demand by hx_mat(). */
    if (k == HI_ICONST || k == HI_ALLOCA || k == HI_GETFP) return;

    /* GADDR, SADDR, FADDR -- emit the relocation and store to slot.
     * We need to do this once at definition and load from slot on use
     * to avoid duplicate relocations. */
    if (k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) {
        hx_mat(idx, X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* COPY: src1 → dst */
    if (k == HI_COPY) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* ADDI: src1 + immediate → dst
     * Always 64-bit: the HIR (designed for 32-bit SLOW-32) often marks
     * pointer-offset ADDI as TY_INT.  64-bit add is always safe. */
    if (k == HI_ADDI) {
        hx_mat(h_src1[idx], X64_RAX);
        if (h_val[idx] != 0) {
            x64_add_ri64(X64_RAX, h_val[idx]);
        }
        hx_spill(idx, X64_RAX);
        return;
    }

    /* PARAM: already stored in prologue, nothing to do here */
    if (k == HI_PARAM) return;

    /* --- Binary arithmetic: load s1→RAX, s2→RCX, compute, store --- */

    if (k == HI_ADD) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_add_rr64(X64_RAX, X64_RCX);
        else x64_add_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_SUB) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_sub_rr64(X64_RAX, X64_RCX);
        else x64_sub_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_MUL) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_imul_rr64(X64_RAX, X64_RCX);
        else x64_imul_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_DIV) {
        /* dividend in RAX, divisor in R10, cdq/xor EDX, idiv/div R10 */
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_R10);
        if (wide) {
            if (ty & TY_UNSIGNED) { x64_xor_rr(X64_RDX, X64_RDX); x64_div64(X64_R10); }
            else { x64_cqo(); x64_idiv64(X64_R10); }
        } else {
            if (ty & TY_UNSIGNED) { x64_zero(X64_RDX); x64_div(X64_R10); }
            else { x64_cdq(); x64_idiv(X64_R10); }
        }
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_REM) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_R10);
        if (wide) {
            if (ty & TY_UNSIGNED) { x64_xor_rr(X64_RDX, X64_RDX); x64_div64(X64_R10); }
            else { x64_cqo(); x64_idiv64(X64_R10); }
        } else {
            if (ty & TY_UNSIGNED) { x64_zero(X64_RDX); x64_div(X64_R10); }
            else { x64_cdq(); x64_idiv(X64_R10); }
        }
        /* Remainder is in EDX/RDX */
        hx_spill(idx, X64_RDX);
        return;
    }

    /* --- Bitwise ops --- */

    if (k == HI_AND) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_and_rr64(X64_RAX, X64_RCX);
        else x64_and_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_OR) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_or_rr64(X64_RAX, X64_RCX);
        else x64_or_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_XOR) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_xor_rr64(X64_RAX, X64_RCX);
        else x64_xor_rr(X64_RAX, X64_RCX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* --- Shifts: count must be in CL (RCX) --- */

    if (k == HI_SLL) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_shl_cl64(X64_RAX);
        else x64_shl_cl(X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_SRA) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_sar_cl64(X64_RAX);
        else x64_sar_cl(X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_SRL) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) x64_shr_cl64(X64_RAX);
        else x64_shr_cl(X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* --- Unary ops --- */

    if (k == HI_NEG) {
        hx_mat(h_src1[idx], X64_RAX);
        if (wide) x64_neg64(X64_RAX);
        else x64_neg(X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_NOT) {
        /* Logical NOT: result = (src == 0) ? 1 : 0 */
        hx_mat(h_src1[idx], X64_RAX);
        if (hx_is_wide(h_ty[h_src1[idx]])) x64_test_rr64(X64_RAX, X64_RAX);
        else x64_test_rr(X64_RAX, X64_RAX);
        x64_sete(X64_RAX);
        x64_movzx_rr8(X64_RAX, X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_BNOT) {
        /* Bitwise NOT */
        hx_mat(h_src1[idx], X64_RAX);
        if (wide) x64_not64(X64_RAX);
        else x64_not(X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* --- Comparisons: CMP s1, s2 then SETcc --- */

    if (k >= HI_SEQ && k <= HI_SGEU) {
        int cmp_wide;
        cmp_wide = hx_is_wide(h_ty[h_src1[idx]]) || hx_is_wide(h_ty[h_src2[idx]]);

        hx_mat(h_src1[idx], X64_RCX);
        hx_mat(h_src2[idx], X64_RDX);
        if (cmp_wide) x64_cmp_rr64(X64_RCX, X64_RDX);
        else x64_cmp_rr(X64_RCX, X64_RDX);

        if (k == HI_SEQ) x64_sete(X64_RAX);
        else if (k == HI_SNE) x64_setne(X64_RAX);
        else if (k == HI_SLT) x64_setl(X64_RAX);
        else if (k == HI_SGT) x64_setg(X64_RAX);
        else if (k == HI_SLE) x64_setle(X64_RAX);
        else if (k == HI_SGE) x64_setge(X64_RAX);
        else if (k == HI_SLTU) x64_setb(X64_RAX);
        else if (k == HI_SGTU) x64_seta(X64_RAX);
        else if (k == HI_SLEU) x64_setbe(X64_RAX);
        else if (k == HI_SGEU) x64_setae(X64_RAX);

        x64_movzx_rr8(X64_RAX, X64_RAX);
        hx_spill(idx, X64_RAX);
        return;
    }

    /* --- Memory ops --- */

    if (k == HI_LOAD) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_load_typed(X64_RAX, X64_RAX, ty);
        hx_spill(idx, X64_RAX);
        return;
    }

    if (k == HI_STORE) {
        hx_mat(h_src1[idx], X64_RAX);   /* address */
        hx_mat(h_src2[idx], X64_RCX);   /* value */
        hx_store_typed(X64_RAX, X64_RCX, ty);
        return;
    }

    /* --- Control flow --- */

    if (k == HI_BR) {
        target = h_val[idx];
        hx_phi_copies(h_blk[idx], target);
        hx_jump_to_block(target);
        return;
    }

    if (k == HI_BRC) {
        int cur_blk;
        int true_blk;
        int false_blk;
        cur_blk = h_blk[idx];
        true_blk = h_src2[idx];    /* target if condition is TRUE */
        false_blk = h_val[idx];    /* target if condition is FALSE */

        hx_mat(h_src1[idx], X64_RAX);
        if (hx_is_wide(h_ty[h_src1[idx]])) x64_test_rr64(X64_RAX, X64_RAX);
        else x64_test_rr(X64_RAX, X64_RAX);

        /* JE to false path (condition is zero → false) */
        {
        int false_patch;
        false_patch = x64_jcc_placeholder(X64_CC_E);

        /* True path: phi copies + jump to true_blk */
        hx_phi_copies(cur_blk, true_blk);
        hx_jump_to_block(true_blk);

        /* False path */
        x64_patch_rel32(false_patch, x64_off);
        hx_phi_copies(cur_blk, false_blk);
        if (false_blk != cur_blk + 1) {
            hx_jump_to_block(false_blk);
        }
        }
        return;
    }

    if (k == HI_RET) {
        if (h_src1[idx] >= 0) {
            hx_mat(h_src1[idx], X64_RAX);
        }
        /* Jump to epilog */
        x64_jmp_rel32(0);
        /* Record patch -- epilog offset not known yet */
        hx_bpatch_blk[hx_nbpatch] = -1;  /* special: epilog */
        hx_bpatch_off[hx_nbpatch] = x64_off - 4;
        hx_nbpatch = hx_nbpatch + 1;
        return;
    }

    /* --- Function calls --- */

    if (k == HI_CALL || k == HI_CALLP) {
        int temp_bytes;
        int pad;
        nargs = h_val[idx];

        /* For indirect call, save callee address first */
        if (k == HI_CALLP) {
            hx_mat(h_src1[idx], X64_R11);
            x64_push(X64_R11);  /* save callee */
        }

        nreg = nargs < 6 ? nargs : 6;
        nstack = nargs > 6 ? nargs - 6 : 0;
        temp_bytes = nargs * 8;
        if (k == HI_CALLP) temp_bytes = temp_bytes + 8;
        outgoing = nstack * 8;
        pad = (16 - ((temp_bytes + outgoing) & 15)) & 15;
        outgoing = outgoing + pad;

        /* Push all args to stack as temp storage (left to right) */
        j = 0;
        while (j < nargs) {
            arg_idx = h_carg[h_cbase[idx] + j];
            hx_mat(arg_idx, X64_RAX);
            x64_push(X64_RAX);
            j = j + 1;
        }

        /* Load register args from stack positions */
        j = 0;
        while (j < nreg) {
            x64_mov_rm64(hx_arg_reg[j], X64_RSP, (nargs - 1 - j) * 8);
            j = j + 1;
        }

        /* Set up stack args if needed */
        if (nstack > 0) {
            x64_sub_ri64(X64_RSP, outgoing);
            j = 0;
            while (j < nstack) {
                x64_mov_rm64(X64_RAX, X64_RSP,
                             outgoing + (nstack - 1 - j) * 8);
                x64_mov_mr64(X64_RSP, j * 8, X64_RAX);
                j = j + 1;
            }
        }

        /* Emit the call */
        if (k == HI_CALLP) {
            /* Callee was pushed before args; it's at [RSP + outgoing + nargs*8] */
            x64_mov_rm64(X64_R11, X64_RSP, outgoing + nargs * 8);
            x64_call_r(X64_R11);
        } else {
            /* Direct call */
            x64_byte(0xE8);
            cg_cpatch_name[cg_ncpatches] = h_name[idx];
            cg_cpatch_off[cg_ncpatches] = x64_off;
            cg_ncpatches = cg_ncpatches + 1;
            x64_dword(0);
        }

        /* Clean up stack: args + outgoing + maybe saved callee */
        {
        int cleanup;
        cleanup = nargs * 8 + outgoing;
        if (k == HI_CALLP) cleanup = cleanup + 8;
        if (cleanup > 0) x64_add_ri64(X64_RSP, cleanup);
        }

        /* Store return value */
        if (hi_has_value(k)) {
            hx_spill(idx, X64_RAX);
        }
        return;
    }

    /* CALLHI: high 32 bits of 64-bit return.  On x64, the full 64-bit
     * result is already in RAX from the CALL.  Extract upper 32 bits. */
    if (k == HI_CALLHI) {
        hx_die("HI_CALLHI is not supported yet", idx);
    }

    /* --- Float ops: stub (not supported yet) --- */
    if (k >= HI_FADD && k <= HI_FCONST) {
        hx_die("floating-point HIR is not supported yet", idx);
    }
}

/* ============================================================================
 * Function generation
 * ============================================================================ */

static void hx_gen_func(Node *fn) {
    int b;
    int i;
    int fs;
    int pidx;
    Node *pp;

    hx_arg_reg[0] = X64_RDI;
    hx_arg_reg[1] = X64_RSI;
    hx_arg_reg[2] = X64_RDX;
    hx_arg_reg[3] = X64_RCX;
    hx_arg_reg[4] = X64_R8;
    hx_arg_reg[5] = X64_R9;

    hx_is_varargs = fn->is_varargs;
    hx_fn_nparams = fn->nparams;

    if (fn->is_varargs) {
        hx_die("varargs functions are not supported by --hir", -1);
    }

    /* Record function in symbol table */
    cg_func_name[cg_nfuncs] = fn->name;
    cg_func_off[cg_nfuncs] = x64_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* --- Run HIR pipeline --- */
    licm_stat_hoisted = 0;
    hl_func(fn);
    hir_ssa_construct();
    hir_opt();
    hir_licm();

    /* --- Allocate spill slots --- */
    hx_nslots = 0;
    hx_slot_base = -(16 + fn->locals_size * 2);

    i = 0;
    while (i < h_ninst) {
        hx_slot[i] = 0;
        if (hi_has_value(h_kind[i]) && !hi_is_remat(h_kind[i])) {
            /* Needs a spill slot (not rematerializable and not GADDR/SADDR/FADDR
             * which are handled specially but still need slots for their result) */
            hx_slot[i] = hx_alloc_slot();
        }
        /* GADDR, SADDR, FADDR produce values but go through hx_mat reloc path.
         * Still need a slot to cache the result. */
        if (h_kind[i] == HI_GADDR || h_kind[i] == HI_SADDR || h_kind[i] == HI_FADDR) {
            if (hx_slot[i] == 0) hx_slot[i] = hx_alloc_slot();
        }
        i = i + 1;
    }

    /* Allocate slots for LICM-hoisted instructions */
    b = 0;
    while (b < bb_nblk) {
        i = licm_head[b];
        while (i >= 0) {
            if (hi_has_value(h_kind[i]) && !hi_is_remat(h_kind[i])) {
                if (hx_slot[i] == 0) hx_slot[i] = hx_alloc_slot();
            }
            i = licm_next[i];
        }
        b = b + 1;
    }

    /* --- Compute frame size --- */
    fs = -(hx_slot_base) + hx_nslots * 8;
    fs = (fs + 15) & ~15;  /* 16-byte align */
    hx_frame_size = fs;

    /* --- Init block offsets --- */
    hx_nbpatch = 0;
    b = 0;
    while (b < bb_nblk) {
        hx_blk_off[b] = -1;
        b = b + 1;
    }

    /* --- Prologue --- */
    x64_push(X64_RBP);
    x64_mov_rr64(X64_RBP, X64_RSP);
    if (fs > 0) x64_sub_ri64(X64_RSP, fs);

    /* Store incoming register args to their PARAM instruction slots.
     * Scan HIR for PARAM instructions and store the corresponding register. */
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PARAM && hx_slot[i] != 0) {
            pidx = h_val[i];  /* physical param index */
            if (pidx < 6) {
                x64_mov_mr64(X64_RBP, hx_slot[i], hx_arg_reg[pidx]);
            } else {
                /* Stack arg: load from caller frame, store to slot */
                x64_mov_rm64(X64_RAX, X64_RBP, 16 + (pidx - 6) * 8);
                x64_mov_mr64(X64_RBP, hx_slot[i], X64_RAX);
            }
        }
        i = i + 1;
    }

    /* --- Emit blocks --- */
    b = 0;
    while (b < bb_nblk) {
        hx_define_block(b);

        /* Emit LICM-hoisted instructions at block top */
        i = licm_head[b];
        while (i >= 0) {
            hx_emit_inst(i);
            i = licm_next[i];
        }

        /* Emit PHI nodes -- these are no-ops (handled at edges) */
        i = ssa_phi_head[b];
        while (i >= 0) {
            /* PHI nodes don't emit code here */
            i = ssa_phi_next[i];
        }

        /* Emit block body */
        i = bb_start[b];
        while (i < bb_end[b]) {
            if (h_kind[i] != HI_NOP && h_kind[i] != HI_PHI)
                hx_emit_inst(i);
            i = i + 1;
        }

        b = b + 1;
    }

    /* --- Epilogue --- */
    {
    int epi_off;
    epi_off = x64_off;

    x64_mov_rr64(X64_RSP, X64_RBP);
    x64_pop(X64_RBP);
    x64_ret();

    /* Resolve epilog patches (RET instructions jump here) */
    i = 0;
    while (i < hx_nbpatch) {
        if (hx_bpatch_blk[i] == -1) {
            x64_patch_rel32(hx_bpatch_off[i], epi_off);
            hx_nbpatch = hx_nbpatch - 1;
            hx_bpatch_blk[i] = hx_bpatch_blk[hx_nbpatch];
            hx_bpatch_off[i] = hx_bpatch_off[hx_nbpatch];
        } else {
            i = i + 1;
        }
    }
    }
}

/* ============================================================================
 * Program generation -- mirrors gen_program() from codegen_x64.h
 * but uses hx_gen_func() for function codegen.
 * ============================================================================ */

static void hx_gen_program(Node *prog, int compile_only) {
    Node *fn;
    int entry_off;
    int i;

    /* Initialize state (same as gen_program) */
    i = 0;
    while (i < CG_MAX_LABELS) {
        cg_lbl_off[i] = -1;
        i = i + 1;
    }
    cg_npatches = 0;
    cg_nfuncs = 0;
    cg_ncpatches = 0;
    cg_ndrelocs = 0;
    cg_nstrings = 0;
    cg_str_pool_used = 0;
    cg_nglobals = 0;
    x64_off = 0;
    cg_object_mode = compile_only;

    /* Collect globals from parser */
    collect_globals(prog);

    /* Emit _start / __save_envp stubs (executable mode only) */
    if (!compile_only) {
        /* Fallback __save_envp (just ret) */
        cg_func_name[cg_nfuncs] = "__save_envp";
        cg_func_off[cg_nfuncs] = x64_off;
        cg_nfuncs = cg_nfuncs + 1;
        x64_ret();
    }

    entry_off = x64_off;

    if (!compile_only) {
        /* _start stub: same as gen_program */
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
    }

    /* Generate all functions via HIR pipeline */
    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) {
            hx_gen_func(fn);
        }
        fn = fn->next;
    }

    /* Copy string pool from lexer (same as gen_program) */
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

    /* Build data sections */
    gen_data_sections(prog);

    /* Record code size */
    cg_olen = x64_off;

    if (compile_only) return;

    /* Executable mode: resolve and set up ELF */
    elf_init();
    elf_set_text(x64_buf, x64_off);
    if (cg_rodata_len > 0) elf_set_rodata(cg_rodata, cg_rodata_len);
    if (cg_data_len > 0 || cg_bss_size > 0) elf_set_data(cg_data, cg_data_len);
    if (cg_bss_size > 0) elf_set_bss(cg_bss_size);
    elf_set_entry(entry_off);

    resolve_calls();
    resolve_relocations();
}

#endif /* HIR_CODEGEN_X64_H */
