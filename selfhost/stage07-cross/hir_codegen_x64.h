/* hir_codegen_x64.h -- HIR to x86-64 code generator with register allocation
 *
 * Uses linear-scan register allocation (hir_regalloc_x64.h) to assign
 * callee-saved registers (RBX, R12-R15).  Values that don't fit are
 * spilled to the stack.  RAX/RCX/RDX serve as scratch registers.
 *
 * Requires: x64_encode.h, codegen_x64.h (for infrastructure arrays),
 *           hir.h, hir_lower.h, hir_ssa.h, hir_opt.h, hir_licm.h,
 *           hir_regalloc_x64.h
 */

#ifndef HIR_CODEGEN_X64_H
#define HIR_CODEGEN_X64_H

/* ============================================================================
 * Per-function state
 * ============================================================================ */

static int hx_frame_size;            /* total frame size */
static int hx_is_varargs;            /* 1 if current function is variadic */
static int hx_fn_nparams;            /* number of named params */
static int hx_param_need_temp;       /* 1 if prologue needs temp area for args */
static int hx_param_temp_base;       /* RBP offset of param temp area */

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

    /* Regalloc: if value is in a physical register, use it directly */
    if (ra_reg[inst] >= 0) {
        if (reg != ra_reg[inst])
            x64_mov_rr64(reg, ra_reg[inst]);
        return;
    }

    k = h_kind[inst];

    /* Rematerializable: ICONST */
    if (k == HI_ICONST) {
        if (h_val[inst] == 0) {
            x64_xor_rr(reg, reg);
        } else if (hx_is_wide(h_ty[inst])) {
            /* HIR integer constants are 32-bit values; for wide x64 uses,
             * preserve signed semantics instead of zero-extending. */
            x64_mov_ri(reg, h_val[inst]);
            x64_movsxd(reg, reg);
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
    off = ra_spill_off[inst];
    if (off != 0) {
        x64_mov_rm64(reg, X64_RBP, off);
    } else {
        /* No slot (unused value or error) -- zero */
        x64_xor_rr(reg, reg);
    }
}

/* Store a value to its destination (physical register or spill slot). */
static void hx_spill(int inst, int reg) {
    int off;
    if (inst < 0) return;

    /* Regalloc: if value has a physical register, move to it */
    if (ra_reg[inst] >= 0) {
        if (reg != ra_reg[inst])
            x64_mov_rr64(ra_reg[inst], reg);
        return;
    }

    /* Rematerializable: no store needed (regenerated on demand) */
    if (hi_is_remat(h_kind[inst]) &&
        h_kind[inst] != HI_GADDR &&
        h_kind[inst] != HI_SADDR &&
        h_kind[inst] != HI_FADDR) {
        return;
    }

    /* Store to spill slot */
    off = ra_spill_off[inst];
    if (off == 0) return;
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
 * Register-aware helpers: hx_src / hx_dst / hx_maybe_spill
 *
 * These avoid the RAX detour when values are already in registers.
 * hx_src returns the register holding a value (no code if allocated).
 * hx_dst returns the register to write the result into.
 * hx_maybe_spill stores the result to its spill slot if needed.
 * ============================================================================ */

/* Return register containing value.  If allocated, returns the physical
 * register (no code emitted).  Otherwise materializes into scratch. */
static int hx_src(int inst, int scratch) {
    if (inst < 0) {
        x64_xor_rr(scratch, scratch);
        return scratch;
    }
    if (ra_reg[inst] >= 0) return ra_reg[inst];
    hx_mat(inst, scratch);
    return scratch;
}

/* Return destination register for a result.  If allocated, returns the
 * physical register.  Otherwise returns RAX (scratch). */
static int hx_dst(int inst) {
    if (ra_reg[inst] >= 0) return ra_reg[inst];
    return X64_RAX;
}

/* If inst is spilled (not in a register), store RAX to its spill slot. */
static void hx_maybe_spill(int inst) {
    int off;
    if (ra_reg[inst] >= 0) return;
    if (hi_is_remat(h_kind[inst]) &&
        h_kind[inst] != HI_GADDR &&
        h_kind[inst] != HI_SADDR &&
        h_kind[inst] != HI_FADDR) return;
    off = ra_spill_off[inst];
    if (off == 0) return;
    x64_mov_mr64(X64_RBP, off, X64_RAX);
}

/* Emit mov dr, sr (64-bit) only if registers differ. */
static void hx_mov_if_needed64(int dr, int sr) {
    if (dr != sr) x64_mov_rr64(dr, sr);
}

/* Emit mov dr, sr (32-bit) only if registers differ. */
static void hx_mov_if_needed(int dr, int sr) {
    if (dr != sr) x64_mov_rr(dr, sr);
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
        {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        hx_mov_if_needed64(dr, sr);
        hx_maybe_spill(idx);
        }
        return;
    }

    /* ADDI: src1 + immediate → dst
     * Always 64-bit: the HIR (designed for 32-bit SLOW-32) often marks
     * pointer-offset ADDI as TY_INT.  64-bit add is always safe. */
    if (k == HI_ADDI) {
        {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        hx_mov_if_needed64(dr, sr);
        if (h_val[idx] != 0) x64_add_ri64(dr, h_val[idx]);
        hx_maybe_spill(idx);
        }
        return;
    }

    /* PARAM: already stored in prologue, nothing to do here */
    if (k == HI_PARAM) return;

    /* --- Binary ALU: dst = s1 OP s2 ---
     *
     * Commutative ops (ADD, MUL, AND, OR, XOR): if dr==s2r, swap operands.
     * Non-commutative (SUB): if dr==s2r, save s2 to RDX first.
     * x64 ops are destructive (dst = dst OP src), so we mov s1→dr first. */

    if (k == HI_ADD || k == HI_SUB || k == HI_MUL ||
        k == HI_AND || k == HI_OR  || k == HI_XOR) {
        int s1r;
        int s2r;
        int dr;
        int commute;
        int tmp;

        commute = (k != HI_SUB);
        dr = hx_dst(idx);
        s1r = hx_src(h_src1[idx], X64_RAX);
        s2r = hx_src(h_src2[idx], X64_RCX);

        /* Handle dr==s2r conflict */
        if (dr == s2r && dr != s1r) {
            if (commute) { tmp = s1r; s1r = s2r; s2r = tmp; }
            else { x64_mov_rr64(X64_RDX, s2r); s2r = X64_RDX; }
        }
        if (wide) hx_mov_if_needed64(dr, s1r); else hx_mov_if_needed(dr, s1r);

        if (k == HI_ADD) { if (wide) x64_add_rr64(dr, s2r); else x64_add_rr(dr, s2r); }
        else if (k == HI_SUB) { if (wide) x64_sub_rr64(dr, s2r); else x64_sub_rr(dr, s2r); }
        else if (k == HI_MUL) { if (wide) x64_imul_rr64(dr, s2r); else x64_imul_rr(dr, s2r); }
        else if (k == HI_AND) { if (wide) x64_and_rr64(dr, s2r); else x64_and_rr(dr, s2r); }
        else if (k == HI_OR)  { if (wide) x64_or_rr64(dr, s2r); else x64_or_rr(dr, s2r); }
        else if (k == HI_XOR) { if (wide) x64_xor_rr64(dr, s2r); else x64_xor_rr(dr, s2r); }

        hx_maybe_spill(idx);
        return;
    }

    /* DIV/REM: fixed registers (RAX dividend, RDX extension, result in RAX/RDX) */
    if (k == HI_DIV || k == HI_REM) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_R10);
        if (wide) {
            if (ty & TY_UNSIGNED) { x64_xor_rr(X64_RDX, X64_RDX); x64_div64(X64_R10); }
            else { x64_cqo(); x64_idiv64(X64_R10); }
        } else {
            if (ty & TY_UNSIGNED) { x64_zero(X64_RDX); x64_div(X64_R10); }
            else { x64_cdq(); x64_idiv(X64_R10); }
        }
        if (k == HI_DIV) hx_spill(idx, X64_RAX);
        else hx_spill(idx, X64_RDX);
        return;
    }

    /* --- Shifts: value in dr, count must be in CL (RCX) --- */

    if (k == HI_SLL || k == HI_SRA || k == HI_SRL) {
        int s1r;
        int s2r;
        int dr;
        dr = hx_dst(idx);
        s1r = hx_src(h_src1[idx], X64_RAX);
        s2r = hx_src(h_src2[idx], X64_RCX);
        if (s2r != X64_RCX) x64_mov_rr(X64_RCX, s2r);
        /* If dr is RCX (impossible since RCX is not allocatable), we'd have a
         * problem.  dr is always an allocated reg or RAX, never RCX. */
        if (wide) hx_mov_if_needed64(dr, s1r); else hx_mov_if_needed(dr, s1r);
        if (k == HI_SLL) { if (wide) x64_shl_cl64(dr); else x64_shl_cl(dr); }
        else if (k == HI_SRA) { if (wide) x64_sar_cl64(dr); else x64_sar_cl(dr); }
        else { if (wide) x64_shr_cl64(dr); else x64_shr_cl(dr); }
        hx_maybe_spill(idx);
        return;
    }

    /* --- Unary ops: dr = OP(s1) --- */

    if (k == HI_NEG) {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        if (wide) { hx_mov_if_needed64(dr, sr); x64_neg64(dr); }
        else { hx_mov_if_needed(dr, sr); x64_neg(dr); }
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_NOT) {
        /* Logical NOT: result = (src == 0) ? 1 : 0.
         * Always compute into RAX (SETcc writes AL), then move to dr. */
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        if (hx_is_wide(h_ty[h_src1[idx]])) x64_test_rr64(sr, sr);
        else x64_test_rr(sr, sr);
        x64_sete(X64_RAX);
        x64_movzx_rr8(X64_RAX, X64_RAX);
        dr = hx_dst(idx);
        hx_mov_if_needed(dr, X64_RAX);
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_BNOT) {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        if (wide) { hx_mov_if_needed64(dr, sr); x64_not64(dr); }
        else { hx_mov_if_needed(dr, sr); x64_not(dr); }
        hx_maybe_spill(idx);
        return;
    }

    /* --- Comparisons: CMP s1, s2 then SETcc → RAX → dr --- */

    if (k >= HI_SEQ && k <= HI_SGEU) {
        int cmp_wide;
        int s1r;
        int s2r;
        int dr;
        cmp_wide = hx_is_wide(h_ty[h_src1[idx]]) || hx_is_wide(h_ty[h_src2[idx]]);
        s1r = hx_src(h_src1[idx], X64_RAX);
        s2r = hx_src(h_src2[idx], X64_RCX);
        if (cmp_wide) x64_cmp_rr64(s1r, s2r);
        else x64_cmp_rr(s1r, s2r);

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
        dr = hx_dst(idx);
        hx_mov_if_needed(dr, X64_RAX);
        hx_maybe_spill(idx);
        return;
    }

    /* --- Memory ops --- */

    if (k == HI_LOAD) {
        int ar;
        int dr;
        ar = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        hx_load_typed(dr, ar, ty);
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_STORE) {
        int ar;
        int vr;
        ar = hx_src(h_src1[idx], X64_RAX);   /* address */
        vr = hx_src(h_src2[idx], X64_RCX);   /* value */
        hx_store_typed(ar, vr, ty);
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
        int inv_cc;
        int cmp_idx;
        int false_patch;

        cur_blk = h_blk[idx];
        true_blk = h_src2[idx];    /* target if condition is TRUE */
        false_blk = h_val[idx];    /* target if condition is FALSE */

        cmp_idx = hx_brc_fuse[idx];
        if (cmp_idx >= 0) {
            /* Fused compare-and-branch: emit CMP + inverted Jcc */
            int ck;
            int s1r;
            int s2r;
            int cmp_wide;

            ck = hx_cmp_kind[cmp_idx];
            cmp_wide = hx_is_wide(h_ty[h_src1[cmp_idx]]) ||
                        hx_is_wide(h_ty[h_src2[cmp_idx]]);
            s1r = hx_src(h_src1[cmp_idx], X64_RAX);
            s2r = hx_src(h_src2[cmp_idx], X64_RCX);
            if (cmp_wide) x64_cmp_rr64(s1r, s2r);
            else x64_cmp_rr(s1r, s2r);

            /* Inverted condition: jump to false path when condition is FALSE */
            if (ck == HI_SEQ) inv_cc = X64_CC_NE;
            else if (ck == HI_SNE) inv_cc = X64_CC_E;
            else if (ck == HI_SLT) inv_cc = X64_CC_GE;
            else if (ck == HI_SGT) inv_cc = X64_CC_LE;
            else if (ck == HI_SLE) inv_cc = X64_CC_G;
            else if (ck == HI_SGE) inv_cc = X64_CC_L;
            else if (ck == HI_SLTU) inv_cc = X64_CC_AE;
            else if (ck == HI_SGTU) inv_cc = X64_CC_BE;
            else if (ck == HI_SLEU) inv_cc = X64_CC_A;
            else inv_cc = X64_CC_B;  /* SGEU → JB */
        } else {
            /* Unfused: TEST condition + JE */
            int cr;
            cr = hx_src(h_src1[idx], X64_RAX);
            if (hx_is_wide(h_ty[h_src1[idx]])) x64_test_rr64(cr, cr);
            else x64_test_rr(cr, cr);
            inv_cc = X64_CC_E;
        }

        /* Jump to false path when condition is FALSE */
        false_patch = x64_jcc_placeholder(inv_cc);

        /* True path: phi copies + jump to true_blk */
        hx_phi_copies(cur_blk, true_blk);
        hx_jump_to_block(true_blk);

        /* False path */
        x64_patch_rel32(false_patch, x64_off);
        hx_phi_copies(cur_blk, false_blk);
        if (false_blk != cur_blk + 1) {
            hx_jump_to_block(false_blk);
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

        /* Reserve outgoing area (stack args + alignment padding) */
        if (outgoing > 0) {
            x64_sub_ri64(X64_RSP, outgoing);
            /* Copy stack args (7+) into outgoing area */
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
    int k;
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

    /* Record function in symbol table (after varargs check so we don't
     * leave a partial entry on fatal error) */
    cg_func_name[cg_nfuncs] = fn->name;
    cg_func_off[cg_nfuncs] = x64_off;
    cg_nfuncs = cg_nfuncs + 1;

    /* --- Run HIR pipeline --- */
    licm_stat_hoisted = 0;
    hl_func(fn);
    hir_ssa_construct();
    hir_opt();
    hir_licm();

    /* --- Register allocation ---
     * hl_temp_stack was set by the lowering to fn->locals_size (SLOW-32 bytes).
     * Scale to x64 (8-byte slots) before the regalloc adds spill/callee-save
     * slots so all offsets are in the same coordinate system. */
    hl_temp_stack = 16 + fn->locals_size * 2;
    hx_identify_fusions();
    hir_regalloc();

    /* Assign spill slots for GADDR/SADDR/FADDR (regalloc treats them as
     * rematerializable, but we cache relocation results to avoid duplicates) */
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if ((k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) &&
            ra_reg[i] < 0 && ra_spill_off[i] == 0) {
            hl_temp_stack = hl_temp_stack + 8;
            ra_spill_off[i] = 0 - hl_temp_stack;
        }
        i = i + 1;
    }

    /* --- Detect arg-register conflicts for prologue ---
     * With caller-saved registers allocatable, a PARAM may be assigned
     * to a register that's also an incoming-arg register for a different
     * parameter.  If so, allocate a temp area in the frame. */
    hx_param_need_temp = 0;
    hx_param_temp_base = 0;
    {
    int pi;
    i = 0;
    while (i < h_ninst && !hx_param_need_temp) {
        if (h_kind[i] == HI_PARAM && ra_reg[i] >= 0) {
            pidx = h_val[i];
            if (pidx < 6) {
                pi = 0;
                while (pi < 6 && !hx_param_need_temp) {
                    if (hx_arg_reg[pi] == ra_reg[i] && pi != pidx)
                        hx_param_need_temp = 1;
                    pi = pi + 1;
                }
            }
        }
        i = i + 1;
    }
    if (hx_param_need_temp) {
        hl_temp_stack = hl_temp_stack + 48;  /* 6 * 8 bytes for arg save area */
        hx_param_temp_base = 0 - hl_temp_stack;
    }
    }

    /* --- Compute frame size --- */
    fs = hl_temp_stack;
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

    /* Save callee-saved registers used by regalloc */
    i = 0;
    while (i < ra_ncsave) {
        x64_mov_mr64(X64_RBP, ra_csave_off[i], ra_csave_reg[i]);
        i = i + 1;
    }

    /* Store incoming args to their allocated register or spill slot. */
    if (hx_param_need_temp) {
        /* Two-phase: save all 6 arg registers to temp area, then distribute */
        int pi;
        pi = 0;
        while (pi < 6) {
            x64_mov_mr64(X64_RBP, hx_param_temp_base + pi * 8, hx_arg_reg[pi]);
            pi = pi + 1;
        }
        i = 0;
        while (i < h_ninst) {
            if (h_kind[i] == HI_PARAM) {
                pidx = h_val[i];
                if (ra_reg[i] >= 0) {
                    if (pidx < 6)
                        x64_mov_rm64(ra_reg[i], X64_RBP, hx_param_temp_base + pidx * 8);
                    else
                        x64_mov_rm64(ra_reg[i], X64_RBP, 16 + (pidx - 6) * 8);
                } else if (ra_spill_off[i] != 0) {
                    if (pidx < 6) {
                        x64_mov_rm64(X64_RAX, X64_RBP, hx_param_temp_base + pidx * 8);
                        x64_mov_mr64(X64_RBP, ra_spill_off[i], X64_RAX);
                    } else {
                        x64_mov_rm64(X64_RAX, X64_RBP, 16 + (pidx - 6) * 8);
                        x64_mov_mr64(X64_RBP, ra_spill_off[i], X64_RAX);
                    }
                }
            }
            i = i + 1;
        }
    } else {
        /* No conflicts: direct move (fast path) */
        i = 0;
        while (i < h_ninst) {
            if (h_kind[i] == HI_PARAM) {
                pidx = h_val[i];
                if (ra_reg[i] >= 0) {
                    if (pidx < 6) {
                        if (ra_reg[i] != hx_arg_reg[pidx])
                            x64_mov_rr64(ra_reg[i], hx_arg_reg[pidx]);
                    } else {
                        x64_mov_rm64(ra_reg[i], X64_RBP, 16 + (pidx - 6) * 8);
                    }
                } else if (ra_spill_off[i] != 0) {
                    if (pidx < 6) {
                        x64_mov_mr64(X64_RBP, ra_spill_off[i], hx_arg_reg[pidx]);
                    } else {
                        x64_mov_rm64(X64_RAX, X64_RBP, 16 + (pidx - 6) * 8);
                        x64_mov_mr64(X64_RBP, ra_spill_off[i], X64_RAX);
                    }
                }
            }
            i = i + 1;
        }
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

    /* Restore callee-saved registers */
    i = 0;
    while (i < ra_ncsave) {
        x64_mov_rm64(ra_csave_reg[i], X64_RBP, ra_csave_off[i]);
        i = i + 1;
    }

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
