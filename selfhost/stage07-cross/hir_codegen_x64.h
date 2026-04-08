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
static int hx_va_save_off;           /* RBP offset of varargs register save area */
static int hx_no_frame;             /* 1 if function needs no frame (no spills, no callee-saves) */

/* Block code offsets for jump patching */
static int hx_blk_off[HIR_MAX_BLOCK];  /* code offset of block start, -1=not yet */

/* Forward jump patches: jump at patch_off targets block patch_blk */
#define HX_MAX_BPATCH 8192
static int hx_bpatch_blk[HX_MAX_BPATCH];
static int hx_bpatch_off[HX_MAX_BPATCH];  /* offset of rel32 in x64_buf */
static int hx_nbpatch;

/* ADDI folding: hx_addi_folded_flag[i]=1 if load/store i has a folded ADDI,
 * hx_addi_folded_disp[i] = the displacement to use in [base+disp] form. */
static int hx_addi_folded_flag[HIR_MAX_INST];
static int hx_addi_folded_disp[HIR_MAX_INST];
/* Legacy name for ADDI emitter check (1 if this ADDI was folded) */
static int hx_addi_folded[HIR_MAX_INST];

/* SIB fold: LOAD/STORE with [base + index*scale] addressing.
 * For LOAD:  src1=base, src2=index (rewritten), scale in hx_sib_scale.
 * For STORE: src1=base, src2=value (unchanged), index in hx_sib_index.
 * NOTE: hx_sib_index[] is declared in hir_regalloc_x64.h (for liveness). */
static int hx_sib_flag[HIR_MAX_INST];     /* 1 if this load/store uses SIB */
static int hx_sib_scale[HIR_MAX_INST];    /* SIB scale: 1=*2, 2=*4, 3=*8 */

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

/* Compute the deepest x64 frame offset already occupied by lowered HIR
 * allocas.  This must include lowering-introduced temporaries, not just
 * parser-level locals_size, or regalloc spill slots can overlap them. */
static int hx_hir_frame_base(Node *fn) {
    int base;
    int i;
    int off;
    int has_alloca;

    /* Start with 0 and only reserve space if there are actual allocas
     * or parser-level locals that need stack slots. */
    has_alloca = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ALLOCA) {
            has_alloca = 1;
            break;
        }
        i = i + 1;
    }

    if (!has_alloca) return 0;

    base = 16 + fn->locals_size * 2;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ALLOCA && h_val[i] < 0) {
            off = 0 - cg_x64_offset(h_val[i]);
            if (off > base) base = off;
        }
        i = i + 1;
    }
    return base;
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

/* Load from [addr_reg + disp] into dst_reg with appropriate width for ty. */
static void hx_load_typed_off(int dst, int addr, int disp, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm8(dst, addr, disp);
        else
            x64_movsx_rm8(dst, addr, disp);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm16(dst, addr, disp);
        else
            x64_movsx_rm16(dst, addr, disp);
    } else if (hx_is_wide(ty)) {
        x64_mov_rm64(dst, addr, disp);
    } else {
        x64_mov_rm(dst, addr, disp);
    }
}

/* Store src_reg to [addr_reg + disp] with appropriate width for ty. */
static void hx_store_typed_off(int addr, int disp, int src, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        x64_mov_mr8(addr, disp, src);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        x64_mov_mr16(addr, disp, src);
    } else if (hx_is_wide(ty)) {
        x64_mov_mr64(addr, disp, src);
    } else {
        x64_mov_mr(addr, disp, src);
    }
}

/* Materialize an incoming PARAM value into dst_reg with the correct width.
 * SysV leaves the upper 32 bits of integer argument registers undefined,
 * so non-wide params must be canonicalized with 32-bit moves/loads. */
static void hx_param_to_reg(int dst_reg, int param_inst, int param_idx, int from_temp) {
    int ty;
    int wide;
    int src_off;

    ty = h_ty[param_inst];
    wide = hx_is_wide(ty);
    if (from_temp && param_idx < 6) src_off = hx_param_temp_base + param_idx * 8;
    else src_off = 16 + (param_idx - 6) * 8;

    if (param_idx < 6 && !from_temp) {
        if (wide) {
            if (dst_reg != hx_arg_reg[param_idx])
                x64_mov_rr64(dst_reg, hx_arg_reg[param_idx]);
        } else {
            if (dst_reg != hx_arg_reg[param_idx])
                x64_mov_rr(dst_reg, hx_arg_reg[param_idx]);
        }
    } else {
        if (wide) x64_mov_rm64(dst_reg, X64_RBP, src_off);
        else x64_mov_rm(dst_reg, X64_RBP, src_off);
    }
}

/* Store an incoming PARAM value to a spill slot with the correct width.
 * Non-wide params are canonicalized into RAX with a 32-bit move/load
 * before the full 64-bit spill store, so later 64-bit spill reloads are safe. */
static void hx_param_to_spill(int spill_off, int param_inst, int param_idx, int from_temp) {
    int ty;
    int wide;
    int src_off;

    ty = h_ty[param_inst];
    wide = hx_is_wide(ty);
    if (from_temp && param_idx < 6) src_off = hx_param_temp_base + param_idx * 8;
    else src_off = 16 + (param_idx - 6) * 8;

    if (param_idx < 6 && !from_temp) {
        if (wide) x64_mov_rr64(X64_RAX, hx_arg_reg[param_idx]);
        else x64_mov_rr(X64_RAX, hx_arg_reg[param_idx]);
    } else {
        if (wide) x64_mov_rm64(X64_RAX, X64_RBP, src_off);
        else x64_mov_rm(X64_RAX, X64_RBP, src_off);
    }
    x64_mov_mr64(X64_RBP, spill_off, X64_RAX);
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
     * If BURG selected this as MEM or SADDR, it was folded — no code.
     * If codegen folded it into a load/store addressing mode, also no code.
     * Otherwise emit LEA or ADD. */
    if (k == HI_ADDI) {
        {
        int pat;
        int sel_nt;
        pat = bg_sel[idx];
        sel_nt = (pat >= 0) ? bg_pnt[pat] : -1;
        /* If result NT is MEM or SADDR, this ADDI is folded into a parent
         * load/store — no code emitted at definition site. */
        if (sel_nt == BG_MEM || sel_nt == BG_SADDR || sel_nt == BG_IMM) {
            /* Folded or constant-propagated: no code */
        } else if (hx_addi_folded[idx]) {
            /* Will be folded into load/store addressing mode: no code */
        } else {
            int sr;
            int dr;
            sr = hx_src(h_src1[idx], X64_RAX);
            dr = hx_dst(idx);
            hx_mov_if_needed64(dr, sr);
            if (h_val[idx] != 0) x64_add_ri64(dr, h_val[idx]);
            hx_maybe_spill(idx);
        }
        }
        return;
    }

    /* PARAM: already stored in prologue, nothing to do here */
    if (k == HI_PARAM) return;

    /* --- Binary ALU: dst = s1 OP s2 ---
     *
     * If src2 is ICONST, use immediate form (add dr, imm) to avoid a
     * register load.  MUL uses the three-operand imul dr, s1r, imm.
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
        int s2_inst;
        int imm_val;
        int use_imm;

        commute = (k != HI_SUB);
        s2_inst = h_src2[idx];

        /* BURG-driven immediate selection: if the selected pattern has
         * an IMM operand, use immediate form */
        use_imm = 0;
        {
        int pat;
        int lnt_p;
        int rnt_p;
        int reg_src;
        pat = bg_sel[idx];
        lnt_p = (pat >= 0) ? bg_plnt[pat] : -1;
        rnt_p = (pat >= 0) ? bg_prnt[pat] : -1;
        reg_src = -1;
        if (rnt_p == BG_IMM && !wide) {
            imm_val = h_val[s2_inst];
            reg_src = h_src1[idx];
            use_imm = 1;
        } else if (lnt_p == BG_IMM && commute && !wide) {
            imm_val = h_val[h_src1[idx]];
            reg_src = h_src2[idx];
            use_imm = 1;
        }
        }

        if (use_imm) {
            /* Immediate form: op dr, imm (reg_src has the register operand) */
            int reg_src;
            reg_src = (bg_prnt[bg_sel[idx]] == BG_IMM) ? h_src1[idx] : h_src2[idx];
            dr = hx_dst(idx);
            s1r = hx_src(reg_src, X64_RAX);
            if (k == HI_MUL) {
                x64_imul_rri(dr, s1r, imm_val);
            } else {
                hx_mov_if_needed(dr, s1r);
            }
            if (k == HI_ADD) x64_add_ri(dr, imm_val);
            else if (k == HI_SUB) x64_sub_ri(dr, imm_val);
            else if (k == HI_MUL) { }
            else if (k == HI_AND) x64_and_ri(dr, imm_val);
            else if (k == HI_OR)  x64_or_ri(dr, imm_val);
            else if (k == HI_XOR) x64_xor_ri(dr, imm_val);
        }
        /* Memory-operand form: op dr, [rbp+disp]
         * When src2 is a single-use LOAD(MEM) in the same block,
         * fold the load into the ALU instruction.  Not for MUL (no
         * memory-operand imul form we use), wide values, or narrow
         * loads that require sign/zero extension. */
        else if (!wide && k != HI_MUL &&
                 s2_inst >= 0 && h_kind[s2_inst] == HI_LOAD &&
                 bg_uses[s2_inst] == 1 &&
                 h_blk[s2_inst] == h_blk[idx] &&
                 !(!ty_is_ptr(h_ty[s2_inst]) &&
                   (((h_ty[s2_inst] & TY_BASE_MASK) == TY_CHAR) ||
                    ((h_ty[s2_inst] & TY_BASE_MASK) == TY_SHORT))) &&
                 bg_plnt[(bg_sel[s2_inst] >= 0) ? bg_sel[s2_inst] : 0] == BG_MEM &&
                 bg_sel[s2_inst] >= 0) {
            int foff;
            foff = bg_foff[h_src1[s2_inst]];
            dr = hx_dst(idx);
            s1r = hx_src(h_src1[idx], X64_RAX);
            hx_mov_if_needed(dr, s1r);
            if (k == HI_ADD) x64_add_rm(dr, X64_RBP, foff);
            else if (k == HI_SUB) x64_sub_rm(dr, X64_RBP, foff);
            else if (k == HI_AND) x64_and_rm(dr, X64_RBP, foff);
            else if (k == HI_OR)  x64_or_rm(dr, X64_RBP, foff);
            else if (k == HI_XOR) x64_xor_rm(dr, X64_RBP, foff);
        } else {
            /* Register form */
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
        }

        hx_maybe_spill(idx);
        return;
    }

    /* DIV/REM: fixed registers (RAX dividend, RDX extension, result in RAX/RDX).
     * Divisor goes into RCX (scratch, not allocatable) to avoid clobbering
     * allocatable R10. */
    if (k == HI_DIV || k == HI_REM) {
        hx_mat(h_src1[idx], X64_RAX);
        hx_mat(h_src2[idx], X64_RCX);
        if (wide) {
            if (ty & TY_UNSIGNED) { x64_xor_rr(X64_RDX, X64_RDX); x64_div64(X64_RCX); }
            else { x64_cqo(); x64_idiv64(X64_RCX); }
        } else {
            if (ty & TY_UNSIGNED) { x64_zero(X64_RDX); x64_div(X64_RCX); }
            else { x64_cdq(); x64_idiv(X64_RCX); }
        }
        if (k == HI_DIV) hx_spill(idx, X64_RAX);
        else hx_spill(idx, X64_RDX);
        return;
    }

    /* --- Shifts: immediate count uses shl/shr/sar dr, imm;
     *             variable count requires CL --- */

    if (k == HI_SLL || k == HI_SRA || k == HI_SRL) {
        int s1r;
        int dr;
        int s2_inst;

        dr = hx_dst(idx);
        s1r = hx_src(h_src1[idx], X64_RAX);
        s2_inst = h_src2[idx];

        if (s2_inst >= 0 && h_kind[s2_inst] == HI_ICONST) {
            /* Shift by immediate constant */
            int cnt;
            cnt = h_val[s2_inst];
            if (wide) hx_mov_if_needed64(dr, s1r); else hx_mov_if_needed(dr, s1r);
            if (k == HI_SLL) x64_shl_ri(dr, cnt);
            else if (k == HI_SRA) x64_sar_ri(dr, cnt);
            else x64_shr_ri(dr, cnt);
        } else {
            /* Variable shift: count in CL */
            int s2r;
            s2r = hx_src(s2_inst, X64_RCX);
            if (s2r != X64_RCX) x64_mov_rr(X64_RCX, s2r);
            if (wide) hx_mov_if_needed64(dr, s1r); else hx_mov_if_needed(dr, s1r);
            if (k == HI_SLL) { if (wide) x64_shl_cl64(dr); else x64_shl_cl(dr); }
            else if (k == HI_SRA) { if (wide) x64_sar_cl64(dr); else x64_sar_cl(dr); }
            else { if (wide) x64_shr_cl64(dr); else x64_shr_cl(dr); }
        }
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
        int pat;
        int rnt_p;
        int ca;
        int cb;
        ca = h_src1[idx];
        cb = h_src2[idx];
        cmp_wide = hx_is_wide(h_ty[ca]) || hx_is_wide(h_ty[cb]);
        pat = bg_sel[idx];
        rnt_p = (pat >= 0) ? bg_prnt[pat] : -1;

        /* Comparison with zero → TEST */
        if (cb >= 0 && h_kind[cb] == HI_ICONST && h_val[cb] == 0 &&
            (k == HI_SEQ || k == HI_SNE)) {
            s1r = hx_src(ca, X64_RAX);
            if (cmp_wide) x64_test_rr64(s1r, s1r);
            else x64_test_rr(s1r, s1r);
        }
        /* Comparison with immediate → CMP r, imm */
        else if (rnt_p == BG_IMM && !cmp_wide) {
            s1r = hx_src(ca, X64_RAX);
            x64_cmp_ri(s1r, h_val[cb]);
        } else {
            s1r = hx_src(ca, X64_RAX);
            s2r = hx_src(cb, X64_RCX);
            if (cmp_wide) x64_cmp_rr64(s1r, s2r);
            else x64_cmp_rr(s1r, s2r);
        }

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
        int pat;
        int lnt;
        pat = bg_sel[idx];
        lnt = (pat >= 0) ? bg_plnt[pat] : -1;
        dr = hx_dst(idx);
        if (lnt == BG_MEM) {
            /* BURG: load from frame-relative [RBP + disp] */
            int foff;
            foff = bg_foff[h_src1[idx]];
            hx_load_typed_off(dr, X64_RBP, foff, ty);
        } else if (lnt == BG_SADDR) {
            /* BURG: load from symbol address [+ constant offset] */
            int base_inst;
            int soff;
            base_inst = bg_ssym[h_src1[idx]];
            soff = bg_soff[h_src1[idx]];
            if (base_inst < 0) hx_die("invalid folded SADDR in load", idx);
            hx_mat(base_inst, X64_RCX);
            hx_load_typed_off(dr, X64_RCX, soff, ty);
        } else {
            /* Fold ADDI displacement: if this load has a pre-folded ADDI,
             * src1 already points to the base and disp is saved. */
            if (hx_addi_folded_flag[idx]) {
                ar = hx_src(h_src1[idx], X64_RAX);
                hx_load_typed_off(dr, ar, hx_addi_folded_disp[idx], ty);
            } else if (hx_sib_flag[idx]) {
                /* SIB: src1=base, src2=index, scale in hx_sib_scale */
                int br;
                int ir;
                br = hx_src(h_src1[idx], X64_RAX);
                ir = hx_src(h_src2[idx], X64_RCX);
                if (wide) {
                    x64_mov_rm64_sib(dr, br, ir, hx_sib_scale[idx], 0);
                } else {
                    x64_mov_rm_sib(dr, br, ir, hx_sib_scale[idx], 0);
                }
            } else {
                ar = hx_src(h_src1[idx], X64_RAX);
                hx_load_typed(dr, ar, ty);
            }
        }
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_STORE) {
        int ar;
        int vr;
        int pat;
        int lnt;
        pat = bg_sel[idx];
        lnt = (pat >= 0) ? bg_plnt[pat] : -1;
        if (lnt == BG_MEM) {
            /* BURG: store to frame-relative [RBP + disp] */
            int foff;
            foff = bg_foff[h_src1[idx]];
            vr = hx_src(h_src2[idx], X64_RAX);
            hx_store_typed_off(X64_RBP, foff, vr, ty);
        } else if (lnt == BG_SADDR) {
            /* BURG: store to symbol address [+ constant offset] */
            int base_inst;
            int soff;
            base_inst = bg_ssym[h_src1[idx]];
            soff = bg_soff[h_src1[idx]];
            if (base_inst < 0) hx_die("invalid folded SADDR in store", idx);
            hx_mat(base_inst, X64_RCX);
            vr = hx_src(h_src2[idx], X64_RAX);
            hx_store_typed_off(X64_RCX, soff, vr, ty);
        } else {
            /* Fold ADDI displacement into store addressing mode */
            if (hx_addi_folded_flag[idx]) {
                ar = hx_src(h_src1[idx], X64_RAX);
                vr = hx_src(h_src2[idx], X64_RCX);
                hx_store_typed_off(ar, hx_addi_folded_disp[idx], vr, ty);
            } else if (hx_sib_flag[idx]) {
                /* SIB: src1=base, src2=value, index in hx_sib_index */
                int br;
                int ir;
                br = hx_src(h_src1[idx], X64_RAX);
                vr = hx_src(h_src2[idx], X64_RDX);
                ir = hx_src(hx_sib_index[idx], X64_RCX);
                if (wide) {
                    x64_mov_mr64_sib(br, ir, hx_sib_scale[idx], 0, vr);
                } else {
                    x64_mov_mr_sib(br, ir, hx_sib_scale[idx], 0, vr);
                }
            } else {
                ar = hx_src(h_src1[idx], X64_RAX);
                vr = hx_src(h_src2[idx], X64_RCX);
                hx_store_typed(ar, vr, ty);
            }
        }
        return;
    }

    /* --- Control flow --- */

    if (k == HI_BR) {
        int cur_blk;
        cur_blk = h_blk[idx];
        target = h_val[idx];
        hx_phi_copies(cur_blk, target);
        /* Elide jump if target is the next block in layout order */
        if (target != cur_blk + 1) {
            hx_jump_to_block(target);
        }
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
            /* Fused compare-and-branch: emit CMP/TEST + inverted Jcc */
            int ck;
            int s1r;
            int s2r;
            int cmp_wide;
            int ca;
            int cb;

            ck = hx_cmp_kind[cmp_idx];
            ca = h_src1[cmp_idx];
            cb = h_src2[cmp_idx];
            cmp_wide = hx_is_wide(h_ty[ca]) || hx_is_wide(h_ty[cb]);

            /* Check for comparison with zero → TEST */
            if (cb >= 0 && h_kind[cb] == HI_ICONST && h_val[cb] == 0 &&
                (ck == HI_SEQ || ck == HI_SNE)) {
                s1r = hx_src(ca, X64_RAX);
                if (cmp_wide) x64_test_rr64(s1r, s1r);
                else x64_test_rr(s1r, s1r);
            } else if (ca >= 0 && h_kind[ca] == HI_ICONST && h_val[ca] == 0 &&
                       (ck == HI_SEQ || ck == HI_SNE)) {
                s1r = hx_src(cb, X64_RAX);
                if (cmp_wide) x64_test_rr64(s1r, s1r);
                else x64_test_rr(s1r, s1r);
            }
            /* Check for comparison with immediate → CMP r, imm */
            else if (cb >= 0 && h_kind[cb] == HI_ICONST && !cmp_wide) {
                s1r = hx_src(ca, X64_RAX);
                x64_cmp_ri(s1r, h_val[cb]);
            } else {
                s1r = hx_src(ca, X64_RAX);
                s2r = hx_src(cb, X64_RCX);
                if (cmp_wide) x64_cmp_rr64(s1r, s2r);
                else x64_cmp_rr(s1r, s2r);
            }

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
        if (hx_no_frame) {
            /* Frameless: emit ret directly, no epilogue needed */
            x64_ret();
        } else {
            /* Jump to epilog */
            x64_jmp_rel32(0);
            /* Record patch -- epilog offset not known yet */
            hx_bpatch_blk[hx_nbpatch] = -1;  /* special: epilog */
            hx_bpatch_off[hx_nbpatch] = x64_off - 4;
            hx_nbpatch = hx_nbpatch + 1;
        }
        return;
    }

    /* --- Function calls --- */

    /* Builtin: __syscall(nr, a0, a1, a2, a3, a4, a5) → inline syscall */
    if (k == HI_CALL && h_name[idx] && cg_strcmp(h_name[idx], "__syscall") == 0) {
        int sc_regs[7];
        int dr;
        sc_regs[0] = X64_RAX;  sc_regs[1] = X64_RDI;  sc_regs[2] = X64_RSI;
        sc_regs[3] = X64_RDX;  sc_regs[4] = X64_R10;  sc_regs[5] = X64_R8;
        sc_regs[6] = X64_R9;
        nargs = h_val[idx];
        /* Push all args to stack, then load into syscall registers */
        j = 0;
        while (j < nargs) {
            arg_idx = h_carg[h_cbase[idx] + j];
            hx_mat(arg_idx, X64_RAX);
            x64_push(X64_RAX);
            j = j + 1;
        }
        j = 0;
        while (j < nargs && j < 7) {
            x64_mov_rm64(sc_regs[j], X64_RSP, (nargs - 1 - j) * 8);
            j = j + 1;
        }
        if (nargs > 0) x64_add_ri64(X64_RSP, nargs * 8);
        x64_syscall();
        /* Result in RAX */
        dr = hx_dst(idx);
        if (dr != X64_RAX) x64_mov_rr64(dr, X64_RAX);
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_CALL || k == HI_CALLP) {
        int temp_bytes;
        int pad;
        nargs = h_val[idx];

        /* For indirect call, save callee address first.
         * Use hx_src to avoid clobbering allocatable registers. */
        if (k == HI_CALLP) {
            int callee_r;
            callee_r = hx_src(h_src1[idx], X64_RAX);
            x64_push(callee_r);  /* save callee */
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
            /* Callee was pushed before args; recover into RAX and call */
            x64_mov_rm64(X64_RAX, X64_RSP, outgoing + nargs * 8);
            x64_call_r(X64_RAX);
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
     * result is already stored in the CALL instruction's slot.  Load it
     * and shift right by 32 to extract the upper half (SLOW-32 hi word). */
    if (k == HI_CALLHI) {
        int dr;
        hx_mat(h_src1[idx], X64_RAX);  /* load full 64-bit CALL result */
        /* shr rax, 32 — REX.W C1 /5 ib */
        x64_byte(0x48); x64_byte(0xC1); x64_byte(0xE8); x64_byte(32);
        dr = hx_dst(idx);
        hx_mov_if_needed(dr, X64_RAX);
        hx_maybe_spill(idx);
        return;
    }

    /* --- Widening: 32→64 sign/zero extend --- */

    if (k == HI_SEXT32) {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        x64_movsxd(dr, sr);
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_ZEXT32) {
        int sr;
        int dr;
        sr = hx_src(h_src1[idx], X64_RAX);
        dr = hx_dst(idx);
        /* mov r32d, r32d zero-extends to 64-bit */
        x64_mov_rr(dr, sr);
        hx_maybe_spill(idx);
        return;
    }

    /* --- Float ops: stub (not supported yet) --- */
    if (k >= HI_FADD && k <= HI_FCONST) {
        hx_die("floating-point HIR is not supported yet", idx);
    }

    /* --- Varargs --- */

    if (k == HI_VA_START) {
        /* Compute initial tagged pointer for va_list.
         * h_val = number of named parameters
         * Produces the tagged pointer as a value. */
        int dr;
        int nparams_va;
        dr = hx_dst(idx);
        nparams_va = h_val[idx];
        if (nparams_va < 6) {
            x64_lea(dr, X64_RBP, hx_va_save_off + nparams_va * 8);
            x64_or_ri64(dr, 6 - nparams_va);
        } else {
            x64_lea(dr, X64_RBP, 16 + (nparams_va - 6) * 8);
        }
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_VA_ARG) {
        /* Extract argument from tagged va_list pointer.
         * h_src1 = current va_list value (tagged pointer)
         * h_ty   = type of argument to retrieve
         * Produces the argument value. */
        int sr;
        int dr;
        int va_ty;
        int patch_use_stack;
        int patch_after;

        sr = hx_src(h_src1[idx], X64_RAX);
        /* Decode tag: RCX = count, RAX = aligned slot pointer */
        x64_mov_rr64(X64_RAX, sr);
        x64_mov_rr64(X64_RCX, X64_RAX);
        x64_and_ri64(X64_RCX, 7);
        x64_and_ri64(X64_RAX, -8);
        x64_test_rr64(X64_RCX, X64_RCX);
        patch_use_stack = x64_jcc_placeholder(X64_CC_E);

        /* Register path: load from save area slot */
        va_ty = ty;
        if (hx_is_wide(va_ty)) {
            x64_mov_rm64(X64_RDX, X64_RAX, 0);
        } else if (!ty_is_ptr(va_ty) && (va_ty & TY_BASE_MASK) == TY_CHAR) {
            if (va_ty & TY_UNSIGNED) x64_movzx_rm8(X64_RDX, X64_RAX, 0);
            else x64_movsx_rm8(X64_RDX, X64_RAX, 0);
        } else if (!ty_is_ptr(va_ty) && (va_ty & TY_BASE_MASK) == TY_SHORT) {
            if (va_ty & TY_UNSIGNED) x64_movzx_rm16(X64_RDX, X64_RAX, 0);
            else x64_movsx_rm16(X64_RDX, X64_RAX, 0);
        } else {
            x64_mov_rm(X64_RDX, X64_RAX, 0);
        }
        patch_after = x64_jmp_placeholder();

        /* Stack path */
        x64_patch_rel32(patch_use_stack, x64_off);
        if (hx_is_wide(va_ty)) {
            x64_mov_rm64(X64_RDX, X64_RAX, 0);
        } else if (!ty_is_ptr(va_ty) && (va_ty & TY_BASE_MASK) == TY_CHAR) {
            if (va_ty & TY_UNSIGNED) x64_movzx_rm8(X64_RDX, X64_RAX, 0);
            else x64_movsx_rm8(X64_RDX, X64_RAX, 0);
        } else if (!ty_is_ptr(va_ty) && (va_ty & TY_BASE_MASK) == TY_SHORT) {
            if (va_ty & TY_UNSIGNED) x64_movzx_rm16(X64_RDX, X64_RAX, 0);
            else x64_movsx_rm16(X64_RDX, X64_RAX, 0);
        } else {
            x64_mov_rm(X64_RDX, X64_RAX, 0);
        }

        x64_patch_rel32(patch_after, x64_off);
        dr = hx_dst(idx);
        if (dr != X64_RDX) x64_mov_rr64(dr, X64_RDX);
        hx_maybe_spill(idx);
        return;
    }

    if (k == HI_VA_NEXT) {
        /* Advance tagged va_list pointer to next slot.
         * h_src1 = current va_list value (tagged pointer)
         * Produces the updated va_list value. */
        int sr;
        int dr;
        int patch_use_stack;
        int patch_stack_transition;
        int patch_after;

        sr = hx_src(h_src1[idx], X64_RAX);
        /* Decode tag */
        x64_mov_rr64(X64_RAX, sr);
        x64_mov_rr64(X64_RCX, X64_RAX);
        x64_and_ri64(X64_RCX, 7);
        x64_and_ri64(X64_RAX, -8);
        x64_test_rr64(X64_RCX, X64_RCX);
        patch_use_stack = x64_jcc_placeholder(X64_CC_E);

        /* Register path: advance and re-tag */
        x64_add_ri64(X64_RAX, 8);
        x64_sub_ri64(X64_RCX, 1);
        x64_test_rr64(X64_RCX, X64_RCX);
        patch_stack_transition = x64_jcc_placeholder(X64_CC_E);
        x64_or_rr64(X64_RAX, X64_RCX);
        patch_after = x64_jmp_placeholder();

        /* Register→stack transition */
        x64_patch_rel32(patch_stack_transition, x64_off);
        x64_lea(X64_RAX, X64_RBP, 16);
        {
        int patch_after2;
        patch_after2 = x64_jmp_placeholder();

        /* Stack path: just advance */
        x64_patch_rel32(patch_use_stack, x64_off);
        x64_add_ri64(X64_RAX, 8);

        x64_patch_rel32(patch_after2, x64_off);
        }
        x64_patch_rel32(patch_after, x64_off);

        dr = hx_dst(idx);
        if (dr != X64_RAX) x64_mov_rr64(dr, X64_RAX);
        hx_maybe_spill(idx);
        return;
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
    hx_va_save_off = 0;

    /* Align function entry to 16-byte boundary for better icache/branch
     * prediction behavior.  Pad with INT3 (0xCC). */
    while (x64_off & 15) {
        x64_byte(0xCC);
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

    /* --- BURG instruction selection --- */
    hir_burg();

    /* --- Fold single-use ADDIs into load/store addressing modes ---
     * Must happen BEFORE regalloc so the NOPed ADDIs don't consume registers.
     * Only fold ADDIs in the BURG fallback path (not already MEM/SADDR).
     * When folding, rewrite the LOAD/STORE's src1 to point to the ADDI's
     * base register and save the displacement.  This ensures liveness
     * analysis correctly tracks the base register, not the dead ADDI. */
    i = 0;
    while (i < h_ninst) {
        hx_addi_folded[i] = 0;
        hx_addi_folded_flag[i] = 0;
        hx_addi_folded_disp[i] = 0;
        i = i + 1;
    }
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_LOAD || k == HI_STORE) {
            int addr_inst;
            int burg_lnt;
            int bp;
            addr_inst = h_src1[i];
            bp = bg_sel[i];
            burg_lnt = (bp >= 0) ? bg_plnt[bp] : -1;
            if (burg_lnt != BG_MEM && burg_lnt != BG_SADDR) {
                if (addr_inst >= 0 && h_kind[addr_inst] == HI_ADDI &&
                    bg_uses[addr_inst] == 1 && h_blk[addr_inst] == h_blk[i]) {
                    /* Save displacement and rewrite src1 to the ADDI's base.
                     * This ensures liveness tracks the base, not the dead ADDI. */
                    hx_addi_folded_flag[i] = 1;
                    hx_addi_folded_disp[i] = h_val[addr_inst];
                    h_src1[i] = h_src1[addr_inst];
                    hx_addi_folded[addr_inst] = 1;
                    h_kind[addr_inst] = HI_NOP;
                }
            }
        }
        i = i + 1;
    }

    /* --- Fold scaled-index addressing (SIB) into LOAD/STORE ---
     * Pattern: LOAD [ADD(base, SLL(index, 1..3))]
     *    or    STORE [ADD(base, SLL(index, 1..3))], value
     * For LOAD:  rewrite src1=base, src2=index (newly tracked for liveness).
     * For STORE: rewrite src1=base; index saved in hx_sib_index[].
     * The ADD and SLL are NOPed so they don't consume registers. */
    i = 0;
    while (i < h_ninst) {
        hx_sib_flag[i] = 0;
        hx_sib_scale[i] = 0;
        hx_sib_index[i] = -1;
        i = i + 1;
    }
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if ((k == HI_LOAD || k == HI_STORE) &&
            !hx_addi_folded_flag[i]) {
            int addr;
            int burg_lnt;
            int bp;
            addr = h_src1[i];
            bp = bg_sel[i];
            burg_lnt = (bp >= 0) ? bg_plnt[bp] : -1;
            if (burg_lnt != BG_MEM && burg_lnt != BG_SADDR &&
                addr >= 0 && h_kind[addr] == HI_ADD &&
                hx_is_wide(h_ty[addr]) &&
                bg_uses[addr] == 1 && h_blk[addr] == h_blk[i]) {
                int lhs;
                int rhs;
                int base_i;
                int idx_i;
                int sll_i;
                int shift;
                lhs = h_src1[addr];
                rhs = h_src2[addr];
                base_i = -1; idx_i = -1; sll_i = -1; shift = -1;
                /* ADD(base, SLL(index, const)) */
                if (rhs >= 0 && h_kind[rhs] == HI_SLL &&
                    bg_uses[rhs] == 1 && h_blk[rhs] == h_blk[i]) {
                    int sc;
                    sc = h_src2[rhs];
                    if (sc >= 0 && h_kind[sc] == HI_ICONST &&
                        h_val[sc] >= 1 && h_val[sc] <= 3) {
                        base_i = lhs;
                        idx_i = h_src1[rhs];
                        sll_i = rhs;
                        shift = h_val[sc];
                    }
                }
                /* ADD(SLL(index, const), base) — commutative */
                if (base_i < 0 && lhs >= 0 && h_kind[lhs] == HI_SLL &&
                    bg_uses[lhs] == 1 && h_blk[lhs] == h_blk[i]) {
                    int sc;
                    sc = h_src2[lhs];
                    if (sc >= 0 && h_kind[sc] == HI_ICONST &&
                        h_val[sc] >= 1 && h_val[sc] <= 3) {
                        base_i = rhs;
                        idx_i = h_src1[lhs];
                        sll_i = lhs;
                        shift = h_val[sc];
                    }
                }
                if (base_i >= 0 && idx_i >= 0 && sll_i >= 0) {
                    hx_sib_flag[i] = 1;
                    hx_sib_scale[i] = shift;
                    /* Rewrite LOAD: src1=base, src2=index */
                    h_src1[i] = base_i;
                    if (k == HI_LOAD) {
                        h_src2[i] = idx_i;
                    } else {
                        /* STORE: src2 stays as the value to store.
                         * Index tracked via side array. */
                        hx_sib_index[i] = idx_i;
                    }
                    /* NOP the consumed ADD and SLL */
                    h_kind[addr] = HI_NOP;
                    h_kind[sll_i] = HI_NOP;
                }
            }
        }
        i = i + 1;
    }

    /* --- Register allocation ---
     * Start spill allocation below the deepest lowered alloca, not just the
     * parser's locals_size.  Lowering can introduce extra HI_ALLOCA temps
     * after parsing, and those already occupy fixed frame slots once scaled
     * through cg_x64_offset().  If we ignore them, spill slots can alias
     * allocas inside large functions like predecode(). */
    hl_temp_stack = hx_hir_frame_base(fn);
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
    int param_exists[6];
    /* Record which physical param slots actually carry a value */
    pi = 0;
    while (pi < 6) { param_exists[pi] = 0; pi = pi + 1; }
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PARAM && h_val[i] >= 0 && h_val[i] < 6)
            param_exists[h_val[i]] = 1;
        i = i + 1;
    }
    /* Check for conflict: PARAM allocated to a register that's the arg
     * register of a DIFFERENT param that actually exists. */
    i = 0;
    while (i < h_ninst && !hx_param_need_temp) {
        if (h_kind[i] == HI_PARAM && ra_reg[i] >= 0) {
            pidx = h_val[i];
            if (pidx < 6) {
                pi = 0;
                while (pi < 6 && !hx_param_need_temp) {
                    if (hx_arg_reg[pi] == ra_reg[i] && pi != pidx && param_exists[pi])
                        hx_param_need_temp = 1;
                    pi = pi + 1;
                }
            }
        }
        i = i + 1;
    }
    if (hx_param_need_temp) {
        /* Demote all register-allocated PARAMs to spilled.  The regalloc
         * may assign the same register to multiple PARAMs (their live
         * ranges don't overlap after SSA), but the prologue loads them
         * simultaneously from the temp area, causing clobbers. */
        i = 0;
        while (i < h_ninst) {
            if (h_kind[i] == HI_PARAM && ra_reg[i] >= 0 && ra_spill_off[i] == 0) {
                hl_temp_stack = hl_temp_stack + 8;
                ra_spill_off[i] = 0 - hl_temp_stack;
                ra_reg[i] = -1;
            }
            i = i + 1;
        }
        hl_temp_stack = hl_temp_stack + 48;  /* 6 * 8 bytes for arg save area */
        hx_param_temp_base = 0 - hl_temp_stack;
    }
    }

    /* --- Varargs save area (48 bytes for 6 register args) --- */
    if (hx_is_varargs) {
        hl_temp_stack = hl_temp_stack + 48;
        hx_va_save_off = 0 - hl_temp_stack;
    }

    /* --- Compute frame size --- */
    fs = hl_temp_stack;
    fs = (fs + 15) & ~15;  /* 16-byte align */
    hx_frame_size = fs;

    /* --- No-frame optimization: skip prologue/epilogue if possible --- */
    hx_no_frame = 0;
    if (fs == 0 && ra_ncsave == 0 && !hx_is_varargs && !hx_param_need_temp) {
        hx_no_frame = 1;
    }

    /* --- Init block offsets --- */
    hx_nbpatch = 0;
    b = 0;
    while (b < bb_nblk) {
        hx_blk_off[b] = -1;
        b = b + 1;
    }

    /* --- Prologue --- */
    if (!hx_no_frame) {
        x64_push(X64_RBP);
        x64_mov_rr64(X64_RBP, X64_RSP);
        if (fs > 0) x64_sub_ri64(X64_RSP, fs);
    }

    /* Save callee-saved registers used by regalloc */
    i = 0;
    while (i < ra_ncsave) {
        x64_mov_mr64(X64_RBP, ra_csave_off[i], ra_csave_reg[i]);
        i = i + 1;
    }

    /* For varargs functions, save all 6 register args to the save area.
     * va_start/va_arg will read from this area. */
    if (hx_is_varargs) {
        i = 0;
        while (i < 6) {
            x64_mov_mr64(X64_RBP, hx_va_save_off + i * 8, hx_arg_reg[i]);
            i = i + 1;
        }
    }

    /* Store incoming args to their allocated register or spill slot. */
    if (hx_param_need_temp) {
        /* Two-phase: save all 6 arg registers to temp area, then
         * distribute to spill slots.  We CANNOT load directly into
         * allocated registers here because the regalloc may assign the
         * same register to multiple PARAMs (short-lived: define → store).
         * Loading the second would clobber the first.  All PARAMs were
         * demoted to spilled during the need_temp detection phase. */
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
                if (ra_spill_off[i] != 0) {
                    hx_param_to_spill(ra_spill_off[i], i, pidx, 1);
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
                    hx_param_to_reg(ra_reg[i], i, pidx, 0);
                } else if (ra_spill_off[i] != 0) {
                    hx_param_to_spill(ra_spill_off[i], i, pidx, 0);
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

    if (hx_no_frame) {
        /* No frame: just RET */
        x64_ret();
    } else {
        /* Restore callee-saved registers */
        i = 0;
        while (i < ra_ncsave) {
            x64_mov_rm64(ra_csave_reg[i], X64_RBP, ra_csave_off[i]);
            i = i + 1;
        }

        x64_mov_rr64(X64_RSP, X64_RBP);
        x64_pop(X64_RBP);
        x64_ret();
    }

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
