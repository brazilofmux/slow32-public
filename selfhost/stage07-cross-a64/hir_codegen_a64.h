/* hir_codegen_a64.h -- HIR to AArch64 code generator with regalloc.
 *
 * Walks HIR blocks in RPO order, dispatches on h_kind[], emits AArch64
 * via a64_encode.h.  Uses IRC graph-coloring regalloc (hir_regalloc_a64.h)
 * to assign physical registers; spills go to dedicated frame slots.
 *
 * MVP scope (this commit):
 *   - All integer HIR ops (ADD/SUB/MUL/DIV/REM/AND/OR/XOR/shifts/cmps/etc.)
 *   - LOAD/STORE with frame, symbol, or register addressing (no SIB folding)
 *   - BR / BRC / RET / CALL / CALLP / PHI / COPY
 *   - ALLOCA, GADDR, SADDR, FADDR, ADDI, GETFP
 *   - SEXT32 / ZEXT32 widening
 *   - PARAM (with up to 8 register args; stack args via pre-load)
 *   - __syscall builtin
 *
 * Deferred for later:
 *   - SIB / ALU+SIB / ADDI / RMW peephole folds
 *   - Compare-branch fusion (CMP+B.cond merging)
 *   - 64-bit ops with carry
 *   - Floating point
 *   - Varargs
 *
 * Requires (in order):
 *   a64_encode.h, a64_reloc_kinds.h, elf_writer.h, codegen_a64.h,
 *   hir.h + hir_lower.h + hir_ssa.h + hir_opt.h + hir_licm.h +
 *   hir_burg_a64.h, hir_regalloc_a64.h
 */

#ifndef HIR_CODEGEN_A64_H
#define HIR_CODEGEN_A64_H

/* Forward declarations (BURG and licm provide these). */
static void hir_burg(void);
static void hir_licm(void);
extern int licm_stat_hoisted;
extern int licm_head[HIR_MAX_BLOCK];
extern int licm_next[HIR_MAX_INST];
/* hir_regalloc.h provides hir_regalloc(void). */
static void hir_regalloc(void);

/* ============================================================================
 * Per-function state
 * ============================================================================ */

static int hx_frame_size;
static int hx_next_blk;          /* RPO-next block; -1 if none (used to elide degenerate `b next_blk`) */
static int hx_no_frame;
static int hx_is_varargs;
static int hx_fn_nparams;
static int hx_va_save_off;          /* unused for MVP */

/* Block-level patching */
static int hx_blk_off[HIR_MAX_BLOCK];

/* Forward branches: site offset, target block.  When we lay down the
 * target's first instruction we walk the patch list. */
#define HX_MAX_BPATCH 8192
static int hx_bpatch_blk[HX_MAX_BPATCH];   /* -1 = epilogue (RET target) */
static int hx_bpatch_off[HX_MAX_BPATCH];
static int hx_nbpatch;

/* AAPCS64 arg registers (8 regs). */
static int hx_arg_reg[8];

/* Scratch register pair (always available; not allocatable in regalloc).
 * X16 is "primary scratch" — used by hx_mat / hx_spill / address temps.
 * X17 is "secondary scratch" — used when the codegen needs a 2nd temp
 * (e.g., loading two spilled operands at once).  Mirrors AArch64's
 * AAPCS64 IP0/IP1 convention. */
#define HX_SCRATCH1  A64_X16
#define HX_SCRATCH2  A64_X17

/* V-class scratch pair.  V30 and V31 are excluded from the regalloc's
 * allocatable pool (RA_NPHY_V=22 covers V0..V7 + V16..V29). */
#define HX_SCRATCH_V1  A64_V30
#define HX_SCRATCH_V2  A64_V31

/* PHI move scheduling buffers (used by hx_phi_copies). */
static int hx_phi_dst_inst[HIR_MAX_INST];   /* PHI inst */
static int hx_phi_src_val[HIR_MAX_INST];    /* incoming value inst */
static int hx_phi_n;

/* Indexed-addressing fold (computed by hx_fold_indexed_addr).
 * For an HI_LOAD/HI_STORE that we want to emit as
 *     ldr/str Wt/Xt, [base, idx, lsl #shift]
 * we record the base inst, idx inst, and shift amount keyed on the
 * load/store inst.  The HI_ADD and HI_SLL feeding it get bg_fold[]=1
 * so their def sites emit nothing. */
static int hx_ridx_base[HIR_MAX_INST];   /* base reg inst, -1 if not folded */
static int hx_ridx_idx[HIR_MAX_INST];    /* index reg inst */
static int hx_ridx_shift[HIR_MAX_INST];  /* shift amount (0, 2, or 3) */

/* Offset-displacement fold: HI_LOAD/HI_STORE whose address is
 * HI_ADDI(reg_base, const) gets emitted as
 *     ldr/str Wt/Xt, [base, #disp]
 * and the HI_ADDI def is suppressed when all real ADDI uses fold this
 * way.  Mutually exclusive with indexed addressing on the same LOAD/STORE. */
static int hx_off_base[HIR_MAX_INST];    /* base inst, -1 if not folded */
static int hx_off_disp[HIR_MAX_INST];    /* byte displacement */
static int hx_off_use[HIR_MAX_INST];     /* post-fusion use count */
static int hx_off_fold_use[HIR_MAX_INST];/* candidate folded LOAD/STORE uses */
static int hx_pre_off_base[HIR_MAX_INST];/* pre-RA single-use fold base */
static int hx_pre_off_disp[HIR_MAX_INST];/* pre-RA single-use fold disp */

/* Tail-call elimination: hx_tce_call[c]==1 marks an HI_CALL that is the
 * last meaningful instruction in its block before HI_RET; emit `b target`
 * instead of `bl + epilogue + ret`.  hx_tce_ret[r]==1 marks the trailing
 * HI_RET so its emitter is skipped (the TCE call already branched away).
 * See hx_identify_tce. */
static int hx_tce_call[HIR_MAX_INST];
static int hx_tce_ret[HIR_MAX_INST];

/* LDP/STP lookahead pairing — see hx_find_load_partner / hx_find_store_partner.
 * When a HI_LOAD/HI_STORE that hits the offset-disp fold path finds a same-base
 * partner later in the same block, codegen emits LDP/STP at the first inst's
 * position and sets hx_skip_emit on the partner so the dispatch loop skips
 * it.  Set in hx_emit_inst's HI_LOAD/HI_STORE branches; cleared per-function. */
static int hx_skip_emit[HIR_MAX_INST];

/* Right-end (exclusive) of the block currently being emitted.  Used by the
 * pair-search lookaheads to bound forward scans.  Set in the codegen
 * dispatch loop in hx_gen_func before each block. */
static int hx_cur_blk_end;

/* ============================================================================
 * Helpers
 * ============================================================================ */

static int hx_is_wide(int ty) { return ty_is_ptr(ty) || ty_is_llong(ty); }

static void hx_die(char *msg, int idx) {
    fdputs("cc-a64 --hir: ", 2);
    fdputs(msg, 2);
    if (idx >= 0) { fdputs(" at HIR inst ", 2); fdputuint(2, idx); }
    fdputs("\n", 2);
    exit(1);
}

/* ra_reg[inst] already stores an AArch64 register encoding (writeback does
 * `ra_reg[inst] = ra_a64_phys[color]`).  This used to do another phys lookup
 * which corrupted assignments — now it's an identity, kept for symmetry with
 * the x64 sibling's call sites. */
static int hx_slot_reg(int reg_enc) {
    return reg_enc;
}

/* The physical register holding the value of HIR instruction `inst`,
 * after any necessary materialisation/reload.  Returns the register
 * encoding (X-reg number 0..30).  May write to scratch1 (and scratch2
 * if `which` is 1, used for the second source operand). */
static void hx_emit_load_to_reg(int dst_reg, int spill_off, int wide);
static void hx_emit_store_from_reg(int src_reg, int spill_off, int wide);

/* ============================================================================
 * Materialise a value into a given register.
 *   - If the value lives in a register that equals `reg`, no-op.
 *   - If it lives in a different register, emit MOV.
 *   - If it's spilled, emit LDR from spill slot.
 *   - If it's rematerialisable (ICONST / ALLOCA / GADDR / SADDR / GETFP),
 *     re-emit the constant / address.
 * ============================================================================ */

static void hx_mat_iconst(int dst_reg, int val, int wide) {
    if (wide) {
        /* For 64-bit constants we'd need lo+hi; HIR ICONST is 32-bit
         * for the values we emit, so just sign-extend low 32. */
        a64_mov_w_imm(dst_reg, val);
        a64_sxtw(dst_reg, dst_reg);
        return;
    }
    a64_mov_w_imm(dst_reg, val);
}

static int hx_emit_add_delta(int dst_reg, int src_reg, long long delta, int wide) {
    long long mag;
    if (delta >= 0) {
        if (delta <= 4095) {
            if (wide) a64_add_x_imm(dst_reg, src_reg, (int)delta);
            else      a64_add_w_imm(dst_reg, src_reg, (int)delta);
            return 1;
        }
        if ((delta & 4095) == 0 && delta <= (4095LL << 12)) {
            if (wide) a64_add_x_imm_lsl12(dst_reg, src_reg, (int)(delta >> 12));
            else      a64_add_w_imm_lsl12(dst_reg, src_reg, (int)(delta >> 12));
            return 1;
        }
        return 0;
    }

    mag = 0 - delta;
    if (mag <= 4095) {
        if (wide) a64_sub_x_imm(dst_reg, src_reg, (int)mag);
        else      a64_sub_w_imm(dst_reg, src_reg, (int)mag);
        return 1;
    }
    if ((mag & 4095) == 0 && mag <= (4095LL << 12)) {
        if (wide) a64_sub_x_imm_lsl12(dst_reg, src_reg, (int)(mag >> 12));
        else      a64_sub_w_imm_lsl12(dst_reg, src_reg, (int)(mag >> 12));
        return 1;
    }
    return 0;
}

/* Compute a frame address (FP + offset) into dst_reg.
 * `frontend_off` is in slow32 4-byte units; cg_a64_offset doubles it. */
static void hx_emit_frame_addr(int dst_reg, int frontend_off) {
    int off;
    off = cg_a64_offset(frontend_off);
    if (hx_emit_add_delta(dst_reg, A64_X29, off, 1)) return;
    if (off > 0) {
        a64_add_x_imm(dst_reg, A64_X29, off & 0xFFF);
        a64_add_x_imm_lsl12(dst_reg, dst_reg, (off >> 12) & 0xFFF);
        return;
    }
    {
        int neg;
        neg = -off;
        a64_sub_x_imm(dst_reg, A64_X29, neg & 0xFFF);
        a64_sub_x_imm_lsl12(dst_reg, dst_reg, (neg >> 12) & 0xFFF);
    }
}

/* Same, but the offset is already a byte offset (no doubling). */
static void hx_emit_frame_addr_bytes(int dst_reg, int off) {
    if (hx_emit_add_delta(dst_reg, A64_X29, off, 1)) return;
    if (off > 0) {
        a64_add_x_imm(dst_reg, A64_X29, off & 0xFFF);
        a64_add_x_imm_lsl12(dst_reg, dst_reg, (off >> 12) & 0xFFF);
        return;
    }
    {
        int neg;
        neg = -off;
        a64_sub_x_imm(dst_reg, A64_X29, neg & 0xFFF);
        a64_sub_x_imm_lsl12(dst_reg, dst_reg, (neg >> 12) & 0xFFF);
    }
}

/* Materialise a 32-bit float bit pattern into a V register.  Tries the
 * FMOV-imm8 fast path (256 encodable patterns including 1.0, 2.0, 0.5,
 * etc.); else does the fallback `mov w_scratch, #bits; fmov s_dst, w`. */
static void hx_mat_fconst_s(int dst_v, unsigned int bits) {
    int imm8;
    if (a64_encode_fmov_s_imm(bits, &imm8)) {
        a64_fmov_s_imm(dst_v, imm8);
        return;
    }
    a64_mov_w_imm(HX_SCRATCH1, (int)bits);
    a64_fmov_s_w(dst_v, HX_SCRATCH1);
}

/* Materialise a remat-able value (ICONST, ALLOCA, GADDR, SADDR, FADDR,
 * GETFP) into dst_reg without consulting ra_reg[]. */
static void hx_remat(int inst, int dst_reg) {
    int k;
    k = h_kind[inst];
    if (k == HI_ICONST) {
        if (ty_is_float(h_ty[inst])) {
            hx_mat_fconst_s(dst_reg, (unsigned int)h_val[inst]);
        } else {
            hx_mat_iconst(dst_reg, h_val[inst], hx_is_wide(h_ty[inst]));
        }
        return;
    }
    if (k == HI_ALLOCA) {
        hx_emit_frame_addr(dst_reg, h_val[inst]);
        return;
    }
    if (k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) {
        if (k == HI_SADDR) {
            /* String literal — h_val is the lexer string-pool index. */
            cg_emit_adrp_add_to(dst_reg, CG_STR_PSEUDO_NAME, h_val[inst]);
        } else {
            /* GADDR / FADDR → emit ADRP+ADD against the named symbol. */
            cg_emit_adrp_add_to(dst_reg, h_name[inst], 0);
        }
        return;
    }
    if (k == HI_GETFP) {
        a64_mov_x(dst_reg, A64_X29);
        return;
    }
    fdputs("cc-a64 --hir: hx_remat unhandled kind=", 2);
    fdputuint(2, k);
    fdputs(" at inst ", 2);
    fdputuint(2, inst);
    fdputs(" ra_reg=", 2);
    fdputuint(2, ra_reg[inst]);
    fdputs(" ra_spill=", 2);
    fdputuint(2, ra_spill_off[inst]);
    fdputs("\n", 2);
    exit(1);
}

/* Class predicate — true if inst's regalloc class is V (FP/SIMD). */
static int hx_is_v(int inst) {
    if (inst < 0) return 0;
    return ra_class_of(inst) == RA_CLASS_V;
}

/* Forward decl: FP frame load/store (defined further down). */
static void hx_emit_fp_load_to_reg(int dst_reg, int spill_off, int is_double);
static void hx_emit_fp_store_from_reg(int src_reg, int spill_off, int is_double);

/* Get the value of HIR instruction `inst` into dst_reg.
 *
 * The dst_reg's class must match inst's class — caller's responsibility
 * (X-class consumers pass an X reg; V-class consumers pass a V reg).
 * If the value is already in dst_reg, no instruction is emitted.
 * Otherwise: MOV (or FMOV) from assigned reg, LDR (or LDR S/D) from
 * spill slot, or remat (X-class only — HI_FCONST remat lands in a
 * later session). */
static void hx_mat(int inst, int dst_reg) {
    int slot;
    int src_reg;
    int wide;

    if (inst < 0) hx_die("hx_mat: bad inst", inst);

    /* If there's an assigned reg, MOV from it. */
    slot = ra_reg[inst];
    if (slot >= 0) {
        src_reg = hx_slot_reg(slot);
        if (src_reg == dst_reg) return;
        if (hx_is_v(inst)) {
            if (ty_is_double(h_ty[inst])) a64_fmov_d_d(dst_reg, src_reg);
            else                          a64_fmov_s_s(dst_reg, src_reg);
        } else {
            /* Always use the wide form — it preserves both 32 and 64 bit values.
             * Since AArch64 X-form move zero-extends the upper half implicitly
             * for W-form, MOV X is always safe. */
            a64_mov_x(dst_reg, src_reg);
        }
        return;
    }

    /* Spilled?  Load from frame. */
    if (ra_spill_off[inst] != 0) {
        if (hx_is_v(inst)) {
            hx_emit_fp_load_to_reg(dst_reg, ra_spill_off[inst],
                                   ty_is_double(h_ty[inst]));
        } else {
            wide = hx_is_wide(h_ty[inst]);
            hx_emit_load_to_reg(dst_reg, ra_spill_off[inst], wide);
        }
        return;
    }

    /* Rematerialise.  V-class remat handles HI_ICONST with TY_FLOAT
     * (the lower lowers `3.0f` to HI_ICONST + TY_FLOAT + bit pattern). */
    hx_remat(inst, dst_reg);
}

/* If a value is not yet in a register, return scratch with the value
 * loaded in.  Otherwise return its assigned reg.  `scratch` must be
 * class-appropriate for `inst` (X scratch for X-class, V scratch for
 * V-class) — caller's responsibility. */
static int hx_get_src(int inst, int scratch) {
    int slot;
    if (inst < 0) hx_die("hx_get_src: -1 src", inst);
    slot = ra_reg[inst];
    if (slot >= 0) return hx_slot_reg(slot);
    hx_mat(inst, scratch);
    return scratch;
}

/* If `inst` was spilled, store dst_reg back to its spill slot. */
static void hx_spill(int inst, int reg) {
    int wide;
    if (ra_reg[inst] >= 0) return;          /* not spilled */
    if (ra_spill_off[inst] == 0) return;    /* no value to spill (e.g., remat) */
    if (hx_is_v(inst)) {
        hx_emit_fp_store_from_reg(reg, ra_spill_off[inst],
                                  ty_is_double(h_ty[inst]));
        return;
    }
    wide = hx_is_wide(h_ty[inst]);
    hx_emit_store_from_reg(reg, ra_spill_off[inst], wide);
}

/* Where does dst_reg need to write to?  If the value is assigned a reg,
 * that's the destination.  If it's spilled, we need a scratch + spill —
 * pick a class-appropriate scratch. */
static int hx_dst_reg(int inst) {
    int slot;
    slot = ra_reg[inst];
    if (slot >= 0) return hx_slot_reg(slot);
    if (hx_is_v(inst)) return HX_SCRATCH_V1;
    if (ra_spill_off[inst] != 0) return HX_SCRATCH1;
    /* Value isn't used by anything that matters (rare edge case). */
    return HX_SCRATCH1;
}

/* If `inst` is HI_ICONST or *EXT32(HI_ICONST), unwrap and return 1, writing
 * the underlying 32-bit value to *vout and the X-form 64-bit semantic to
 * *llvout (sign-extended for HI_ICONST/HI_SEXT32, zero-extended for
 * HI_ZEXT32).  Used by the binary-op fast paths to see through the
 * widening node the lower inserts for 64-bit ops fed by a 32-bit literal. */
static int hx_peek_iconst(int inst, int *vout, long long *llvout) {
    int k;
    int v;
    if (inst < 0) return 0;
    k = h_kind[inst];
    if (k == HI_ICONST) {
        v = h_val[inst];
        *vout = v;
        *llvout = (long long)v;
        return 1;
    }
    if ((k == HI_SEXT32 || k == HI_ZEXT32) && h_src1[inst] >= 0
        && h_kind[h_src1[inst]] == HI_ICONST) {
        v = h_val[h_src1[inst]];
        *vout = v;
        if (k == HI_ZEXT32)
            *llvout = (long long)(unsigned long long)(unsigned int)v;
        else
            *llvout = (long long)v;
        return 1;
    }
    return 0;
}

/* Helper to load/store via scratch when the offset doesn't fit in imm12. */
static void hx_emit_load_to_reg(int dst_reg, int spill_off, int wide) {
    /* spill_off is a frontend offset (negative).  Compute absolute frame
     * offset and emit an LDR. */
    int off;
    off = cg_a64_offset(spill_off);
    /* Positive scaled-offset form (LDR Wt/Xt, [Xn, #imm12*scale]). */
    if (off >= 0 && off <= 32760 && (off & (wide ? 7 : 3)) == 0) {
        if (wide) a64_ldr_x_imm(dst_reg, A64_X29, off);
        else      a64_ldr_w_imm(dst_reg, A64_X29, off);
        return;
    }
    /* Unscaled signed-offset form (LDUR Wt/Xt, [Xn, #imm9]) — covers
     * spills at FP-N for N in [1, 256]. */
    if (off >= -256 && off <= 255) {
        if (wide) a64_ldur_x_imm(dst_reg, A64_X29, off);
        else      a64_ldur_w_imm(dst_reg, A64_X29, off);
        return;
    }
    /* General path: compute address into scratch2, then load. */
    {
        int neg;
        if (off < 0) {
            neg = -off;
            if (neg <= 4095) {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg);
            } else {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg & 0xFFF);
                a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF);
            }
        } else {
            if (off <= 4095) {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off);
            } else {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (off >> 12) & 0xFFF);
            }
        }
        if (wide) a64_ldr_x_imm(dst_reg, HX_SCRATCH2, 0);
        else      a64_ldr_w_imm(dst_reg, HX_SCRATCH2, 0);
    }
}

static void hx_emit_store_from_reg(int src_reg, int spill_off, int wide) {
    int off;
    off = cg_a64_offset(spill_off);
    if (off >= 0 && off <= 32760 && (off & (wide ? 7 : 3)) == 0) {
        if (wide) a64_str_x_imm(src_reg, A64_X29, off);
        else      a64_str_w_imm(src_reg, A64_X29, off);
        return;
    }
    if (off >= -256 && off <= 255) {
        if (wide) a64_stur_x_imm(src_reg, A64_X29, off);
        else      a64_stur_w_imm(src_reg, A64_X29, off);
        return;
    }
    {
        int neg;
        if (off < 0) {
            neg = -off;
            if (neg <= 4095) {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg);
            } else {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg & 0xFFF);
                a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF);
            }
        } else {
            if (off <= 4095) {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off);
            } else {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (off >> 12) & 0xFFF);
            }
        }
        if (wide) a64_str_x_imm(src_reg, HX_SCRATCH2, 0);
        else      a64_str_w_imm(src_reg, HX_SCRATCH2, 0);
    }
}

/* FP load/store via address reg + byte offset.  Mirrors the int-form
 * dispatch (scaled imm12 → unscaled imm9 → compute address into
 * scratch).  Caller passes a V-class dst/src and an X-class addr_reg.
 * Used by HI_LOAD/HI_STORE when h_ty is float/double — pre-Session 7
 * those would have emitted X-form ldr/str into a V dst encoding,
 * which is the wrong physical reg file. */
static void hx_fp_load_at(int dst_v, int addr_reg, int byte_off, int is_double) {
    if (byte_off >= 0 && byte_off <= 32760 && (byte_off & (is_double ? 7 : 3)) == 0) {
        if (is_double) a64_ldr_d_imm(dst_v, addr_reg, byte_off);
        else           a64_ldr_s_imm(dst_v, addr_reg, byte_off);
        return;
    }
    if (byte_off >= -256 && byte_off <= 255) {
        if (is_double) a64_ldur_d_imm(dst_v, addr_reg, byte_off);
        else           a64_ldur_s_imm(dst_v, addr_reg, byte_off);
        return;
    }
    /* Out of range: bake offset into HX_SCRATCH2 and load at zero. */
    if (byte_off > 0) {
        if (byte_off <= 4095) a64_add_x_imm(HX_SCRATCH2, addr_reg, byte_off);
        else { a64_add_x_imm(HX_SCRATCH2, addr_reg, byte_off & 0xFFF);
               a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (byte_off >> 12) & 0xFFF); }
    } else {
        int neg;
        neg = -byte_off;
        if (neg <= 4095) a64_sub_x_imm(HX_SCRATCH2, addr_reg, neg);
        else { a64_sub_x_imm(HX_SCRATCH2, addr_reg, neg & 0xFFF);
               a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF); }
    }
    if (is_double) a64_ldr_d_imm(dst_v, HX_SCRATCH2, 0);
    else           a64_ldr_s_imm(dst_v, HX_SCRATCH2, 0);
}

static void hx_fp_store_at(int src_v, int addr_reg, int byte_off, int is_double) {
    if (byte_off >= 0 && byte_off <= 32760 && (byte_off & (is_double ? 7 : 3)) == 0) {
        if (is_double) a64_str_d_imm(src_v, addr_reg, byte_off);
        else           a64_str_s_imm(src_v, addr_reg, byte_off);
        return;
    }
    if (byte_off >= -256 && byte_off <= 255) {
        if (is_double) a64_stur_d_imm(src_v, addr_reg, byte_off);
        else           a64_stur_s_imm(src_v, addr_reg, byte_off);
        return;
    }
    if (byte_off > 0) {
        if (byte_off <= 4095) a64_add_x_imm(HX_SCRATCH2, addr_reg, byte_off);
        else { a64_add_x_imm(HX_SCRATCH2, addr_reg, byte_off & 0xFFF);
               a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (byte_off >> 12) & 0xFFF); }
    } else {
        int neg;
        neg = -byte_off;
        if (neg <= 4095) a64_sub_x_imm(HX_SCRATCH2, addr_reg, neg);
        else { a64_sub_x_imm(HX_SCRATCH2, addr_reg, neg & 0xFFF);
               a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF); }
    }
    if (is_double) a64_str_d_imm(src_v, HX_SCRATCH2, 0);
    else           a64_str_s_imm(src_v, HX_SCRATCH2, 0);
}

static int hx_access_size_for_ty(int ty) {
    if (ty_is_double(ty)) return 8;
    if (hx_is_wide(ty)) return 8;
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) return 1;
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) return 2;
    return 4;
}

/* Positive scaled imm12 form: `ldr/str` with `[base, #imm*scale]` for
 * `imm` in [0, 4095].  Requires `disp` to be a non-negative multiple of
 * `access_size` within range. */
static int hx_offset_fits_scaled_imm12(int disp, int access_size) {
    int max_disp;
    max_disp = access_size * 4095;
    return disp >= 0 && disp <= max_disp
        && (disp & (access_size - 1)) == 0;
}

/* Unscaled signed imm9 form: `ldur/stur` with `[base, #imm]` for `imm`
 * in [-256, 255].  No alignment requirement. */
static int hx_offset_fits_imm9(int disp) {
    return disp >= -256 && disp <= 255;
}

/* True when an offset-displacement fold is representable in either of
 * the two AArch64 imm forms.  Useful as a fold-eligibility predicate;
 * emitters still pick a specific form by sign/alignment. */
static int hx_offset_fold_encodable(int disp, int access_size) {
    return hx_offset_fits_scaled_imm12(disp, access_size)
        || hx_offset_fits_imm9(disp);
}

static void hx_int_load_at(int dst_reg, int addr_reg, int byte_off, int ty) {
    if (hx_offset_fits_scaled_imm12(byte_off, hx_access_size_for_ty(ty))) {
        if (hx_is_wide(ty)) a64_ldr_x_imm(dst_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
            if (ty & TY_UNSIGNED) a64_ldrb_imm(dst_reg, addr_reg, byte_off);
            else                  a64_ldrsb_w_imm(dst_reg, addr_reg, byte_off);
        } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
            if (ty & TY_UNSIGNED) a64_ldrh_imm(dst_reg, addr_reg, byte_off);
            else                  a64_ldrsh_w_imm(dst_reg, addr_reg, byte_off);
        } else {
            a64_ldr_w_imm(dst_reg, addr_reg, byte_off);
        }
        return;
    }
    if (hx_offset_fits_imm9(byte_off)) {
        if (hx_is_wide(ty)) a64_ldur_x_imm(dst_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
            if (ty & TY_UNSIGNED) a64_ldurb_imm(dst_reg, addr_reg, byte_off);
            else                  a64_ldursb_w_imm(dst_reg, addr_reg, byte_off);
        } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
            if (ty & TY_UNSIGNED) a64_ldurh_imm(dst_reg, addr_reg, byte_off);
            else                  a64_ldursh_w_imm(dst_reg, addr_reg, byte_off);
        } else {
            a64_ldur_w_imm(dst_reg, addr_reg, byte_off);
        }
        return;
    }
    if (!hx_emit_add_delta(HX_SCRATCH2, addr_reg, byte_off, 1)) {
        hx_mat_iconst(HX_SCRATCH2, byte_off, 1);
        a64_add_x(HX_SCRATCH2, addr_reg, HX_SCRATCH2);
    }
    if (hx_is_wide(ty)) a64_ldr_x_imm(dst_reg, HX_SCRATCH2, 0);
    else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) a64_ldrb_imm(dst_reg, HX_SCRATCH2, 0);
        else                  a64_ldrsb_w_imm(dst_reg, HX_SCRATCH2, 0);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) a64_ldrh_imm(dst_reg, HX_SCRATCH2, 0);
        else                  a64_ldrsh_w_imm(dst_reg, HX_SCRATCH2, 0);
    } else {
        a64_ldr_w_imm(dst_reg, HX_SCRATCH2, 0);
    }
}

static void hx_int_store_at(int src_reg, int addr_reg, int byte_off, int ty) {
    if (hx_offset_fits_scaled_imm12(byte_off, hx_access_size_for_ty(ty))) {
        if (hx_is_wide(ty)) a64_str_x_imm(src_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
            a64_strb_imm(src_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
            a64_strh_imm(src_reg, addr_reg, byte_off);
        else
            a64_str_w_imm(src_reg, addr_reg, byte_off);
        return;
    }
    if (hx_offset_fits_imm9(byte_off)) {
        if (hx_is_wide(ty)) a64_stur_x_imm(src_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
            a64_sturb_imm(src_reg, addr_reg, byte_off);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
            a64_sturh_imm(src_reg, addr_reg, byte_off);
        else
            a64_stur_w_imm(src_reg, addr_reg, byte_off);
        return;
    }
    if (!hx_emit_add_delta(HX_SCRATCH1, addr_reg, byte_off, 1)) {
        hx_mat_iconst(HX_SCRATCH1, byte_off, 1);
        a64_add_x(HX_SCRATCH1, addr_reg, HX_SCRATCH1);
    }
    if (hx_is_wide(ty)) a64_str_x_imm(src_reg, HX_SCRATCH1, 0);
    else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
        a64_strb_imm(src_reg, HX_SCRATCH1, 0);
    else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
        a64_strh_imm(src_reg, HX_SCRATCH1, 0);
    else
        a64_str_w_imm(src_reg, HX_SCRATCH1, 0);
}

/* FP variants of the frame load/store helpers.  Mirrors the X-form ones:
 * try scaled-imm12 form first, then unscaled imm9, then materialised
 * scratch address.  is_double picks D-form (8-byte) vs S-form (4-byte). */
static void hx_emit_fp_load_to_reg(int dst_reg, int spill_off, int is_double) {
    int off;
    off = cg_a64_offset(spill_off);
    if (off >= 0 && off <= 32760 && (off & (is_double ? 7 : 3)) == 0) {
        if (is_double) a64_ldr_d_imm(dst_reg, A64_X29, off);
        else           a64_ldr_s_imm(dst_reg, A64_X29, off);
        return;
    }
    if (off >= -256 && off <= 255) {
        if (is_double) a64_ldur_d_imm(dst_reg, A64_X29, off);
        else           a64_ldur_s_imm(dst_reg, A64_X29, off);
        return;
    }
    {
        int neg;
        if (off < 0) {
            neg = -off;
            if (neg <= 4095) {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg);
            } else {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg & 0xFFF);
                a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF);
            }
        } else {
            if (off <= 4095) {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off);
            } else {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (off >> 12) & 0xFFF);
            }
        }
        if (is_double) a64_ldr_d_imm(dst_reg, HX_SCRATCH2, 0);
        else           a64_ldr_s_imm(dst_reg, HX_SCRATCH2, 0);
    }
}

static void hx_emit_fp_store_from_reg(int src_reg, int spill_off, int is_double) {
    int off;
    off = cg_a64_offset(spill_off);
    if (off >= 0 && off <= 32760 && (off & (is_double ? 7 : 3)) == 0) {
        if (is_double) a64_str_d_imm(src_reg, A64_X29, off);
        else           a64_str_s_imm(src_reg, A64_X29, off);
        return;
    }
    if (off >= -256 && off <= 255) {
        if (is_double) a64_stur_d_imm(src_reg, A64_X29, off);
        else           a64_stur_s_imm(src_reg, A64_X29, off);
        return;
    }
    {
        int neg;
        if (off < 0) {
            neg = -off;
            if (neg <= 4095) {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg);
            } else {
                a64_sub_x_imm(HX_SCRATCH2, A64_X29, neg & 0xFFF);
                a64_sub_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (neg >> 12) & 0xFFF);
            }
        } else {
            if (off <= 4095) {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off);
            } else {
                a64_add_x_imm(HX_SCRATCH2, A64_X29, off & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH2, HX_SCRATCH2, (off >> 12) & 0xFFF);
            }
        }
        if (is_double) a64_str_d_imm(src_reg, HX_SCRATCH2, 0);
        else           a64_str_s_imm(src_reg, HX_SCRATCH2, 0);
    }
}

/* ============================================================================
 * Block-level branch helpers
 * ============================================================================ */

static void hx_jump_to_block(int blk) {
    if (hx_blk_off[blk] >= 0) {
        a64_b(hx_blk_off[blk] - a64_off);
        return;
    }
    hx_bpatch_blk[hx_nbpatch] = blk;
    hx_bpatch_off[hx_nbpatch] = a64_off;
    hx_nbpatch = hx_nbpatch + 1;
    a64_b(0);
}

static void hx_bcond_to_block(int cond, int blk) {
    if (hx_blk_off[blk] >= 0) {
        a64_b_cond(cond, hx_blk_off[blk] - a64_off);
        return;
    }
    hx_bpatch_blk[hx_nbpatch] = blk;
    hx_bpatch_off[hx_nbpatch] = a64_off;
    hx_nbpatch = hx_nbpatch + 1;
    a64_b_cond(cond, 0);
}

static void hx_cbz_to_block(int reg, int blk) {
    if (hx_blk_off[blk] >= 0) {
        a64_cbz_w(reg, hx_blk_off[blk] - a64_off);
        return;
    }
    hx_bpatch_blk[hx_nbpatch] = blk;
    hx_bpatch_off[hx_nbpatch] = a64_off;
    hx_nbpatch = hx_nbpatch + 1;
    a64_cbz_w(reg, 0);
}

static int hx_edge_has_phi(int from_blk, int to_blk) {
    int p;
    int j;

    p = ssa_phi_head[to_blk];
    while (p >= 0) {
        if (h_kind[p] != HI_NOP) {
            j = 0;
            while (j < h_pcnt[p]) {
                if (h_pblk[h_pbase[p] + j] == from_blk) return 1;
                j = j + 1;
            }
        }
        p = ssa_phi_next[p];
    }
    return 0;
}

/* ----------------------------------------------------------------------
 * Indexed-addressing fold pass.
 *
 * Recognises LOAD/STORE whose address is HI_ADD(base, HI_SLL(idx, k))
 * (or symmetric) where k is 0 / 2 / 3 matching the access size, and
 * the ADD and SLL are single-use.  Emits this as the AArch64
 * register-indexed addressing mode `[Xn, Xm, LSL #k]` (1 instruction
 * vs the 3 instructions of a separate SLL + ADD + LDR).
 *
 * Run after bg_select / bg_count_uses so bg_uses[] is current.  Sets
 * bg_fold[ADD] = bg_fold[SLL] = 1 to prevent their def-site emissions,
 * and stores the folded operands for codegen to look up.
 * ---------------------------------------------------------------------- */
static int hx_indexed_shift_for_size(int access_size) {
    if (access_size == 4) return 2;   /* LDR Wt: scale 4 = LSL #2 */
    if (access_size == 8) return 3;   /* LDR Xt: scale 8 = LSL #3 */
    return -1;
}

static int hx_load_store_size(int idx) {
    int k; int ty;
    k = h_kind[idx];
    if (k != HI_LOAD && k != HI_STORE) return -1;
    ty = h_ty[idx];
    return hx_access_size_for_ty(ty);
}

static void hx_count_post_fusion_uses(void) {
    int i;
    int j;
    int k;
    int base;
    int cnt;

    i = 0;
    while (i < h_ninst) {
        hx_off_use[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        if (h_src1[i] >= 0) hx_off_use[h_src1[i]] = hx_off_use[h_src1[i]] + 1;
        if (h_src2[i] >= 0 && ho_src2_is_ref(k))
            hx_off_use[h_src2[i]] = hx_off_use[h_src2[i]] + 1;

        if (k == HI_CALL || k == HI_CALLP || k == HI_A64_DBT_TRAMPOLINE) {
            base = h_cbase[i];
            cnt = h_val[i];
            j = 0;
            while (j < cnt) {
                if (h_carg[base + j] >= 0)
                    hx_off_use[h_carg[base + j]] = hx_off_use[h_carg[base + j]] + 1;
                j = j + 1;
            }
        }

        if (k == HI_PHI) {
            base = h_pbase[i];
            cnt = h_pcnt[i];
            j = 0;
            while (j < cnt) {
                if (h_pval[base + j] >= 0)
                    hx_off_use[h_pval[base + j]] = hx_off_use[h_pval[base + j]] + 1;
                j = j + 1;
            }
        }

        i = i + 1;
    }

    /* Fused BRCs keep using the NOPed comparison's operands through
     * hx_brc_fuse[].  Count those side uses so an ADDI feeding a fused
     * compare is not mistaken for fully folded-away. */
    i = 0;
    while (i < h_ninst) {
        int cmp;
        cmp = hx_brc_fuse[i];
        if (cmp >= 0) {
            if (h_src1[cmp] >= 0) hx_off_use[h_src1[cmp]] = hx_off_use[h_src1[cmp]] + 1;
            if (h_src2[cmp] >= 0) hx_off_use[h_src2[cmp]] = hx_off_use[h_src2[cmp]] + 1;
        }
        i = i + 1;
    }
}

static int hx_addi_base_safe_at_use(int add_inst, int base_inst, int use_inst) {
    int base_reg;
    int add_reg;
    int use_pos;

    if (base_inst < 0 || use_inst < 0) return 0;
    if (h_kind[base_inst] == HI_NOP) return 0;
    if (ra_reg[base_inst] < 0) return 0;

    use_pos = ra_pos[use_inst];
    /* No linearised position means the use is dead/skipped (NOPed comparison,
     * remat-only def, etc.) — declining to fold is the safe answer; regalloc
     * decisions about base liveness aren't meaningful at a position regalloc
     * doesn't index. */
    if (use_pos < 0) return 0;

    /* If the original base is live here, regalloc protects its register. */
    if (ra_iend[base_inst] >= use_pos) return 1;

    /* Otherwise the fold is still safe when IRC coalesced ADDI onto the
     * base register: the ADDI interval protects the same physical register,
     * and suppressing the ADDI leaves the original base value there. */
    if (ra_reg[add_inst] < 0) return 0;
    base_reg = hx_slot_reg(ra_reg[base_inst]);
    add_reg = hx_slot_reg(ra_reg[add_inst]);
    return base_reg == add_reg;
}

static void hx_fold_single_use_addi_addr_pre_ra(void) {
    int i;

    i = 0;
    while (i < h_ninst) {
        hx_pre_off_base[i] = -1;
        hx_pre_off_disp[i] = 0;
        i = i + 1;
    }

    hx_count_post_fusion_uses();

    i = 0;
    while (i < h_ninst) {
        int access_size;
        int addr_inst;
        int base_inst;
        int disp;
        int pat;
        int lnt;

        access_size = hx_load_store_size(i);
        if (access_size < 0) { i = i + 1; continue; }

        addr_inst = h_src1[i];
        if (addr_inst < 0 || h_kind[addr_inst] != HI_ADDI) {
            i = i + 1; continue;
        }
        if (hx_off_use[addr_inst] != 1) { i = i + 1; continue; }
        if (h_blk[addr_inst] != h_blk[i]) { i = i + 1; continue; }
        if (bg_fold[addr_inst]) { i = i + 1; continue; }

        pat = bg_sel[i];
        lnt = (pat >= 0) ? bg_plnt[pat] : -1;
        if (lnt == BG_MEM || lnt == BG_SADDR) { i = i + 1; continue; }

        base_inst = h_src1[addr_inst];
        disp = h_val[addr_inst];
        if (base_inst < 0 || h_kind[base_inst] == HI_NOP) {
            i = i + 1; continue;
        }
        if (!hx_offset_fold_encodable(disp, access_size)) {
            i = i + 1; continue;
        }

        hx_pre_off_base[i] = base_inst;
        hx_pre_off_disp[i] = disp;
        h_src1[i] = base_inst;
        bg_fold[addr_inst] = 1;
        h_kind[addr_inst] = HI_NOP;

        i = i + 1;
    }
}

static void hx_fold_indexed_addr(void) {
    int i;
    int access_size; int wanted_shift;
    int add_inst; int sll_inst;
    int base_inst; int idx_inst;
    int shift_amt;
    int s2_imm_inst;
    int a; int b;

    i = 0;
    while (i < h_ninst) {
        hx_ridx_base[i] = -1;
        hx_off_base[i]  = hx_pre_off_base[i];
        hx_off_disp[i]  = hx_pre_off_disp[i];
        hx_off_fold_use[i] = 0;
        i = i + 1;
    }

    hx_count_post_fusion_uses();

    i = 0;
    while (i < h_ninst) {
        access_size = hx_load_store_size(i);
        if (access_size < 0) { i = i + 1; continue; }
        if (hx_off_base[i] >= 0) { i = i + 1; continue; }

        /* HI_LOAD's address is src1; HI_STORE's address is src1 too
         * (value being stored is src2). */
        add_inst = h_src1[i];
        if (add_inst < 0) { i = i + 1; continue; }

        /* --- Pattern A: HI_ADDI(base, const) — offset-displacement fold ---
         * This pass runs after regalloc.  Folding a multi-use ADDI is only
         * safe when every real use is rewritten here; if any consumer still
         * needs the ADDI value, the standalone ADDI must remain.  Per-use,
         * the base register must also be protected either by the base's own
         * live range or by coalescing the ADDI onto the same register. */
        if (h_kind[add_inst] == HI_ADDI && !bg_fold[add_inst]) {
            int base_a; int disp;
            int v_inst;
            base_a = h_src1[add_inst];
            disp   = h_val[add_inst];
            v_inst = (h_kind[i] == HI_STORE) ? h_src2[i] : -1;
            if (base_a >= 0 &&
                hx_addi_base_safe_at_use(add_inst, base_a, i) &&
                /* For STORE, val also needs a real reg (we only have
                 * 2 scratches; base may need one). */
                (v_inst < 0 || ra_reg[v_inst] >= 0)) {
                if (hx_offset_fold_encodable(disp, access_size)) {
                    hx_off_base[i] = base_a;
                    hx_off_disp[i] = disp;
                    hx_off_fold_use[add_inst] = hx_off_fold_use[add_inst] + 1;
                    i = i + 1;
                    continue;
                }
            }
        }

        if (h_kind[add_inst] != HI_ADD) { i = i + 1; continue; }
        if (bg_uses[add_inst] != 1) { i = i + 1; continue; }
        if (bg_fold[add_inst]) { i = i + 1; continue; }  /* already folded by BURG */
        /* No FP register-indexed encoders yet (a64_ldr_s/d_reg_lsl).
         * Skip the fold for V-class load/store; offset-displacement
         * fold (Pattern A above) still applies and uses ldr s/d imm. */
        if (access_size < 4) { i = i + 1; continue; }
        if (ty_is_fp(h_ty[i])) { i = i + 1; continue; }

        /* Try right arm: ADD(base, SLL(idx, k)). */
        a = h_src1[add_inst];
        b = h_src2[add_inst];
        sll_inst = -1;
        base_inst = -1;
        if (b >= 0 && h_kind[b] == HI_SLL && bg_uses[b] == 1) {
            sll_inst = b; base_inst = a;
        } else if (a >= 0 && h_kind[a] == HI_SLL && bg_uses[a] == 1) {
            /* Symmetric: ADD(SLL(idx, k), base). */
            sll_inst = a; base_inst = b;
        }
        if (sll_inst < 0) {
            /* Also accept shift = 0: ADD(base, idx) directly.  Rare
             * but harmless to fold (saves 1 instruction). */
            i = i + 1; continue;
        }

        /* The SLL must have a constant shift amount. */
        s2_imm_inst = h_src2[sll_inst];
        if (s2_imm_inst < 0 || h_kind[s2_imm_inst] != HI_ICONST) {
            i = i + 1; continue;
        }
        shift_amt = h_val[s2_imm_inst];
        wanted_shift = hx_indexed_shift_for_size(access_size);
        if (shift_amt != 0 && shift_amt != wanted_shift) {
            /* AArch64 indexed addressing only encodes shift 0 or the
             * access-size shift.  Other shifts can't fold. */
            i = i + 1; continue;
        }

        idx_inst = h_src1[sll_inst];
        if (idx_inst < 0 || base_inst < 0) { i = i + 1; continue; }

        /* Scratch budget: base needs SCRATCH1 (if spilled), idx needs
         * SCRATCH2 (if spilled).  For HI_STORE we also need a register
         * for the value being stored — and we only have two scratches.
         * Require all operands to be in real registers (ra_reg[] >= 0)
         * so codegen never has to load via scratch.  Hot dispatch
         * handlers are register-pressure-light, so this is the common
         * case anyway. */
        if (ra_reg[base_inst] < 0) { i = i + 1; continue; }
        if (ra_reg[idx_inst]  < 0) { i = i + 1; continue; }
        if (h_kind[i] == HI_STORE) {
            int v_inst;
            v_inst = h_src2[i];
            if (v_inst < 0 || ra_reg[v_inst] < 0) { i = i + 1; continue; }
        }

        /* Commit the fold. */
        hx_ridx_base[i]  = base_inst;
        hx_ridx_idx[i]   = idx_inst;
        hx_ridx_shift[i] = shift_amt;
        bg_fold[add_inst] = 1;
        bg_fold[sll_inst] = 1;
        i = i + 1;
    }

    /* Commit only ADDIs whose complete current use set was folded.  This
     * keeps partially-foldable ADDIs materialised for their remaining users
     * and also filters out folded-looking sites that were only candidates. */
    i = 0;
    while (i < h_ninst) {
        if (hx_off_base[i] >= 0 && hx_pre_off_base[i] < 0) {
            add_inst = h_src1[i];
            if (add_inst < 0 || h_kind[add_inst] != HI_ADDI ||
                hx_off_fold_use[add_inst] != hx_off_use[add_inst]) {
                hx_off_base[i] = -1;
                hx_off_disp[i] = 0;
            }
        }
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ADDI &&
            hx_off_fold_use[i] > 0 &&
            hx_off_fold_use[i] == hx_off_use[i]) {
            bg_fold[i] = 1;
        }
        i = i + 1;
    }
}

/* When BLK is laid down, walk patches and rewrite. */
static void hx_define_block(int blk) {
    int i;
    int word; int diff; int top8; int site; int kept;
    hx_blk_off[blk] = a64_off;
    i = 0;
    while (i < hx_nbpatch) {
        if (hx_bpatch_blk[i] == blk) {
            site = hx_bpatch_off[i];
            diff = a64_off - site;
            word = a64_read_inst(site);
            top8 = (word >> 24) & 0xFF;
            if (top8 == 0x14 || top8 == 0x94) {
                int imm26;
                imm26 = (diff >> 2) & 0x03FFFFFF;
                kept = word & 0xFC000000;
                a64_patch_inst(site, kept | imm26);
            } else {
                int imm19;
                imm19 = (diff >> 2) & 0x7FFFF;
                kept = word & 0xFF00001F;
                a64_patch_inst(site, kept | (imm19 << 5));
            }
            hx_nbpatch = hx_nbpatch - 1;
            hx_bpatch_blk[i] = hx_bpatch_blk[hx_nbpatch];
            hx_bpatch_off[i] = hx_bpatch_off[hx_nbpatch];
        } else {
            i = i + 1;
        }
    }
}

/* ============================================================================
 * Per-instruction emission
 * ============================================================================ */

/* Map a comparison HIR opcode to an AArch64 condition code. */
static int hx_cmp_cond(int k) {
    if (k == HI_SEQ)  return A64_COND_EQ;
    if (k == HI_SNE)  return A64_COND_NE;
    if (k == HI_SLT)  return A64_COND_LT;
    if (k == HI_SGT)  return A64_COND_GT;
    if (k == HI_SLE)  return A64_COND_LE;
    if (k == HI_SGE)  return A64_COND_GE;
    if (k == HI_SLTU) return A64_COND_LO;
    if (k == HI_SGTU) return A64_COND_HI;
    if (k == HI_SLEU) return A64_COND_LS;
    if (k == HI_SGEU) return A64_COND_HS;
    return A64_COND_EQ;
}

/* Inverted condition for fused BRC: jump to "not-taken" when comparison
 * is FALSE, so we want the inverse of hx_cmp_cond. */
static int hx_cmp_inv_cond(int k) {
    if (k == HI_SEQ)  return A64_COND_NE;
    if (k == HI_SNE)  return A64_COND_EQ;
    if (k == HI_SLT)  return A64_COND_GE;
    if (k == HI_SGT)  return A64_COND_LE;
    if (k == HI_SLE)  return A64_COND_GT;
    if (k == HI_SGE)  return A64_COND_LT;
    if (k == HI_SLTU) return A64_COND_HS;
    if (k == HI_SGTU) return A64_COND_LS;
    if (k == HI_SLEU) return A64_COND_HI;
    if (k == HI_SGEU) return A64_COND_LO;
    return A64_COND_NE;
}

static int hx_is_cmp_op(int k) { return k >= HI_SEQ && k <= HI_SGEU; }
static int hx_is_fcmp_op(int k) { return k == HI_FEQ || k == HI_FLT || k == HI_FLE; }

/* AArch64 condition mapping for FP compares (ordered: NaN → false).
 * LT uses MI (N=1) instead of LT (N!=V) to make unordered → false;
 * LE uses LS (C=0 OR Z=1) instead of LE for the same reason. */
static int hx_fcmp_cond(int k) {
    if (k == HI_FEQ) return A64_COND_EQ;
    if (k == HI_FLT) return A64_COND_MI;
    if (k == HI_FLE) return A64_COND_LS;
    return A64_COND_EQ;
}

/* Inverted FP cond for fused BRC.  Inverse must include "unordered"
 * (so branch-to-not-taken fires on NaN, matching the C semantic that
 * `a < b` etc. are false when either operand is NaN). */
static int hx_fcmp_inv_cond(int k) {
    if (k == HI_FEQ) return A64_COND_NE;
    if (k == HI_FLT) return A64_COND_PL;  /* N=0: ordered-GE and unordered */
    if (k == HI_FLE) return A64_COND_HI;  /* C=1 AND Z=0: ordered-GT and unordered */
    return A64_COND_NE;
}

static void hx_stack_store(int off, int reg, int wide) {
    if (off >= 0 && off <= 32760 && (off & (wide ? 7 : 3)) == 0) {
        if (wide) a64_str_x_imm(reg, A64_SP, off);
        else      a64_str_w_imm(reg, A64_SP, off);
        return;
    }
    if (off >= -256 && off <= 255) {
        if (wide) a64_stur_x_imm(reg, A64_SP, off);
        else      a64_stur_w_imm(reg, A64_SP, off);
        return;
    }
    hx_die("hx_stack_store: offset out of range", off);
}

static void hx_stack_load(int reg, int off, int wide) {
    if (off >= 0 && off <= 32760 && (off & (wide ? 7 : 3)) == 0) {
        if (wide) a64_ldr_x_imm(reg, A64_SP, off);
        else      a64_ldr_w_imm(reg, A64_SP, off);
        return;
    }
    if (off >= -256 && off <= 255) {
        if (wide) a64_ldur_x_imm(reg, A64_SP, off);
        else      a64_ldur_w_imm(reg, A64_SP, off);
        return;
    }
    hx_die("hx_stack_load: offset out of range", off);
}

/* ----- PHI move scheduling -----
 * When control flow leaves a block to a successor that has PHI nodes,
 * we materialize each incoming value to a temporary stack slot, then
 * restore them into the PHI destinations. This preserves parallel-copy
 * semantics even for cycles like register swaps on loop backedges. */

static void hx_phi_copies(int from_blk, int to_blk) {
    int i; int src; int dst_inst; int dst_reg;
    int wide;
    int j;
    int tmp_bytes;
    int tmp_slot;

    /* Collect all (PHI_dst, incoming_value) pairs for moves. */
    hx_phi_n = 0;
    i = ssa_phi_head[to_blk];
    while (i >= 0) {
        if (h_kind[i] != HI_NOP) {
            /* Find the entry in h_pblk/h_pval matching from_blk. */
            int base; int n; int found;
            base = h_pbase[i];
            n = h_pcnt[i];
            found = -1;
            j = 0;
            while (j < n) {
                if (h_pblk[base + j] == from_blk) { found = h_pval[base + j]; break; }
                j = j + 1;
            }
            if (found >= 0) {
                hx_phi_dst_inst[hx_phi_n] = i;
                hx_phi_src_val[hx_phi_n]  = found;
                hx_phi_n = hx_phi_n + 1;
            }
        }
        i = ssa_phi_next[i];
    }
    if (hx_phi_n == 0) return;

    /* Fast path: greedy parallel move via direct register copies.
     *
     * For each phi we want to emit `dst_reg = src_reg` (or a hx_mat for
     * rematerializable / spilled sources).  Hazard: if some other phi
     * still needs the OLD value of dst_reg as ITS source, we can't
     * write dst_reg yet.
     *
     * Greedy: repeatedly pick any phi whose dst_reg is not the src_reg
     * of any remaining phi, emit its move, mark done.  If progress
     * stalls, there's a cycle (e.g., swap) — fall back to the
     * stack-based parallel move below.
     *
     * Phis whose source is rematerializable or spilled have no register
     * hazard for that src — they can always emit safely (hx_mat loads
     * via SCRATCH1 or directly into dst_reg). */
    {
        int done[HIR_MAX_INST];   /* per-phi bool, indexed by phi position */
        int phi_src_reg[HIR_MAX_INST];
        int phi_dst_reg[HIR_MAX_INST];
        int n_remaining;
        int progress;
        int p;

        i = 0;
        while (i < hx_phi_n) {
            done[i] = 0;
            src = hx_phi_src_val[i];
            dst_inst = hx_phi_dst_inst[i];
            phi_src_reg[i] = (ra_reg[src] >= 0) ? hx_slot_reg(ra_reg[src]) : -1;
            phi_dst_reg[i] = hx_dst_reg(dst_inst);
            /* No-op move (src and dst already share the register): mark
             * done immediately so it doesn't block others. */
            if (phi_src_reg[i] >= 0 && phi_src_reg[i] == phi_dst_reg[i]) {
                hx_spill(dst_inst, phi_dst_reg[i]);
                done[i] = 1;
            }
            i = i + 1;
        }

        n_remaining = 0;
        i = 0;
        while (i < hx_phi_n) { if (!done[i]) n_remaining = n_remaining + 1; i = i + 1; }

        progress = 1;
        while (n_remaining > 0 && progress) {
            progress = 0;
            p = 0;
            while (p < hx_phi_n) {
                int blocked;
                if (done[p]) { p = p + 1; continue; }
                /* phi_dst_reg[p] is safe to clobber iff no remaining phi
                 * has phi_dst_reg[p] as its src_reg. */
                blocked = 0;
                i = 0;
                while (i < hx_phi_n) {
                    if (!done[i] && i != p && phi_src_reg[i] == phi_dst_reg[p]) {
                        blocked = 1;
                        i = hx_phi_n;
                    } else {
                        i = i + 1;
                    }
                }
                if (!blocked) {
                    src = hx_phi_src_val[p];
                    dst_inst = hx_phi_dst_inst[p];
                    dst_reg = phi_dst_reg[p];
                    wide = hx_is_wide(h_ty[dst_inst]);
                    if (phi_src_reg[p] >= 0) {
                        if (hx_is_v(dst_inst)) {
                            if (ty_is_double(h_ty[dst_inst]))
                                a64_fmov_d_d(dst_reg, phi_src_reg[p]);
                            else
                                a64_fmov_s_s(dst_reg, phi_src_reg[p]);
                        } else if (wide) {
                            a64_mov_x(dst_reg, phi_src_reg[p]);
                        } else {
                            a64_mov_w(dst_reg, phi_src_reg[p]);
                        }
                    } else {
                        /* Spilled / rematerialisable source: bring it
                         * straight into dst_reg. */
                        hx_mat(src, dst_reg);
                    }
                    hx_spill(dst_inst, dst_reg);
                    done[p] = 1;
                    n_remaining = n_remaining - 1;
                    progress = 1;
                }
                p = p + 1;
            }
        }
        if (n_remaining == 0) return;
        /* Otherwise fall through to the stack-based fallback for the
         * remaining phis (cycles).  We still need to handle them all
         * uniformly: rebuild the (src, dst) list with only those still
         * undone. */
        {
            int new_n;
            new_n = 0;
            i = 0;
            while (i < hx_phi_n) {
                if (!done[i]) {
                    hx_phi_dst_inst[new_n] = hx_phi_dst_inst[i];
                    hx_phi_src_val[new_n]  = hx_phi_src_val[i];
                    new_n = new_n + 1;
                }
                i = i + 1;
            }
            hx_phi_n = new_n;
            if (hx_phi_n == 0) return;
        }
    }

    /* Stack-based parallel move (fallback for cycles). */
    tmp_bytes = hx_phi_n * 8;
    if (tmp_bytes & 15) tmp_bytes = (tmp_bytes + 15) & ~15;
    if (tmp_bytes <= 4095) a64_sub_x_imm(A64_SP, A64_SP, tmp_bytes);
    else {
        a64_sub_x_imm(A64_SP, A64_SP, tmp_bytes & 0xFFF);
        a64_sub_x_imm_lsl12(A64_SP, A64_SP, (tmp_bytes >> 12) & 0xFFF);
    }

    /* Phase 1: snapshot every source into the temp area.  Each slot is
     * 8 bytes wide regardless of class (double / pointer / int all fit). */
    i = 0;
    while (i < hx_phi_n) {
        src      = hx_phi_src_val[i];
        wide     = hx_is_wide(h_ty[src]);
        if (hx_is_v(src)) {
            hx_mat(src, HX_SCRATCH_V1);
            if (ty_is_double(h_ty[src]))
                a64_str_d_imm(HX_SCRATCH_V1, A64_SP, i * 8);
            else
                a64_str_s_imm(HX_SCRATCH_V1, A64_SP, i * 8);
        } else {
            hx_mat(src, HX_SCRATCH1);
            hx_stack_store(i * 8, HX_SCRATCH1, wide);
        }
        i = i + 1;
    }

    /* Phase 2: restore into destinations. */
    i = 0;
    while (i < hx_phi_n) {
        dst_inst = hx_phi_dst_inst[i];
        dst_reg  = hx_dst_reg(dst_inst);
        wide     = hx_is_wide(h_ty[dst_inst]);
        tmp_slot = i * 8;
        if (hx_is_v(dst_inst)) {
            if (ty_is_double(h_ty[dst_inst]))
                a64_ldr_d_imm(dst_reg, A64_SP, tmp_slot);
            else
                a64_ldr_s_imm(dst_reg, A64_SP, tmp_slot);
        } else {
            hx_stack_load(dst_reg, tmp_slot, wide);
        }
        hx_spill(dst_inst, dst_reg);
        i = i + 1;
    }

    if (tmp_bytes <= 4095) a64_add_x_imm(A64_SP, A64_SP, tmp_bytes);
    else {
        a64_add_x_imm(A64_SP, A64_SP, tmp_bytes & 0xFFF);
        a64_add_x_imm_lsl12(A64_SP, A64_SP, (tmp_bytes >> 12) & 0xFFF);
    }
}

/* ----- Frame teardown -----
 * Restores callee-saved regs, FP/LR, and SP — everything the regular epilogue
 * does except the final `ret`.  Used by the regular epilogue (followed by
 * `ret`) and by TCE sites (followed by `b target`). */

static void hx_emit_teardown(void) {
    int fs;
    int i;
    fs = hx_frame_size;

    /* Restore callee-saved regs (mirror prologue's STP pairing). */
    i = 0;
    while (i < ra_ncsave) {
        if (i + 1 < ra_ncsave &&
            ra_csave_off[i + 1] == ra_csave_off[i] - 4) {
            int boff_lo;
            boff_lo = cg_a64_offset(ra_csave_off[i + 1]);
            if (boff_lo >= -512 && boff_lo <= 504 && (boff_lo & 7) == 0) {
                a64_ldp_x_off(ra_csave_reg[i + 1], ra_csave_reg[i],
                              A64_X29, boff_lo);
                i = i + 2;
                continue;
            }
        }
        hx_emit_load_to_reg(ra_csave_reg[i], ra_csave_off[i], 1);
        i = i + 1;
    }

    if (!hx_no_frame) {
        if (fs - 16 <= 504) {
            a64_ldp_x_off(A64_X29, A64_X30, A64_SP, fs - 16);
        } else {
            if ((fs - 16) <= 4095) a64_add_x_imm(HX_SCRATCH1, A64_SP, fs - 16);
            else {
                a64_add_x_imm(HX_SCRATCH1, A64_SP, (fs - 16) & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH1, HX_SCRATCH1, ((fs - 16) >> 12) & 0xFFF);
            }
            a64_ldp_x_off(A64_X29, A64_X30, HX_SCRATCH1, 0);
        }
        if (fs <= 4095) a64_add_x_imm(A64_SP, A64_SP, fs);
        else {
            a64_add_x_imm(A64_SP, A64_SP, fs & 0xFFF);
            a64_add_x_imm_lsl12(A64_SP, A64_SP, (fs >> 12) & 0xFFF);
        }
    }
}

/* ----- Tail-call argument marshal -----
 * Move the call's arg insts into x0..x7 with parallel-move hazard handling.
 * Same shape as hx_phi_copies's greedy-then-stack scheduler, but the dests
 * are the AAPCS arg regs.  Sources can be in any register, spilled, or
 * rematerialisable.  Restricted to nargs <= 8 (no stack args).
 *
 * This must run BEFORE the frame teardown (sources may live in callee-saved
 * regs that the teardown will restore, or in FP-relative spills which need
 * FP intact to read). */
static void hx_marshal_call_args(int base, int nargs) {
    int j;
    int n;
    int done[8];
    int src_inst[8];
    int dst_reg[8];
    int src_reg[8];
    int arg_cls[8];     /* RA_CLASS_X (X0..X7) or RA_CLASS_V (V0..V7) */
    int n_remaining;
    int progress;
    int p;
    int i;
    int wide;
    int ngrn; int nsrn;

    if (nargs > 8) hx_die("hx_marshal_call_args: stack args unsupported", nargs);
    n = nargs;

    /* AAPCS64: int args fill X0..X7 via NGRN, FP args fill V0..V7 via
     * NSRN — independent counters, different physical reg files. */
    ngrn = 0; nsrn = 0;
    j = 0;
    while (j < n) {
        int arg_inst;
        int is_fp;
        arg_inst = h_carg[base + j];
        /* Classify by ra_class_of (NOT ty_is_fp): HI_ALLOCA / HI_GADDR
         * etc. carry h_ty=storage-type but produce X-class addresses. */
        is_fp = (ra_class_of(arg_inst) == RA_CLASS_V);
        src_inst[j] = arg_inst;
        if (is_fp) {
            arg_cls[j] = RA_CLASS_V;
            dst_reg[j] = nsrn;          /* V0..V7 share encoding 0..7 */
            nsrn = nsrn + 1;
        } else {
            arg_cls[j] = RA_CLASS_X;
            dst_reg[j] = hx_arg_reg[ngrn];
            ngrn = ngrn + 1;
        }
        src_reg[j]  = (ra_reg[arg_inst] >= 0) ? hx_slot_reg(ra_reg[arg_inst]) : -1;
        done[j] = 0;
        if (src_reg[j] >= 0 && src_reg[j] == dst_reg[j]) done[j] = 1;
        j = j + 1;
    }

    n_remaining = 0;
    j = 0;
    while (j < n) { if (!done[j]) n_remaining = n_remaining + 1; j = j + 1; }

    progress = 1;
    while (n_remaining > 0 && progress) {
        progress = 0;
        p = 0;
        while (p < n) {
            int blocked;
            if (done[p]) { p = p + 1; continue; }
            blocked = 0;
            i = 0;
            while (i < n) {
                /* Cross-class never conflicts (different physical files);
                 * within a class, same reg encoding is a true conflict. */
                if (!done[i] && i != p
                 && arg_cls[i] == arg_cls[p]
                 && src_reg[i] == dst_reg[p]) {
                    blocked = 1;
                    i = n;
                } else {
                    i = i + 1;
                }
            }
            if (!blocked) {
                wide = hx_is_wide(h_ty[src_inst[p]]);
                if (src_reg[p] >= 0) {
                    if (arg_cls[p] == RA_CLASS_V) {
                        if (ty_is_double(h_ty[src_inst[p]]))
                            a64_fmov_d_d(dst_reg[p], src_reg[p]);
                        else
                            a64_fmov_s_s(dst_reg[p], src_reg[p]);
                    } else if (wide) {
                        a64_mov_x(dst_reg[p], src_reg[p]);
                    } else {
                        a64_mov_w(dst_reg[p], src_reg[p]);
                    }
                } else {
                    /* Spilled or rematerialisable: bring directly into dst. */
                    hx_mat(src_inst[p], dst_reg[p]);
                }
                done[p] = 1;
                n_remaining = n_remaining - 1;
                progress = 1;
            }
            p = p + 1;
        }
    }

    if (n_remaining == 0) return;

    /* Cycle: stack-snapshot remaining sources, then restore.  Same pattern
     * as hx_phi_copies's fallback path. */
    {
        int tmp_bytes;
        int slot;
        tmp_bytes = n_remaining * 8;
        if (tmp_bytes & 15) tmp_bytes = (tmp_bytes + 15) & ~15;
        if (tmp_bytes <= 4095) a64_sub_x_imm(A64_SP, A64_SP, tmp_bytes);
        else {
            a64_sub_x_imm(A64_SP, A64_SP, tmp_bytes & 0xFFF);
            a64_sub_x_imm_lsl12(A64_SP, A64_SP, (tmp_bytes >> 12) & 0xFFF);
        }
        slot = 0;
        i = 0;
        while (i < n) {
            if (!done[i]) {
                wide = hx_is_wide(h_ty[src_inst[i]]);
                if (arg_cls[i] == RA_CLASS_V) {
                    hx_mat(src_inst[i], HX_SCRATCH_V1);
                    if (ty_is_double(h_ty[src_inst[i]]))
                        a64_str_d_imm(HX_SCRATCH_V1, A64_SP, slot * 8);
                    else
                        a64_str_s_imm(HX_SCRATCH_V1, A64_SP, slot * 8);
                } else {
                    hx_mat(src_inst[i], HX_SCRATCH1);
                    hx_stack_store(slot * 8, HX_SCRATCH1, wide);
                }
                slot = slot + 1;
            }
            i = i + 1;
        }
        slot = 0;
        i = 0;
        while (i < n) {
            if (!done[i]) {
                wide = hx_is_wide(h_ty[src_inst[i]]);
                if (arg_cls[i] == RA_CLASS_V) {
                    if (ty_is_double(h_ty[src_inst[i]]))
                        a64_ldr_d_imm(dst_reg[i], A64_SP, slot * 8);
                    else
                        a64_ldr_s_imm(dst_reg[i], A64_SP, slot * 8);
                } else {
                    hx_stack_load(dst_reg[i], slot * 8, wide);
                }
                slot = slot + 1;
            }
            i = i + 1;
        }
        if (tmp_bytes <= 4095) a64_add_x_imm(A64_SP, A64_SP, tmp_bytes);
        else {
            a64_add_x_imm(A64_SP, A64_SP, tmp_bytes & 0xFFF);
            a64_add_x_imm_lsl12(A64_SP, A64_SP, (tmp_bytes >> 12) & 0xFFF);
        }
    }
}

/* Does this value transitively trace back to an HI_ALLOCA?  Used to
 * block TCE when a frame-local pointer would flow to the callee — after
 * teardown the pointer dangles. */
static int hx_traces_to_alloca(int v, int depth) {
    int k;
    if (v < 0) return 0;
    if (depth > 32) return 1;     /* conservative on cycles / deep chains */
    k = h_kind[v];
    if (k == HI_ALLOCA) return 1;
    if (k == HI_ADDI || k == HI_COPY) return hx_traces_to_alloca(h_src1[v], depth + 1);
    if (k == HI_ADD || k == HI_SUB) {
        if (hx_traces_to_alloca(h_src1[v], depth + 1)) return 1;
        return hx_traces_to_alloca(h_src2[v], depth + 1);
    }
    return 0;
}

/* ----- TCE identification -----
 * Set hx_tce_call[c]/hx_tce_ret[r] for each block that ends:
 *     ... CALL ... (NOP/PHI/CALLHI/COPY)* RET
 * with RET's value (chased through COPYs) equal to the call result.
 * Restricted to direct calls (HI_CALL), nargs <= 8, non-__syscall, no
 * arg-derived-from-HI_ALLOCA (frame-local ptr would dangle), no varargs
 * caller, no DBT trampoline. */
static void hx_identify_tce(Node *fn) {
    int i; int b;
    int has_alloca; int has_dbt;

    i = 0;
    while (i < h_ninst) {
        hx_tce_call[i] = 0;
        hx_tce_ret[i] = 0;
        i = i + 1;
    }

    if (fn->is_varargs) return;

    has_alloca = 0;
    has_dbt = 0;
    i = 0;
    while (i < h_ninst) {
        int k;
        k = h_kind[i];
        if (k == HI_ALLOCA) has_alloca = 1;
        if (k == HI_A64_DBT_TRAMPOLINE) has_dbt = 1;
        i = i + 1;
    }
    /* Allow HI_ALLOCA in the function (the lower emits it for the
     * synthetic retval slot used by multi-return functions; that slot
     * is read by the regular epilogue, which TCE branches out before).
     * We DO need to guard per-call: if any arg traces back to an
     * HI_ALLOCA, the callee gets a pointer into our frame which
     * dangles after teardown.  See the per-call alloca check below. */
    (void)has_alloca;
    if (has_dbt) return;

    b = 0;
    while (b < bb_nblk) {
        int last;
        int call_idx;
        if (bb_end[b] <= bb_start[b]) { b = b + 1; continue; }
        last = bb_end[b] - 1;
        if (h_kind[last] != HI_RET) { b = b + 1; continue; }

        /* Walk backward from RET, skipping HI_NOP / HI_PHI / HI_CALLHI /
         * HI_COPY; the first non-skipped op must be HI_CALL.  HI_COPY
         * is reg-to-reg only (no side effect); the TCE branch dead-codes
         * any COPY between CALL and RET, which is fine because their
         * only observable effect is via RET's value, and we re-check
         * that the returned value chains back through COPYs to the
         * call result. */
        call_idx = -1;
        i = last - 1;
        while (i >= bb_start[b]) {
            int k;
            k = h_kind[i];
            if (k == HI_NOP || k == HI_PHI || k == HI_CALLHI || k == HI_COPY) {
                i = i - 1;
                continue;
            }
            if (k == HI_CALL) call_idx = i;
            break;
        }
        if (call_idx >= 0) {
            int ok;
            char *nm;
            int v;
            ok = 1;

            /* RET's value (if any) must be — possibly via HI_COPY chain —
             * the call result. */
            if (h_src1[last] >= 0) {
                v = h_src1[last];
                while (v >= 0 && h_kind[v] == HI_COPY) v = h_src1[v];
                if (v != call_idx) ok = 0;
            }

            /* No stack args. */
            if (h_val[call_idx] > 8) ok = 0;

            /* Skip __syscall builtin (different ABI: x8 = nr). */
            nm = h_name[call_idx];
            if (nm != 0
             && nm[0] == '_' && nm[1] == '_'
             && nm[2] == 's' && nm[3] == 'y'
             && nm[4] == 's' && nm[5] == 'c') ok = 0;

            /* Block if any arg traces back to an HI_ALLOCA — the pointer
             * would dangle after frame teardown.  Cheap intra-procedural
             * check; covers the common `return foo(&local)` pattern. */
            if (ok) {
                int j;
                int base;
                int nargs;
                base = h_cbase[call_idx];
                nargs = h_val[call_idx];
                j = 0;
                while (j < nargs) {
                    if (hx_traces_to_alloca(h_carg[base + j], 0)) {
                        ok = 0;
                        j = nargs;
                    } else {
                        j = j + 1;
                    }
                }
            }

            if (ok) {
                hx_tce_call[call_idx] = 1;
                hx_tce_ret[last] = 1;
            }
        }
        b = b + 1;
    }
}

static void hx_cleanup_redundant_moves(void) {
    int i;

    i = 0;
    while (i < h_ninst) {
        int k;
        int s;
        k = h_kind[i];
        if ((k == HI_COPY || (k == HI_ADDI && h_val[i] == 0)) &&
            ra_spill_off[i] == 0) {
            s = h_src1[i];
            if (s >= 0 && ra_reg[i] >= 0 && ra_reg[s] >= 0 &&
                hx_slot_reg(ra_reg[i]) == hx_slot_reg(ra_reg[s])) {
                h_kind[i] = HI_NOP;
            }
        }
        i = i + 1;
    }
}

/* ============================================================================
 * LDP/STP partner search — find a same-base, adjacent-disp HI_LOAD/HI_STORE
 * later in the current block that can fuse with idx into a single LDP/STP.
 * Returns the partner's HIR index, or -1.
 *
 * Preconditions: idx already cleared the offset-disp-fold path
 * (hx_off_base[idx] >= 0, integer type).
 *
 * Safety:
 *   - Aliasing: stop at any HI_STORE / HI_CALL / HI_CALLP / DBT
 *     trampoline between loads.  For stores, stop at any intervening
 *     memory op or call unless it is the actual partner.
 *     (memory side-effects could be observed differently after fusion).
 *   - LDP architectural: rt1 != rt2; rt1/rt2 != base register
 *     (CONSTRAINED UNPREDICTABLE otherwise).
 *   - Color clobber: the partner's destination color must not be live
 *     across (idx, partner).  We approximate with a syntactic scan:
 *     no inst in (idx, partner) reads or writes that color.
 *   - Imm range: signed imm7 × access scale (W: ±256/±252 step 4;
 *     X: ±512/±504 step 8).
 *   - Partner must also be on the offset-disp-fold path, same width,
 *     same base instruction, integer type. */
static int hx_pair_mem_barrier(int kind) {
    return kind == HI_CALL || kind == HI_CALLP || kind == HI_A64_DBT_TRAMPOLINE;
}

static int hx_pairable_int_offset_mem(int idx, int kind, int wide) {
    int access_size;

    if (idx < 0 || h_kind[idx] != kind) return 0;
    if (hx_off_base[idx] < 0) return 0;
    if (ty_is_fp(h_ty[idx])) return 0;
    access_size = hx_access_size_for_ty(h_ty[idx]);
    if (access_size != (wide ? 8 : 4)) return 0;
    return 1;
}

static int hx_load_pair_safe(int idx, int j, int wide) {
    int color_b;
    int color_a;
    int color_base;
    int min_off;
    int diff;
    int x;

    color_a    = hx_slot_reg(ra_reg[idx]);
    color_b    = hx_slot_reg(ra_reg[j]);
    color_base = (ra_reg[hx_off_base[idx]] >= 0)
                   ? hx_slot_reg(ra_reg[hx_off_base[idx]]) : -1;

    if (color_a == color_b) return 0;                    /* LDP rt1 != rt2 */
    if (color_base >= 0 && color_a == color_base) return 0;
    if (color_base >= 0 && color_b == color_base) return 0;

    diff = hx_off_disp[j] - hx_off_disp[idx];
    if (diff > 0) min_off = hx_off_disp[idx];
    else          min_off = hx_off_disp[j];
    if (wide) {
        if (min_off < -512 || min_off > 504 || (min_off & 7)) return 0;
    } else {
        if (min_off < -256 || min_off > 252 || (min_off & 3)) return 0;
    }

    /* Scan (idx, j) for color clobbers / reads of color_b. */
    x = idx + 1;
    while (x < j) {
        int kx;
        kx = h_kind[x];
        if (kx != HI_NOP && kx != HI_PHI) {
            if (ra_reg[x] >= 0 && hx_slot_reg(ra_reg[x]) == color_b) return 0;
            if (h_src1[x] >= 0 && ra_reg[h_src1[x]] >= 0
                    && hx_slot_reg(ra_reg[h_src1[x]]) == color_b) return 0;
            if (h_src2[x] >= 0 && ra_reg[h_src2[x]] >= 0
                    && hx_slot_reg(ra_reg[h_src2[x]]) == color_b) return 0;
        }
        x = x + 1;
    }
    return 1;
}

static int hx_find_load_partner(int idx, int wide) {
    int step;
    int j;
    int base_inst;

    step = wide ? 8 : 4;
    base_inst = hx_off_base[idx];

    j = idx + 1;
    while (j < hx_cur_blk_end) {
        int kj;
        kj = h_kind[j];
        if (kj == HI_NOP || kj == HI_PHI) { j = j + 1; continue; }
        /* Aliasing barriers: a store / call between us could be observed
         * (or could clobber state we read).  Stop. */
        if (kj == HI_STORE || hx_pair_mem_barrier(kj)) return -1;
        if (hx_pairable_int_offset_mem(j, HI_LOAD, wide)
                && hx_off_base[j] == base_inst) {
            int diff;
            diff = hx_off_disp[j] - hx_off_disp[idx];
            if (diff == step || diff == -step) {
                if (hx_load_pair_safe(idx, j, wide)) return j;
                /* This particular candidate isn't safe; keep scanning,
                 * a further-out load might still pair (rare). */
            }
        }
        j = j + 1;
    }
    return -1;
}

/* STP analog.  The value-register read constraint is stricter than for
 * loads: we need ra_reg[h_src2[partner]] to still hold its def value at
 * idx's position (no inst between [src2-def, idx] re-uses that color),
 * and no inst in (idx, partner) clobbers it either. */
static int hx_store_pair_safe(int idx, int j, int wide) {
    int color_v_b;
    int v_b;
    int min_off;
    int diff;
    int x;

    /* Stored-value defs must be live at idx's position. */
    if (h_src2[j] < 0) return 0;
    v_b = h_src2[j];
    /* h_src2[j] must be defined before idx (so its value reaches idx). */
    if (v_b >= idx) return 0;

    color_v_b = (ra_reg[v_b] >= 0) ? hx_slot_reg(ra_reg[v_b]) : -1;
    if (color_v_b < 0) return 0;     /* spilled — let normal path reload */

    /* STP allows rt1 == rt2 (storing the same value twice is meaningful);
     * no architectural rt vs base constraint either.  Just imm range. */
    diff = hx_off_disp[j] - hx_off_disp[idx];
    if (diff > 0) min_off = hx_off_disp[idx];
    else          min_off = hx_off_disp[j];
    if (wide) {
        if (min_off < -512 || min_off > 504 || (min_off & 7)) return 0;
    } else {
        if (min_off < -256 || min_off > 252 || (min_off & 3)) return 0;
    }

    /* No inst in (idx, j) may write color_v_b (else partner's value at
     * idx's position is no longer the same as it would have been at j). */
    x = idx + 1;
    while (x < j) {
        int kx;
        kx = h_kind[x];
        if (kx != HI_NOP && kx != HI_PHI) {
            if (ra_reg[x] >= 0 && hx_slot_reg(ra_reg[x]) == color_v_b) return 0;
        }
        x = x + 1;
    }
    /* And between v_b's def and idx, no inst may overwrite color_v_b. */
    x = v_b + 1;
    while (x <= idx) {
        int kx;
        kx = h_kind[x];
        if (kx != HI_NOP && kx != HI_PHI) {
            if (ra_reg[x] >= 0 && hx_slot_reg(ra_reg[x]) == color_v_b
                    && x != v_b) return 0;
        }
        x = x + 1;
    }
    return 1;
}

static int hx_find_store_partner(int idx, int wide) {
    int step;
    int j;
    int base_inst;

    step = wide ? 8 : 4;
    base_inst = hx_off_base[idx];

    j = idx + 1;
    while (j < hx_cur_blk_end) {
        int kj;
        kj = h_kind[j];
        if (kj == HI_NOP || kj == HI_PHI) { j = j + 1; continue; }
        if (kj == HI_LOAD || hx_pair_mem_barrier(kj)) return -1;
        if (kj == HI_STORE) {
            int diff;
            if (hx_pairable_int_offset_mem(j, HI_STORE, wide)
                    && hx_off_base[j] == base_inst) {
                diff = hx_off_disp[j] - hx_off_disp[idx];
                if (diff == step || diff == -step) {
                    if (hx_store_pair_safe(idx, j, wide)) return j;
                }
            }
            return -1;
        }
        j = j + 1;
    }
    return -1;
}

/* ============================================================================
 * Emit one HIR instruction.
 * ============================================================================ */

static void hx_emit_inst(int idx) {
    int k;
    int s1; int s2;
    int wide;
    int dst;
    int r1; int r2;

    k = h_kind[idx];
    if (k == HI_NOP) return;
    if (k == HI_PHI) return;     /* handled at edges via hx_phi_copies */

    /* Rematerializable instructions — no code emitted at the def site;
     * hx_mat()/hx_remat() regenerate them on demand at each use, so an
     * unused def (e.g. an HI_ICONST whose only consumer is a fused
     * comparison that's been NOPed) costs nothing. */
    if (k == HI_ICONST || k == HI_ALLOCA || k == HI_GETFP) return;

    /* Folded into a parent's addressing mode (see hx_fold_indexed_addr). */
    if (bg_fold[idx] && (k == HI_ADD || k == HI_SLL || k == HI_ADDI)) return;

    s1 = h_src1[idx];
    s2 = h_src2[idx];
    wide = hx_is_wide(h_ty[idx]);
    dst = hx_dst_reg(idx);
    (void)wide;
    if (k == HI_GADDR || k == HI_FADDR) {
        cg_emit_adrp_add_to(dst, h_name[idx], 0);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_SADDR) {
        cg_emit_adrp_add_to(dst, CG_STR_PSEUDO_NAME, h_val[idx]);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_A64_MRS_CNTVCT) {
        a64_mrs_cntvct_el0(dst);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_PARAM) {
        /* The value is supposed to already be in its assigned register
         * (regalloc tries to keep PARAM in arg-reg).  If not, the prologue
         * stored it to spill slot — load now. */
        int slot; int reg;
        slot = ra_reg[idx];
        if (slot >= 0) {
            reg = hx_slot_reg(slot);
            /* If reg differs from the AAPCS arg reg, prologue already MOV'd. */
            (void)reg;
        }
        hx_spill(idx, hx_slot_reg(ra_reg[idx] >= 0 ? ra_reg[idx] : 0));
        /* Actually, hx_spill checks ra_reg first — if assigned, it returns. */
        return;
    }

    /* ---------- Loads / stores ---------- */
    if (k == HI_LOAD) {
        int addr_reg;
        int ty;
        int pat;
        int lnt;
        ty = h_ty[idx];
        pat = bg_sel[idx];
        lnt = (pat >= 0) ? bg_plnt[pat] : -1;

        /* V-class load: separate path with FP-form ldr s/d.  Passing a
         * V-reg encoding to a64_ldr_w_imm would target the X file at the
         * same number — wrong physical register.  Indexed-addr fold is
         * already gated off for FP loads in hx_fold_indexed_addr; the
         * other paths reuse the int versions' structure. */
        if (ty_is_fp(ty)) {
            int is_d;
            is_d = ty_is_double(ty);

            /* Offset-displacement fold (Pattern A from
             * hx_fold_indexed_addr). */
            if (hx_off_base[idx] >= 0) {
                addr_reg = hx_get_src(hx_off_base[idx], HX_SCRATCH1);
                hx_fp_load_at(dst, addr_reg, hx_off_disp[idx], is_d);
                hx_spill(idx, dst);
                return;
            }
            /* BG_SADDR: symbol+offset chain. */
            if (lnt == BG_SADDR && s1 >= 0 && bg_fold[s1] && bg_ssym[s1] >= 0) {
                int sym_inst; int sym_off;
                sym_inst = bg_ssym[s1];
                sym_off  = bg_soff[s1];
                if (h_kind[sym_inst] == HI_SADDR) {
                    cg_emit_adrp_add_to(HX_SCRATCH1, CG_STR_PSEUDO_NAME,
                                        h_val[sym_inst] + sym_off);
                } else {
                    cg_emit_adrp_add_to(HX_SCRATCH1, h_name[sym_inst], sym_off);
                }
                hx_fp_load_at(dst, HX_SCRATCH1, 0, is_d);
                hx_spill(idx, dst);
                return;
            }
            /* BG_MEM: frame-relative. */
            if (lnt == BG_MEM && s1 >= 0) {
                hx_fp_load_at(dst, A64_X29, bg_foff[s1], is_d);
                hx_spill(idx, dst);
                return;
            }
            /* General path: address in src1. */
            addr_reg = hx_get_src(s1, HX_SCRATCH1);
            hx_fp_load_at(dst, addr_reg, 0, is_d);
            hx_spill(idx, dst);
            return;
        }

        /* Indexed addressing: hx_fold_indexed_addr decided this LOAD's
         * address is base + (idx << shift) and folded the ADD+SLL. */
        if (hx_ridx_base[idx] >= 0) {
            int base_r; int idx_r;
            base_r = hx_get_src(hx_ridx_base[idx], HX_SCRATCH1);
            idx_r  = hx_get_src(hx_ridx_idx[idx],  HX_SCRATCH2);
            if (hx_is_wide(ty)) {
                a64_ldr_x_reg_lsl(dst, base_r, idx_r, hx_ridx_shift[idx]);
            } else {
                a64_ldr_w_reg_lsl(dst, base_r, idx_r, hx_ridx_shift[idx]);
            }
            hx_spill(idx, dst);
            return;
        }

        /* Offset-displacement fold: address is base + #const. */
        if (hx_off_base[idx] >= 0) {
            int base_r;
            int wide_a;
            int pairable;
            int partner;
            wide_a = hx_is_wide(ty);
            pairable = !ty_is_fp(ty) && hx_access_size_for_ty(ty) >= 4;
            base_r = hx_get_src(hx_off_base[idx], HX_SCRATCH1);
            partner = pairable ? hx_find_load_partner(idx, wide_a) : -1;
            if (partner >= 0) {
                int part_dst;
                int diff;
                int min_off;
                int rt1; int rt2;
                part_dst = hx_slot_reg(ra_reg[partner]);
                diff = hx_off_disp[partner] - hx_off_disp[idx];
                if (diff > 0) {
                    min_off = hx_off_disp[idx];
                    rt1 = dst; rt2 = part_dst;
                } else {
                    min_off = hx_off_disp[partner];
                    rt1 = part_dst; rt2 = dst;
                }
                if (wide_a) a64_ldp_x_off(rt1, rt2, base_r, min_off);
                else        a64_ldp_w_off(rt1, rt2, base_r, min_off);
                hx_spill(idx, dst);
                hx_spill(partner, part_dst);
                hx_skip_emit[partner] = 1;
                return;
            }
            if (ty_is_fp(ty)) hx_fp_load_at(dst, base_r, hx_off_disp[idx], ty_is_double(ty));
            else              hx_int_load_at(dst, base_r, hx_off_disp[idx], ty);
            hx_spill(idx, dst);
            return;
        }

        /* BURG selected BG_MEM addressing mode → frame-relative LDR.
         * lnt == BG_MEM matches when src1 is HI_ALLOCA or HI_ADDI (chain),
         * which is exactly when bg_foff[s1] is meaningful.  Even if
         * bg_select didn't fold s1 (ALLOCA is remat-skipped), the
         * fast-path is correct. */
        /* BG_SADDR lhs: src1 chain produces a symbol+offset address.
         * Only do the integrated ADRP+ADD+LDR when src1 is actually folded
         * (e.g., bg_select NOPed an HI_ADDI that accumulates into the
         * symbol address).  When src1 is a bare HI_GADDR/HI_SADDR/HI_FADDR
         * with bg_uses>1 the value lives in its own register; fall through
         * to the general path so we don't emit a duplicate ADRP+ADD. */
        if (lnt == BG_SADDR && s1 >= 0 && bg_fold[s1] && bg_ssym[s1] >= 0) {
            int sym_inst;
            int sym_off;
            sym_inst = bg_ssym[s1];
            sym_off  = bg_soff[s1];
            if (h_kind[sym_inst] == HI_SADDR) {
                cg_emit_adrp_add_to(HX_SCRATCH1, CG_STR_PSEUDO_NAME,
                                    h_val[sym_inst] + sym_off);
            } else {
                cg_emit_adrp_add_to(HX_SCRATCH1, h_name[sym_inst], sym_off);
            }
            if (hx_is_wide(ty)) a64_ldr_x_imm(dst, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                if (ty & TY_UNSIGNED) a64_ldrb_imm(dst, HX_SCRATCH1, 0);
                else                  a64_ldrsb_w_imm(dst, HX_SCRATCH1, 0);
            }
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
                if (ty & TY_UNSIGNED) a64_ldrh_imm(dst, HX_SCRATCH1, 0);
                else                  a64_ldrsh_w_imm(dst, HX_SCRATCH1, 0);
            }
            else { a64_ldr_w_imm(dst, HX_SCRATCH1, 0); }
            hx_spill(idx, dst);
            return;
        }
        if (lnt == BG_MEM && s1 >= 0) {
            int frame_off;
            frame_off = bg_foff[s1];
            /* Positive aligned scaled-offset form (LDR ..., [Xn, #imm12*scale]). */
            if (frame_off >= 0 && frame_off <= 32760) {
                if (hx_is_wide(ty)) { a64_ldr_x_imm(dst, A64_X29, frame_off); }
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                    if (ty & TY_UNSIGNED) a64_ldrb_imm(dst, A64_X29, frame_off);
                    else                  a64_ldrsb_w_imm(dst, A64_X29, frame_off);
                }
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
                    if (ty & TY_UNSIGNED) a64_ldrh_imm(dst, A64_X29, frame_off);
                    else                  a64_ldrsh_w_imm(dst, A64_X29, frame_off);
                }
                else { a64_ldr_w_imm(dst, A64_X29, frame_off); }
                hx_spill(idx, dst);
                return;
            }
            /* Unscaled signed-offset form for negative frame offsets
             * (FP-N), imm9 in [-256, 255]. */
            if (frame_off >= -256 && frame_off <= 255) {
                if (hx_is_wide(ty)) { a64_ldur_x_imm(dst, A64_X29, frame_off); }
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                    if (ty & TY_UNSIGNED) a64_ldurb_imm(dst, A64_X29, frame_off);
                    else                  a64_ldursb_w_imm(dst, A64_X29, frame_off);
                }
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
                    if (ty & TY_UNSIGNED) a64_ldurh_imm(dst, A64_X29, frame_off);
                    else                  a64_ldursh_w_imm(dst, A64_X29, frame_off);
                }
                else { a64_ldur_w_imm(dst, A64_X29, frame_off); }
                hx_spill(idx, dst);
                return;
            }
            /* Out of imm9 range: compute FP+offset into scratch then LDR. */
            hx_emit_frame_addr_bytes(HX_SCRATCH1, frame_off);
            if (hx_is_wide(ty)) a64_ldr_x_imm(dst, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                if (ty & TY_UNSIGNED) a64_ldrb_imm(dst, HX_SCRATCH1, 0);
                else                  a64_ldrsb_w_imm(dst, HX_SCRATCH1, 0);
            }
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
                if (ty & TY_UNSIGNED) a64_ldrh_imm(dst, HX_SCRATCH1, 0);
                else                  a64_ldrsh_w_imm(dst, HX_SCRATCH1, 0);
            }
            else a64_ldr_w_imm(dst, HX_SCRATCH1, 0);
            hx_spill(idx, dst);
            return;
        }

        /* General path: address into scratch1, then LDR. */
        addr_reg = hx_get_src(s1, HX_SCRATCH1);
        if (hx_is_wide(ty)) a64_ldr_x_imm(dst, addr_reg, 0);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
            if (ty & TY_UNSIGNED) a64_ldrb_imm(dst, addr_reg, 0);
            else                  a64_ldrsb_w_imm(dst, addr_reg, 0);
        }
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
            if (ty & TY_UNSIGNED) a64_ldrh_imm(dst, addr_reg, 0);
            else                  a64_ldrsh_w_imm(dst, addr_reg, 0);
        }
        else a64_ldr_w_imm(dst, addr_reg, 0);
        hx_spill(idx, dst);
        return;
    }

    if (k == HI_STORE) {
        int addr_reg; int val_reg;
        int ty;
        int pat;
        int lnt;
        ty = h_ty[idx];   /* type of stored value */
        pat = bg_sel[idx];
        lnt = (pat >= 0) ? bg_plnt[pat] : -1;

        /* V-class store: dispatch to FP-form str s/d.  Mirrors the
         * V-class HI_LOAD path above. */
        if (ty_is_fp(ty)) {
            int is_d;
            int v_r;
            is_d = ty_is_double(ty);
            v_r  = hx_get_src(s2, HX_SCRATCH_V1);

            /* Offset-displacement fold. */
            if (hx_off_base[idx] >= 0) {
                addr_reg = hx_get_src(hx_off_base[idx], HX_SCRATCH1);
                hx_fp_store_at(v_r, addr_reg, hx_off_disp[idx], is_d);
                return;
            }
            /* BG_SADDR. */
            if (lnt == BG_SADDR && s1 >= 0 && bg_fold[s1] && bg_ssym[s1] >= 0) {
                int sym_inst; int sym_off;
                sym_inst = bg_ssym[s1];
                sym_off  = bg_soff[s1];
                if (h_kind[sym_inst] == HI_SADDR) {
                    cg_emit_adrp_add_to(HX_SCRATCH1, CG_STR_PSEUDO_NAME,
                                        h_val[sym_inst] + sym_off);
                } else {
                    cg_emit_adrp_add_to(HX_SCRATCH1, h_name[sym_inst], sym_off);
                }
                hx_fp_store_at(v_r, HX_SCRATCH1, 0, is_d);
                return;
            }
            /* BG_MEM. */
            if (lnt == BG_MEM && s1 >= 0) {
                hx_fp_store_at(v_r, A64_X29, bg_foff[s1], is_d);
                return;
            }
            /* General path. */
            addr_reg = hx_get_src(s1, HX_SCRATCH1);
            hx_fp_store_at(v_r, addr_reg, 0, is_d);
            return;
        }

        /* Indexed addressing: see LOAD branch above. */
        if (hx_ridx_base[idx] >= 0) {
            int base_r; int idx_r; int v_r;
            base_r = hx_get_src(hx_ridx_base[idx], HX_SCRATCH1);
            idx_r  = hx_get_src(hx_ridx_idx[idx],  HX_SCRATCH2);
            v_r    = hx_get_src(s2, HX_SCRATCH1);
            if (hx_is_wide(ty)) {
                a64_str_x_reg_lsl(v_r, base_r, idx_r, hx_ridx_shift[idx]);
            } else {
                a64_str_w_reg_lsl(v_r, base_r, idx_r, hx_ridx_shift[idx]);
            }
            return;
        }

        /* Offset-displacement fold: address is base + #const. */
        if (hx_off_base[idx] >= 0) {
            int base_r; int v_r;
            int wide_a;
            int pairable;
            int partner;
            wide_a = hx_is_wide(ty);
            pairable = !ty_is_fp(ty) && hx_access_size_for_ty(ty) >= 4;
            base_r = hx_get_src(hx_off_base[idx], HX_SCRATCH1);
            v_r    = hx_get_src(s2, HX_SCRATCH2);
            partner = pairable ? hx_find_store_partner(idx, wide_a) : -1;
            if (partner >= 0) {
                int part_v;
                int diff;
                int min_off;
                int rt1; int rt2;
                /* Partner's value reg is already in its assigned color
                 * (we verified safety above — color_v_b is live). */
                part_v = hx_slot_reg(ra_reg[h_src2[partner]]);
                diff = hx_off_disp[partner] - hx_off_disp[idx];
                if (diff > 0) {
                    min_off = hx_off_disp[idx];
                    rt1 = v_r; rt2 = part_v;
                } else {
                    min_off = hx_off_disp[partner];
                    rt1 = part_v; rt2 = v_r;
                }
                if (wide_a) a64_stp_x_off(rt1, rt2, base_r, min_off);
                else        a64_stp_w_off(rt1, rt2, base_r, min_off);
                hx_skip_emit[partner] = 1;
                return;
            }
            if (ty_is_fp(ty)) hx_fp_store_at(v_r, base_r, hx_off_disp[idx], ty_is_double(ty));
            else              hx_int_store_at(v_r, base_r, hx_off_disp[idx], ty);
            return;
        }

        /* BG_SADDR lhs: emit ADRP+ADD (with addend), then STR.  Only when
         * src1 is actually folded — see LOAD comment above. */
        if (lnt == BG_SADDR && s1 >= 0 && bg_fold[s1] && bg_ssym[s1] >= 0) {
            int sym_inst;
            int sym_off;
            sym_inst = bg_ssym[s1];
            sym_off  = bg_soff[s1];
            if (h_kind[sym_inst] == HI_SADDR) {
                cg_emit_adrp_add_to(HX_SCRATCH1, CG_STR_PSEUDO_NAME,
                                    h_val[sym_inst] + sym_off);
            } else {
                cg_emit_adrp_add_to(HX_SCRATCH1, h_name[sym_inst], sym_off);
            }
            val_reg = hx_get_src(s2, HX_SCRATCH2);
            if (hx_is_wide(ty))         a64_str_x_imm(val_reg, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
                a64_strb_imm(val_reg, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
                a64_strh_imm(val_reg, HX_SCRATCH1, 0);
            else a64_str_w_imm(val_reg, HX_SCRATCH1, 0);
            return;
        }
        /* BG_MEM lhs → frame-relative STR.  See LOAD comment. */
        if (lnt == BG_MEM && s1 >= 0) {
            int frame_off;
            frame_off = bg_foff[s1];
            if (frame_off >= 0 && frame_off <= 32760) {
                val_reg = hx_get_src(s2, HX_SCRATCH1);
                if (hx_is_wide(ty))         a64_str_x_imm(val_reg, A64_X29, frame_off);
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
                    a64_strb_imm(val_reg, A64_X29, frame_off);
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
                    a64_strh_imm(val_reg, A64_X29, frame_off);
                else a64_str_w_imm(val_reg, A64_X29, frame_off);
                return;
            }
            if (frame_off >= -256 && frame_off <= 255) {
                val_reg = hx_get_src(s2, HX_SCRATCH1);
                if (hx_is_wide(ty))         a64_stur_x_imm(val_reg, A64_X29, frame_off);
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
                    a64_sturb_imm(val_reg, A64_X29, frame_off);
                else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
                    a64_sturh_imm(val_reg, A64_X29, frame_off);
                else a64_stur_w_imm(val_reg, A64_X29, frame_off);
                return;
            }
            /* Out of imm9 range: compute FP+offset into scratch1 then STR.
             * (val into scratch2 since scratch1 holds the address.) */
            hx_emit_frame_addr_bytes(HX_SCRATCH1, frame_off);
            val_reg = hx_get_src(s2, HX_SCRATCH2);
            if (hx_is_wide(ty))         a64_str_x_imm(val_reg, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
                a64_strb_imm(val_reg, HX_SCRATCH1, 0);
            else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
                a64_strh_imm(val_reg, HX_SCRATCH1, 0);
            else a64_str_w_imm(val_reg, HX_SCRATCH1, 0);
            return;
        }

        addr_reg = hx_get_src(s1, HX_SCRATCH1);
        val_reg  = hx_get_src(s2, HX_SCRATCH2);
        if (hx_is_wide(ty))         a64_str_x_imm(val_reg, addr_reg, 0);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR)
            a64_strb_imm(val_reg, addr_reg, 0);
        else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT)
            a64_strh_imm(val_reg, addr_reg, 0);
        else a64_str_w_imm(val_reg, addr_reg, 0);
        return;
    }

    /* ---------- Binary arithmetic / logic ---------- */
    if (k == HI_ADD || k == HI_SUB || k == HI_MUL || k == HI_DIV || k == HI_REM
     || k == HI_AND || k == HI_OR  || k == HI_XOR
     || k == HI_SLL || k == HI_SRA || k == HI_SRL) {

        /* Wide-form fast paths: the lower wraps a 32-bit literal in
         * HI_SEXT32 / HI_ZEXT32 to widen it for a 64-bit op, so the bare
         * `h_kind == HI_ICONST` checks further down miss it.  Peek
         * through and try the X-form imm encoders.  Covers ADD/SUB
         * imm12 (with negate) and AND/OR/XOR logical-imm. */
        if (wide && (k == HI_ADD || k == HI_SUB
                  || k == HI_AND || k == HI_OR || k == HI_XOR)) {
            int v_; long long llv;
            int ok = 0;

            /* s2-as-constant. */
            if (hx_peek_iconst(s2, &v_, &llv)) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (k == HI_ADD) {
                    ok = hx_emit_add_delta(dst, r1, llv, 1);
                } else if (k == HI_SUB) {
                    ok = hx_emit_add_delta(dst, r1, 0 - llv, 1);
                } else if (k == HI_AND) ok = a64_and_x_imm(dst, r1, llv);
                else if   (k == HI_OR ) ok = a64_orr_x_imm(dst, r1, llv);
                else                    ok = a64_eor_x_imm(dst, r1, llv);
            }

            /* Symmetric: s1-as-constant, only for commutative ops
             * (ADD/AND/OR/XOR — NOT SUB). */
            if (!ok && k != HI_SUB && hx_peek_iconst(s1, &v_, &llv)) {
                r1 = hx_get_src(s2, HX_SCRATCH1);
                if (k == HI_ADD) {
                    ok = hx_emit_add_delta(dst, r1, llv, 1);
                }
                else if (k == HI_AND) ok = a64_and_x_imm(dst, r1, llv);
                else if (k == HI_OR ) ok = a64_orr_x_imm(dst, r1, llv);
                else                  ok = a64_eor_x_imm(dst, r1, llv);
            }
            if (ok) { hx_spill(idx, dst); return; }
        }

        /* Immediate-form fast paths (ADD/SUB imm12, shifts imm6).
         * AArch64 ADD/SUB take an unsigned 12-bit imm (optionally LSL #12);
         * we handle the [0, 4095] and [-4095, -1] cases without LSL.  For
         * shifts the count must fit in [0, 31] (W) or [0, 63] (X). */
        if (s2 >= 0 && h_kind[s2] == HI_ICONST) {
            int v;
            int max_shift;
            v = h_val[s2];
            max_shift = wide ? 63 : 31;

            if (k == HI_ADD) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (hx_emit_add_delta(dst, r1, v, wide)) {
                    hx_spill(idx, dst); return;
                }
            } else if (k == HI_SUB) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (hx_emit_add_delta(dst, r1, 0 - (long long)v, wide)) {
                    hx_spill(idx, dst); return;
                }
            } else if (k == HI_SLL && v >= 0 && v <= max_shift) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) a64_lsl_x_imm(dst, r1, v);
                else      a64_lsl_w_imm(dst, r1, v);
                hx_spill(idx, dst); return;
            } else if (k == HI_SRL && v >= 0 && v <= max_shift) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) a64_lsr_x_imm(dst, r1, v);
                else      a64_lsr_w_imm(dst, r1, v);
                hx_spill(idx, dst); return;
            } else if (k == HI_SRA && v >= 0 && v <= max_shift) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) a64_asr_x_imm(dst, r1, v);
                else      a64_asr_w_imm(dst, r1, v);
                hx_spill(idx, dst); return;
            } else if (k == HI_AND) {
                /* AArch64 logical-immediate encoding (bit-pattern imm). */
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) {
                    if (a64_and_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_and_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            } else if (k == HI_OR) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) {
                    if (a64_orr_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_orr_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            } else if (k == HI_XOR) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (wide) {
                    if (a64_eor_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_eor_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            }
            /* Fall through to register-register form. */
        }
        /* Symmetric: ICONST on the LEFT for commutative ops. */
        if (s1 >= 0 && h_kind[s1] == HI_ICONST) {
            int v;
            v = h_val[s1];
            if (k == HI_ADD) {
                r1 = hx_get_src(s2, HX_SCRATCH1);
                if (hx_emit_add_delta(dst, r1, v, wide)) {
                    hx_spill(idx, dst); return;
                }
            } else if (k == HI_AND) {
                r1 = hx_get_src(s2, HX_SCRATCH1);
                if (wide) {
                    if (a64_and_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_and_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            } else if (k == HI_OR) {
                r1 = hx_get_src(s2, HX_SCRATCH1);
                if (wide) {
                    if (a64_orr_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_orr_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            } else if (k == HI_XOR) {
                r1 = hx_get_src(s2, HX_SCRATCH1);
                if (wide) {
                    if (a64_eor_x_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                } else {
                    if (a64_eor_w_imm(dst, r1, v)) { hx_spill(idx, dst); return; }
                }
            }
        }

        r1 = hx_get_src(s1, HX_SCRATCH1);
        r2 = hx_get_src(s2, HX_SCRATCH2);
        if (k == HI_ADD) {
            if (wide) a64_add_x(dst, r1, r2);
            else      a64_add_w(dst, r1, r2);
        } else if (k == HI_SUB) {
            if (wide) a64_sub_x(dst, r1, r2);
            else      a64_sub_w(dst, r1, r2);
        } else if (k == HI_MUL) {
            if (wide) a64_mul_x(dst, r1, r2);
            else      a64_mul_w(dst, r1, r2);
        } else if (k == HI_DIV) {
            if (wide) {
                if (h_ty[idx] & TY_UNSIGNED) a64_udiv_x(dst, r1, r2);
                else                         a64_sdiv_x(dst, r1, r2);
            } else {
                if (h_ty[idx] & TY_UNSIGNED) a64_udiv_w(dst, r1, r2);
                else                         a64_sdiv_w(dst, r1, r2);
            }
        } else if (k == HI_REM) {
            /* dst = r1 - (r1/r2)*r2.  Compute quotient into scratch, MSUB. */
            if (wide) {
                if (h_ty[idx] & TY_UNSIGNED) a64_udiv_x(HX_SCRATCH1, r1, r2);
                else                         a64_sdiv_x(HX_SCRATCH1, r1, r2);
                /* MSUB Xd, Xn, Xm, Xa: 1 00 11011 000 Rm 1 Ra Rn Rd */
                a64_inst(0x9B008000
                         | ((r2 & 0x1F) << 16)
                         | ((r1 & 0x1F) << 10)
                         | ((HX_SCRATCH1 & 0x1F) << 5)
                         |  (dst & 0x1F));
            } else {
                if (h_ty[idx] & TY_UNSIGNED) a64_udiv_w(HX_SCRATCH1, r1, r2);
                else                         a64_sdiv_w(HX_SCRATCH1, r1, r2);
                a64_msub_w(dst, HX_SCRATCH1, r2, r1);
            }
        } else if (k == HI_AND) {
            if (hx_bic_fuse[idx]) {
                if (wide) a64_bic_x(dst, r1, r2);
                else      a64_bic_w(dst, r1, r2);
            } else {
                if (wide) a64_and_x(dst, r1, r2);
                else      a64_and_w(dst, r1, r2);
            }
        } else if (k == HI_OR) {
            if (hx_bic_fuse[idx]) {
                if (wide) a64_orn_x(dst, r1, r2);
                else      a64_orn_w(dst, r1, r2);
            } else {
                if (wide) a64_orr_x(dst, r1, r2);
                else      a64_orr_w(dst, r1, r2);
            }
        } else if (k == HI_XOR) {
            if (hx_bic_fuse[idx]) {
                if (wide) a64_eon_x(dst, r1, r2);
                else      a64_eon_w(dst, r1, r2);
            } else {
                if (wide) a64_eor_x(dst, r1, r2);
                else      a64_eor_w(dst, r1, r2);
            }
        } else if (k == HI_SLL) {
            if (wide) a64_lslv_x(dst, r1, r2);
            else      a64_lslv_w(dst, r1, r2);
        } else if (k == HI_SRA) {
            if (wide) a64_asrv_x(dst, r1, r2);
            else      a64_asrv_w(dst, r1, r2);
        } else if (k == HI_SRL) {
            if (wide) a64_lsrv_x(dst, r1, r2);
            else      a64_lsrv_w(dst, r1, r2);
        }
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Comparisons → 0/1 in dst ---------- */
    if (hx_is_cmp_op(k)) {
        int cmp64;
        int v;
        cmp64 = hx_is_wide(h_ty[s1]) || hx_is_wide(h_ty[s2]);
        if (s2 >= 0 && h_kind[s2] == HI_ICONST) {
            v = h_val[s2];
            if (v >= 0 && v <= 4095) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (cmp64) a64_cmp_x_imm(r1, v);
                else       a64_cmp_w_imm(r1, v);
                a64_cset_w(dst, hx_cmp_cond(k));
                hx_spill(idx, dst); return;
            }
            if (v < 0 && v >= -4095) {
                r1 = hx_get_src(s1, HX_SCRATCH1);
                if (cmp64) a64_cmn_x_imm(r1, -v);
                else       a64_cmn_w_imm(r1, -v);
                a64_cset_w(dst, hx_cmp_cond(k));
                hx_spill(idx, dst); return;
            }
        }
        r1 = hx_get_src(s1, HX_SCRATCH1);
        r2 = hx_get_src(s2, HX_SCRATCH2);
        if (cmp64) a64_cmp_x(r1, r2);
        else       a64_cmp_w(r1, r2);
        a64_cset_w(dst, hx_cmp_cond(k));
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Unary ops ---------- */
    if (k == HI_NEG) {
        r1 = hx_get_src(s1, HX_SCRATCH1);
        if (wide) a64_sub_x(dst, A64_XZR, r1);
        else      a64_neg_w(dst, r1);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_NOT) {
        /* Logical NOT: dst = (src == 0) ? 1 : 0.
         * CMP src, #0 ; CSET dst, EQ. */
        int src_wide;
        src_wide = hx_is_wide(h_ty[s1]);
        r1 = hx_get_src(s1, HX_SCRATCH1);
        if (src_wide) a64_cmp_x_imm(r1, 0);
        else          a64_cmp_w_imm(r1, 0);
        a64_cset_w(dst, A64_COND_EQ);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_BNOT) {
        /* Bitwise NOT via MVN (W) or ORN XZR,Xm (X). */
        r1 = hx_get_src(s1, HX_SCRATCH1);
        if (wide) {
            /* ORN Xd, XZR, Rm — mvn x form. */
            a64_inst(0xAA2003E0 | ((r1 & 0x1F) << 16) | (dst & 0x1F));
        } else {
            a64_mvn_w(dst, r1);
        }
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Sign / zero extension (32 → 64) ---------- */
    if (k == HI_SEXT32) {
        r1 = hx_get_src(s1, HX_SCRATCH1);
        a64_sxtw(dst, r1);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_ZEXT32) {
        r1 = hx_get_src(s1, HX_SCRATCH1);
        /* Zero-extend 32 → 64: MOV Wd, Wn (writes upper-half clear). */
        a64_mov_w(dst, r1);
        hx_spill(idx, dst);
        return;
    }

    /* ---------- ADDI ---------- */
    if (k == HI_ADDI) {
        int v;
        v = h_val[idx];
        r1 = hx_get_src(s1, HX_SCRATCH1);
        if (!hx_emit_add_delta(dst, r1, v, 1)) {
            /* Large constant: materialise into scratch then ADD. */
            hx_mat_iconst(HX_SCRATCH2, v, 1);
            a64_add_x(dst, r1, HX_SCRATCH2);
        }
        hx_spill(idx, dst);
        return;
    }

    /* ---------- COPY ---------- */
    if (k == HI_COPY) {
        if (hx_is_v(idx)) {
            r1 = hx_get_src(s1, HX_SCRATCH_V1);
            if (r1 != dst) {
                if (ty_is_double(h_ty[idx])) a64_fmov_d_d(dst, r1);
                else                         a64_fmov_s_s(dst, r1);
            }
        } else {
            r1 = hx_get_src(s1, HX_SCRATCH1);
            if (r1 != dst) {
                if (wide) a64_mov_x(dst, r1);
                else      a64_mov_w(dst, r1);
            }
        }
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Control flow ---------- */
    if (k == HI_BR) {
        /* h_val[idx] = target block.  PHI moves at edge.  If the target
         * is the next block in RPO order, no `b` is needed (fall-through). */
        int blk;
        blk = h_val[idx];
        hx_phi_copies(h_blk[idx], blk);
        if (blk != hx_next_blk) hx_jump_to_block(blk);
        return;
    }
    if (k == HI_BRC) {
        /* Conditional branch.  Encoding (from hir_lower.h):
         *   src1 = condition value
         *   src2 = then_blk (taken if cond != 0)
         *   val  = else_blk (taken if cond == 0)
         *
         * Layout (mirrors x64 sibling):
         *    <CMP/CSET>
         *    b.cond/cbz  <patch>           ; jumps to NOT-taken on FALSE
         *    <phi copies for taken>
         *    b taken
         *  <patch lands here>:
         *    <phi copies for notaken>
         *    b notaken
         */
        int taken; int notaken;
        int cmp_idx;
        int site;
        int word; int diff; int kept; int imm19;
        int direct_notaken;

        taken    = s2;
        notaken  = h_val[idx];
        cmp_idx  = hx_brc_fuse[idx];
        site     = a64_off;
        /* Direct branch on the not-taken edge: when `taken` is the next RPO
         * block we fall through to it, and as long as the not-taken edge
         * carries no phi we can branch straight to it without staging a
         * patched B.cond + intermediate phi-copies block.  The phi-free
         * gate is what makes this sound — see direct_notaken returns below. */
        direct_notaken = (taken == hx_next_blk && !hx_edge_has_phi(h_blk[idx], notaken));

        if (cmp_idx >= 0) {
            /* Fused: emit (F)CMP on the original comparison's operands,
             * then B.cond(inverted) → not-taken. */
            int ck; int ca; int cb;
            int s1r; int s2r;
            int cmp_wide;
            int inv_cc;

            ck = hx_cmp_kind[cmp_idx];
            ca = h_src1[cmp_idx];
            cb = h_src2[cmp_idx];

            if (hx_is_fcmp_op(ck)) {
                int is_d;
                is_d = ty_is_double(h_ty[ca]);
                s1r = hx_get_src(ca, HX_SCRATCH_V1);
                s2r = hx_get_src(cb, HX_SCRATCH_V2);
                if (is_d) a64_fcmp_d(s1r, s2r);
                else      a64_fcmp_s(s1r, s2r);
                inv_cc = hx_fcmp_inv_cond(ck);
            } else {
                cmp_wide = hx_is_wide(h_ty[ca]) || hx_is_wide(h_ty[cb]);

                /* CMP with imm folds into CMP/CMN imm12.  Negative immediates
                 * use CMN (ADDS XZR/WZR, Wn, #imm) which sets the same flags
                 * as `cmp Wn, #-imm`. */
                if (cb >= 0 && h_kind[cb] == HI_ICONST &&
                    h_val[cb] >= 0 && h_val[cb] <= 4095) {
                    s1r = hx_get_src(ca, HX_SCRATCH1);
                    if (cmp_wide) a64_cmp_x_imm(s1r, h_val[cb]);
                    else          a64_cmp_w_imm(s1r, h_val[cb]);
                } else if (cb >= 0 && h_kind[cb] == HI_ICONST &&
                           h_val[cb] < 0 && h_val[cb] >= -4095) {
                    s1r = hx_get_src(ca, HX_SCRATCH1);
                    if (cmp_wide) a64_cmn_x_imm(s1r, -h_val[cb]);
                    else          a64_cmn_w_imm(s1r, -h_val[cb]);
                } else {
                    s1r = hx_get_src(ca, HX_SCRATCH1);
                    s2r = hx_get_src(cb, HX_SCRATCH2);
                    if (cmp_wide) a64_cmp_x(s1r, s2r);
                    else          a64_cmp_w(s1r, s2r);
                }
                inv_cc = hx_cmp_inv_cond(ck);
            }
            if (direct_notaken) {
                /* B.cond → notaken; fall through into taken-edge phi copies
                 * and then into taken's first inst.  No notaken phi copies
                 * are needed (gate above filtered that case out). */
                hx_bcond_to_block(inv_cc, notaken);
                hx_phi_copies(h_blk[idx], taken);
                return;
            }
            site = a64_off;
            a64_b_cond(inv_cc, 0);   /* placeholder */
        } else {
            /* Unfused: cond materialised as 0/1 — CBZ to not-taken on 0. */
            int cond_reg;
            cond_reg = hx_get_src(s1, HX_SCRATCH1);
            if (direct_notaken) {
                /* Symmetric to the fused branch above; same gate, same
                 * fall-through-into-taken-edge-phi-copies layout. */
                hx_cbz_to_block(cond_reg, notaken);
                hx_phi_copies(h_blk[idx], taken);
                return;
            }
            site = a64_off;
            a64_cbz_w(cond_reg, 0);  /* placeholder */
        }

        /* Taken path: phi copies + jump.  We can't elide this even when
         * taken == next_blk, because the not-taken section is laid down
         * between us and the next block. */
        hx_phi_copies(h_blk[idx], taken);
        hx_jump_to_block(taken);

        /* Patch the conditional branch (B.cond or CBZ — same encoding
         * shape: imm19 in bits [23:5]). */
        diff = a64_off - site;
        word = a64_read_inst(site);
        kept = word & 0xFF00001F;
        imm19 = (diff >> 2) & 0x7FFFF;
        a64_patch_inst(site, kept | (imm19 << 5));

        /* Not-taken path: phi copies + jump (elide if notaken is next block). */
        hx_phi_copies(h_blk[idx], notaken);
        if (notaken != hx_next_blk) hx_jump_to_block(notaken);
        return;
    }
    if (k == HI_RET) {
        /* If a preceding HI_CALL was tail-call-eliminated, the b-target
         * has already been emitted; this RET is dead. */
        if (hx_tce_ret[idx]) return;
        /* If there's a return value, ensure it's in the ABI return reg
         * for its class — X0 for int/ptr, V0 for float/double. */
        if (s1 >= 0) {
            int slot;
            int ret_reg;
            ret_reg = hx_is_v(s1) ? A64_V0 : A64_X0;
            slot = ra_reg[s1];
            if (slot >= 0 && hx_slot_reg(slot) == ret_reg) {
                /* Already there. */
            } else {
                hx_mat(s1, ret_reg);
            }
        }
        /* In no-frame leaves the epilogue is just RET.  Two cases:
         *   - Not the final block (hx_next_blk != -1): emit RET in place,
         *     skipping the branch-to-epilogue + epilogue-RET pair.
         *   - The final block (hx_next_blk == -1): emit nothing and fall
         *     through; the function trailer's `ret` will run next. */
        if (hx_no_frame) {
            if (hx_next_blk != -1) a64_ret();
            return;
        }
        /* Branch to epilogue.  Use bpatch with blk = -1. */
        hx_bpatch_blk[hx_nbpatch] = -1;
        hx_bpatch_off[hx_nbpatch] = a64_off;
        hx_nbpatch = hx_nbpatch + 1;
        a64_b(0);
        return;
    }

    /* ---------- Calls ---------- */
    if (k == HI_CALL || k == HI_CALLP) {
        /* Builtin: __syscall(nr, a0..a5). */
        if (k == HI_CALL && h_name[idx] != 0
         && h_name[idx][0] == '_'
         && h_name[idx][1] == '_'
         && h_name[idx][2] == 's' && h_name[idx][3] == 'y'
         && h_name[idx][4] == 's' && h_name[idx][5] == 'c') {
            /* __syscall builtin */
            int base; int nargs; int j;
            int sc_regs[7];
            sc_regs[0] = A64_X8;
            sc_regs[1] = A64_X0;
            sc_regs[2] = A64_X1;
            sc_regs[3] = A64_X2;
            sc_regs[4] = A64_X3;
            sc_regs[5] = A64_X4;
            sc_regs[6] = A64_X5;

            base = h_cbase[idx];
            nargs = h_val[idx];
            j = 0;
            while (j < nargs && j < 7) {
                int arg_inst;
                arg_inst = h_carg[base + j];
                hx_mat(arg_inst, sc_regs[j]);
                j = j + 1;
            }
            a64_svc(0);
            /* Result in x0. */
            if (dst != A64_X0) a64_mov_x(dst, A64_X0);
            hx_spill(idx, dst);
            return;
        }

        /* Tail-call elimination: marshal args, tear down our frame,
         * then `b target`.  hx_identify_tce has verified preconditions
         * (direct call, nargs <= 8, no alloca/varargs/DBT, RET follows). */
        if (k == HI_CALL && hx_tce_call[idx]) {
            int base; int nargs;
            base = h_cbase[idx];
            nargs = h_val[idx];
            hx_marshal_call_args(base, nargs);
            hx_emit_teardown();
            cg_cpatch_add(h_name[idx], a64_off, A64K_JUMP26, 0);
            a64_b(0);
            /* No hx_spill: the trailing HI_RET (which would consume the
             * call's value) is NOPed via hx_tce_ret[]. */
            return;
        }

        /* Regular call.  AAPCS64: int args fill X0..X7 via NGRN, FP args
         * fill V0..V7 via NSRN (independent counters); overflows go on
         * the stack in declaration order at 8-byte slots. */
        {
            int base; int nargs; int j;
            int ngrn; int nsrn;
            int sp_adj;
            int callee_reg;
            int arg_cls[32];     /* RA_CLASS_X / V */
            int arg_reg[32];     /* dest reg encoding, -1 if on stack */
            int arg_stack[32];   /* SP-relative byte offset, -1 if in regs */
            int nstack_slots;

            base = h_cbase[idx];
            nargs = h_val[idx];
            if (nargs > 32) hx_die("HI_CALL: nargs > 32 unsupported", nargs);

            /* Pass 1: assign each arg to a register or stack slot.
             * Classify by ra_class_of (NOT ty_is_fp) so that HI_ALLOCA
             * with h_ty=TY_FLOAT is correctly seen as an X-class pointer. */
            ngrn = 0; nsrn = 0; nstack_slots = 0;
            j = 0;
            while (j < nargs) {
                int arg = h_carg[base + j];
                int is_fp = (ra_class_of(arg) == RA_CLASS_V);
                arg_cls[j]   = is_fp ? RA_CLASS_V : RA_CLASS_X;
                arg_reg[j]   = -1;
                arg_stack[j] = -1;
                if (is_fp) {
                    if (nsrn < 8) { arg_reg[j] = nsrn; nsrn = nsrn + 1; }
                    else { arg_stack[j] = nstack_slots * 8; nstack_slots = nstack_slots + 1; }
                } else {
                    if (ngrn < 8) { arg_reg[j] = hx_arg_reg[ngrn]; ngrn = ngrn + 1; }
                    else { arg_stack[j] = nstack_slots * 8; nstack_slots = nstack_slots + 1; }
                }
                j = j + 1;
            }
            sp_adj = nstack_slots * 8;
            if (sp_adj & 15) sp_adj = (sp_adj + 15) & ~15;

            /* Pass 2: register-passed args.  Sequential-mov hazard exists
             * (PARAM 0 placed in X1, want bar(b, a) → swap clobbers) — for
             * V class we hint PARAMs to V0..V7 so it rarely fires; the
             * TCE marshal in hx_marshal_call_args has the proper
             * parallel-move scheduler if/when we need it here too. */
            j = 0;
            while (j < nargs) {
                if (arg_reg[j] >= 0) {
                    int arg = h_carg[base + j];
                    hx_mat(arg, arg_reg[j]);
                }
                j = j + 1;
            }

            /* Pass 3: stack args.  Pre-allocate area, then store. */
            if (sp_adj > 0) {
                a64_sub_x_imm(A64_SP, A64_SP, sp_adj);
                j = 0;
                while (j < nargs) {
                    if (arg_stack[j] >= 0) {
                        int arg = h_carg[base + j];
                        if (arg_cls[j] == RA_CLASS_V) {
                            int rs = hx_get_src(arg, HX_SCRATCH_V1);
                            if (ty_is_double(h_ty[arg]))
                                a64_str_d_imm(rs, A64_SP, arg_stack[j]);
                            else
                                a64_str_s_imm(rs, A64_SP, arg_stack[j]);
                        } else {
                            int rs = hx_get_src(arg, HX_SCRATCH1);
                            a64_str_x_imm(rs, A64_SP, arg_stack[j]);
                        }
                    }
                    j = j + 1;
                }
            }

            if (k == HI_CALLP) {
                /* h_src1 = callee value */
                callee_reg = hx_get_src(s1, HX_SCRATCH1);
                a64_blr(callee_reg);
            } else {
                cg_cpatch_add(h_name[idx], a64_off, A64K_CALL26, 0);
                a64_bl(0);
            }

            if (sp_adj > 0)
                a64_add_x_imm(A64_SP, A64_SP, sp_adj);

            /* Result lands in V0 for float/double return, X0 otherwise. */
            if (hx_is_v(idx)) {
                if (dst != A64_V0) {
                    if (ty_is_double(h_ty[idx])) a64_fmov_d_d(dst, A64_V0);
                    else                          a64_fmov_s_s(dst, A64_V0);
                }
            } else {
                if (dst != A64_X0) a64_mov_x(dst, A64_X0);
            }
            hx_spill(idx, dst);
            return;
        }
    }

    if (k == HI_CALLHI) {
        /* High word of 64-bit CALL return — AArch64 returns 64-bit values
         * in a single register, so this should be impossible.  Treat as
         * NOP. */
        return;
    }

    if (k == HI_A64_DBT_TRAMPOLINE) {
        int base;
        int nargs;
        int j;
        int arg_reg[4];
        arg_reg[0] = A64_X0;
        arg_reg[1] = A64_X1;
        arg_reg[2] = A64_X2;
        arg_reg[3] = A64_X3;
        base = h_cbase[idx];
        nargs = h_val[idx];
        j = 0;
        while (j < nargs) {
            hx_mat(h_carg[base + j], HX_SCRATCH1);
            a64_str_x_pre(HX_SCRATCH1, A64_SP, -16);
            j = j + 1;
        }
        j = 0;
        while (j < nargs && j < 4) {
            int off;
            off = (nargs - 1 - j) * 16;
            a64_ldr_x_imm(arg_reg[j], A64_SP, off);
            j = j + 1;
        }
        if (nargs > 0) a64_add_x_imm(A64_SP, A64_SP, nargs * 16);
        cg_emit_a64_dbt_trampoline();
        return;
    }

    /* ---------- Floating-point conversions ---------- */
    if (k == HI_FCVT_ItoF) {
        /* int → float / double.  src is X-class, dst is V-class.
         * sf bit comes from the int side (TY_LLONG → 64-bit X form);
         * type bit comes from the FP side (TY_DOUBLE → D form). */
        int rs; int sf; int type;
        rs = hx_get_src(s1, HX_SCRATCH1);
        sf   = ty_is_llong(h_ty[s1]) ? 1 : 0;
        type = ty_is_double(h_ty[idx]) ? 1 : 0;
        if (h_ty[s1] & TY_UNSIGNED) a64_ucvtf(sf, type, dst, rs);
        else                         a64_scvtf(sf, type, dst, rs);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_FCVT_FtoI) {
        /* float / double → int / llong (round toward zero).  src is
         * V-class, dst is X-class.  sf from dst type, type from src. */
        int rs; int sf; int type;
        rs = hx_get_src(s1, HX_SCRATCH_V1);
        sf   = ty_is_llong(h_ty[idx]) ? 1 : 0;
        type = ty_is_double(h_ty[s1]) ? 1 : 0;
        if (h_ty[idx] & TY_UNSIGNED) a64_fcvtzu(sf, type, dst, rs);
        else                          a64_fcvtzs(sf, type, dst, rs);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_FCVT_FtoD) {
        /* f32 → f64.  Both V-class.  Currently unreachable (the lower
         * routes f32→f64 via __fp64_cvt_ftoD libcall) but wired for
         * future when the f64-libcall path is replaced. */
        int rs;
        rs = hx_get_src(s1, HX_SCRATCH_V1);
        a64_fcvt_d_s(dst, rs);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_FCVT_DtoF) {
        /* f64 → f32.  Same caveat as FtoD. */
        int rs;
        rs = hx_get_src(s1, HX_SCRATCH_V1);
        a64_fcvt_s_d(dst, rs);
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Floating-point comparisons ---------- */
    if (hx_is_fcmp_op(k)) {
        int rs1; int rs2; int is_d;
        is_d = ty_is_double(h_ty[s1]);
        rs1 = hx_get_src(s1, HX_SCRATCH_V1);
        rs2 = hx_get_src(s2, HX_SCRATCH_V2);
        if (is_d) a64_fcmp_d(rs1, rs2);
        else      a64_fcmp_s(rs1, rs2);
        a64_cset_w(dst, hx_fcmp_cond(k));
        hx_spill(idx, dst);
        return;
    }

    /* ---------- Floating-point arithmetic ---------- */
    if (k == HI_FADD || k == HI_FSUB || k == HI_FMUL || k == HI_FDIV) {
        int rs1; int rs2; int is_d;
        is_d = ty_is_double(h_ty[idx]);
        rs1 = hx_get_src(s1, HX_SCRATCH_V1);
        rs2 = hx_get_src(s2, HX_SCRATCH_V2);
        if (is_d) {
            if      (k == HI_FADD) a64_fadd_d(dst, rs1, rs2);
            else if (k == HI_FSUB) a64_fsub_d(dst, rs1, rs2);
            else if (k == HI_FMUL) a64_fmul_d(dst, rs1, rs2);
            else                   a64_fdiv_d(dst, rs1, rs2);
        } else {
            if      (k == HI_FADD) a64_fadd_s(dst, rs1, rs2);
            else if (k == HI_FSUB) a64_fsub_s(dst, rs1, rs2);
            else if (k == HI_FMUL) a64_fmul_s(dst, rs1, rs2);
            else                   a64_fdiv_s(dst, rs1, rs2);
        }
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_FNEG) {
        int rs1;
        rs1 = hx_get_src(s1, HX_SCRATCH_V1);
        if (ty_is_double(h_ty[idx])) a64_fneg_d(dst, rs1);
        else                         a64_fneg_s(dst, rs1);
        hx_spill(idx, dst);
        return;
    }
    if (k == HI_FSQRT) {
        int rs1;
        rs1 = hx_get_src(s1, HX_SCRATCH_V1);
        if (ty_is_double(h_ty[idx])) a64_fsqrt_d(dst, rs1);
        else                         a64_fsqrt_s(dst, rs1);
        hx_spill(idx, dst);
        return;
    }

    /* Unhandled */
    hx_die("hx_emit_inst: unhandled kind", idx);
}

/* ============================================================================
 * Function generation
 * ============================================================================ */

static void hx_gen_func(Node *fn) {
    int b; int i;
    int fs;
    int spill_slots;
    int callee_save_bytes;

    /* Initialise arg-reg table (AAPCS64). */
    hx_arg_reg[0] = A64_X0;
    hx_arg_reg[1] = A64_X1;
    hx_arg_reg[2] = A64_X2;
    hx_arg_reg[3] = A64_X3;
    hx_arg_reg[4] = A64_X4;
    hx_arg_reg[5] = A64_X5;
    hx_arg_reg[6] = A64_X6;
    hx_arg_reg[7] = A64_X7;

    hx_is_varargs = fn->is_varargs;
    hx_fn_nparams = fn->nparams;

    /* Align to 16 bytes (NOP padding). */
    while (a64_off & 15) a64_inst(0xD503201F);

    cg_func_name[cg_nfuncs]  = fn->name;
    cg_func_off[cg_nfuncs]   = a64_off;
    cg_func_local[cg_nfuncs] = fn->is_static;
    cg_nfuncs = cg_nfuncs + 1;

    /* Run the HIR pipeline. */
    licm_stat_hoisted = 0;
    hl_func(fn);
    hir_ssa_construct();
    hir_opt();
    hir_licm();

    /* Run the full BURG pipeline: label, count uses, select+fold.
     * bg_select() picks tiles top-down and NOPs folded children
     * (currently HI_ADDI on top of an ALLOCA chain — see LOAD/STORE
     * codegen which dispatches on bg_plnt[bg_sel[idx]]).  HI_ALLOCA is
     * remat-skipped at the def site already (see hx_emit_inst top). */
    if (!bg_inited) { bg_init(); bg_build_index(); bg_inited = 1; }
    bg_compute_foff();
    if (h_ninst <= BG_MAX_INST) {
        bg_label();
        bg_count_uses();
        bg_select();
    } else {
        int i_;
        i_ = 0;
        while (i_ < h_ninst) { bg_sel[i_] = -1; bg_fold[i_] = 0; i_ = i_ + 1; }
    }


    /* Identify fusable BRCs.  ra_extend_fused_cmp() (called from
     * hir_regalloc) extends operand live ranges and NOPs the fused
     * comparison; the BRC codegen below consumes hx_brc_fuse[] to emit
     * CMP + B.cond instead of CSET + CBZ. */
    hx_identify_fusions();

    /* Identify BIC/ORN/EON peephole sites.  Rewrites HIR (NOPs the BNOT,
     * potentially swaps src1/src2) so regalloc sees the post-fold form. */
    hx_identify_bic_peephole();

    /* Single-use ADDI address folds can be committed before regalloc:
     * rewrite the memory op to read the original base so liveness and
     * coloring never see the dead address temporary. */
    hx_fold_single_use_addi_addr_pre_ra();

    /* Run regalloc.  This sets ra_reg[], ra_spill_off[], and writes
     * the callee-saved set into ra_csave_*. */
    hir_regalloc();

    /* Fold ADD(base, SLL(idx, k)) chains feeding LOAD/STORE into the
     * AArch64 register-indexed addressing mode.  Must run after
     * regalloc (so ra_reg[] is final) but before codegen emits the
     * standalone HI_ADD/HI_SLL.  Sets bg_fold[] on the consumed nodes. */
    hx_fold_indexed_addr();

    /* Identify tail-call sites after regalloc (we need final ra_csave_*
     * for the teardown sequence; identification itself doesn't depend
     * on regalloc but we keep it next to the consumers). */
    hx_identify_tce(fn);

    hx_cleanup_redundant_moves();

    /* Frame layout
     * ------------
     *   [FP + 0]:   saved FP
     *   [FP + 8]:   saved LR
     *   [FP + 16]:  start of locals + spill slots, going UP for
     *               variable storage (we use NEGATIVE offsets via
     *               cg_a64_offset — actually FP-relative negative,
     *               so locals live BELOW FP).  This matches the
     *               tree-walk codegen.
     *
     * Frame size:
     *   16 (FP/LR)
     * + 2 * fn->locals_size  (frontend-scaled)
     * + ra_ncsave * 8        (callee-save spill slots)
     * + 16 if varargs (TBD)
     * Round up to 16-byte alignment.
     */
    /* hl_temp_stack now spans locals + lowered ALLOCA temps + regalloc spills
     * + callee-save slots (all in slow32 units of 4 bytes).  cg_a64_offset
     * doubles to AArch64 byte offsets, so frame-byte usage = 2 * hl_temp_stack
     * plus 16 for FP/LR. */
    (void)spill_slots; (void)callee_save_bytes;
    fs = 16 + 2 * hl_temp_stack;
    fs = (fs + 15) & ~15;
    if (fs < 16) fs = 16;        /* always at least save FP/LR */
    hx_frame_size = fs;

    /* No-frame leaf optimization: skip the entire prologue/epilogue
     * (sub sp / stp fp,lr / add fp / ldp fp,lr / add sp) when this
     * function:
     *   - uses no callee-saved regs (ra_ncsave == 0)
     *   - makes no calls (no HI_CALL/HI_CALLP — so LR survives)
     *   - has no &local addresses (no HI_ALLOCA — body never references FP)
     *   - has no actual spills (ra_spill_off[i] == 0 for all i, including
     *     PARAM nodes — confirms nothing in the body stores to the frame)
     * Param-to-reg MOVs in the prologue still run; they never touch FP. */
    hx_no_frame = 0;
    if (ra_ncsave == 0) {
        int can_nf;
        int i_;
        can_nf = 1;
        i_ = 0;
        while (i_ < h_ninst && can_nf) {
            int k_;
            k_ = h_kind[i_];
            if (k_ == HI_CALL || k_ == HI_CALLP || k_ == HI_A64_DBT_TRAMPOLINE || k_ == HI_ALLOCA) can_nf = 0;
            else if (ra_spill_off[i_] != 0) can_nf = 0;
            i_ = i_ + 1;
        }
        if (can_nf) hx_no_frame = 1;
    }

    /* Prologue.
     *
     * STP/LDP (signed-offset, scaled by 8) only encodes a 7-bit immediate
     * (±504 bytes).  For frames larger than 504+16, save FP/LR via a
     * scratch pointer:
     *     sub sp, sp, #fs
     *     add x16, sp, #(fs-16)
     *     stp x29, x30, [x16]
     *     mov x29, x16
     * For small frames keep the simpler `stp [sp, #fs-16]` form.
     * No-frame leaves skip the whole sequence. */
    if (!hx_no_frame) {
        if (fs <= 4095) a64_sub_x_imm(A64_SP, A64_SP, fs);
        else {
            a64_sub_x_imm(A64_SP, A64_SP, fs & 0xFFF);
            a64_sub_x_imm_lsl12(A64_SP, A64_SP, (fs >> 12) & 0xFFF);
        }
        if (fs - 16 <= 504) {
            a64_stp_x_off(A64_X29, A64_X30, A64_SP, fs - 16);
            if ((fs - 16) <= 4095) a64_add_x_imm(A64_X29, A64_SP, fs - 16);
            else {
                a64_add_x_imm(A64_X29, A64_SP, (fs - 16) & 0xFFF);
                a64_add_x_imm_lsl12(A64_X29, A64_X29, ((fs - 16) >> 12) & 0xFFF);
            }
        } else {
            /* Compute fp slot in HX_SCRATCH1, then STP at offset 0, then mov FP. */
            if ((fs - 16) <= 4095) a64_add_x_imm(HX_SCRATCH1, A64_SP, fs - 16);
            else {
                a64_add_x_imm(HX_SCRATCH1, A64_SP, (fs - 16) & 0xFFF);
                a64_add_x_imm_lsl12(HX_SCRATCH1, HX_SCRATCH1, ((fs - 16) >> 12) & 0xFFF);
            }
            a64_stp_x_off(A64_X29, A64_X30, HX_SCRATCH1, 0);
            a64_mov_x(A64_X29, HX_SCRATCH1);
        }
    }

    /* Save callee-saved regs that regalloc decided to use.  Adjacent
     * slots are 4 slow32 units (= 8 bytes) apart, with csave[i+1] at the
     * lower address; pair them into STP when the lower byte offset fits
     * the signed-imm7 range (±504, scaled by 8). */
    i = 0;
    while (i < ra_ncsave) {
        if (i + 1 < ra_ncsave &&
            ra_csave_off[i + 1] == ra_csave_off[i] - 4) {
            int boff_lo;
            boff_lo = cg_a64_offset(ra_csave_off[i + 1]);
            if (boff_lo >= -512 && boff_lo <= 504 && (boff_lo & 7) == 0) {
                a64_stp_x_off(ra_csave_reg[i + 1], ra_csave_reg[i],
                              A64_X29, boff_lo);
                i = i + 2;
                continue;
            }
        }
        hx_emit_store_from_reg(ra_csave_reg[i], ra_csave_off[i], 1);
        i = i + 1;
    }

    /* Save register-passed args to their assigned spill slots OR move
     * them into their assigned regs.  AAPCS64 has independent counters
     * for general (NGRN, X0..X7) and SIMD (NSRN, V0..V7) args; we walk
     * fn->args in source order and bump the appropriate counter per
     * arg type. */
    {
        int param_inst[16];
        int p_idx;
        int ngrn;
        int nsrn;
        Node *pp;

        /* Build param index → HIR inst map in one pass over the HIR. */
        p_idx = 0;
        while (p_idx < 16) { param_inst[p_idx] = -1; p_idx = p_idx + 1; }
        i = 0;
        while (i < h_ninst) {
            if (h_kind[i] == HI_PARAM) {
                int v;
                v = h_val[i];
                if (v >= 0 && v < 16) param_inst[v] = i;
            }
            i = i + 1;
        }

        p_idx = 0;
        ngrn = 0;
        nsrn = 0;
        pp = fn->args;
        while (pp) {
            int pi;
            int slot;
            int dst_reg;
            int is_fp;
            int src_reg;
            int in_regs;
            pi = (p_idx < 16) ? param_inst[p_idx] : -1;
            is_fp = ty_is_fp(pp->ty);

            if (is_fp && nsrn < 8) {
                src_reg = nsrn;     /* V0..V7 share encoding 0..7 with X */
                in_regs = 1;
                nsrn = nsrn + 1;
            } else if (!is_fp && ngrn < 8) {
                src_reg = hx_arg_reg[ngrn];
                in_regs = 1;
                ngrn = ngrn + 1;
            } else {
                src_reg = -1;
                in_regs = 0;
            }

            if (pi < 0) { p_idx = p_idx + 1; pp = pp->next; continue; }

            if (in_regs) {
                slot = ra_reg[pi];
                if (slot >= 0) {
                    dst_reg = hx_slot_reg(slot);
                    if (dst_reg != src_reg) {
                        if (is_fp) {
                            if (ty_is_double(pp->ty)) a64_fmov_d_d(dst_reg, src_reg);
                            else                       a64_fmov_s_s(dst_reg, src_reg);
                        } else {
                            a64_mov_x(dst_reg, src_reg);
                        }
                    }
                } else if (ra_spill_off[pi] != 0) {
                    if (is_fp) {
                        hx_emit_fp_store_from_reg(src_reg,
                                                  ra_spill_off[pi],
                                                  ty_is_double(pp->ty));
                    } else {
                        hx_emit_store_from_reg(src_reg,
                                               ra_spill_off[pi],
                                               hx_is_wide(h_ty[pi]));
                    }
                }
            } else {
                /* Stack arg.  AAPCS lays them out in declaration order
                 * starting at [old_SP] = [FP + 16].  Counting position
                 * is approximate — for non-trivial mixed signatures the
                 * 16-byte alignment / type-size rules apply.  For the
                 * test surface we hit today (≤8 args, no struct passing),
                 * source-order indexing matches AAPCS layout closely
                 * enough.  Refinement deferred. */
                int caller_off;
                caller_off = 16 + (p_idx - 8) * 8;
                if (is_fp) {
                    if (ty_is_double(pp->ty))
                        a64_ldr_d_imm(HX_SCRATCH_V1, A64_X29, caller_off);
                    else
                        a64_ldr_s_imm(HX_SCRATCH_V1, A64_X29, caller_off);
                    slot = ra_reg[pi];
                    if (slot >= 0) {
                        if (ty_is_double(pp->ty))
                            a64_fmov_d_d(hx_slot_reg(slot), HX_SCRATCH_V1);
                        else
                            a64_fmov_s_s(hx_slot_reg(slot), HX_SCRATCH_V1);
                    } else if (ra_spill_off[pi] != 0) {
                        hx_emit_fp_store_from_reg(HX_SCRATCH_V1,
                                                  ra_spill_off[pi],
                                                  ty_is_double(pp->ty));
                    }
                } else {
                    a64_ldr_x_imm(HX_SCRATCH1, A64_X29, caller_off);
                    slot = ra_reg[pi];
                    if (slot >= 0) {
                        a64_mov_x(hx_slot_reg(slot), HX_SCRATCH1);
                    } else if (ra_spill_off[pi] != 0) {
                        hx_emit_store_from_reg(HX_SCRATCH1, ra_spill_off[pi],
                                               hx_is_wide(h_ty[pi]));
                    }
                }
            }
            p_idx = p_idx + 1;
            pp = pp->next;
        }
    }

    /* --- Body: walk blocks in RPO order, emit each instruction. --- */
    i = 0;
    while (i < bb_nblk) { hx_blk_off[i] = -1; i = i + 1; }
    hx_nbpatch = 0;
    /* Clear LDP/STP lookahead skip-marks for this function. */
    i = 0;
    while (i < h_ninst) { hx_skip_emit[i] = 0; i = i + 1; }

    {
        int rpo_idx;
        rpo_idx = 0;
        while (rpo_idx < ssa_rpo_cnt) {
            b = ssa_rpo[rpo_idx];
            hx_define_block(b);
            /* Tell BR/BRC which block lays down next, so a `b next_blk`
             * at the end of this block can be elided. */
            if (rpo_idx + 1 < ssa_rpo_cnt) hx_next_blk = ssa_rpo[rpo_idx + 1];
            else                            hx_next_blk = -1;
            /* Emit LICM-hoisted instructions (live at block-top). */
            i = licm_head[b];
            while (i >= 0) { hx_emit_inst(i); i = licm_next[i]; }
            /* Emit non-PHI / non-NOP instructions in original order.
             * Skip any inst that LDP/STP lookahead consumed at an earlier
             * position (hx_skip_emit). */
            hx_cur_blk_end = bb_end[b];
            i = bb_start[b];
            while (i < bb_end[b]) {
                if (!hx_skip_emit[i]
                        && h_kind[i] != HI_NOP && h_kind[i] != HI_PHI)
                    hx_emit_inst(i);
                i = i + 1;
            }
            rpo_idx = rpo_idx + 1;
        }
    }

    /* --- Epilogue --- */
    {
        int epi_off;
        epi_off = a64_off;

        hx_emit_teardown();
        a64_ret();

        /* Resolve epilog patches (RET pseudo-target = -1) */
        i = 0;
        while (i < hx_nbpatch) {
            if (hx_bpatch_blk[i] == -1) {
                int site; int word; int diff; int top8; int kept;
                site = hx_bpatch_off[i];
                diff = epi_off - site;
                word = a64_read_inst(site);
                top8 = (word >> 24) & 0xFF;
                if (top8 == 0x14 || top8 == 0x94) {
                    int imm26;
                    imm26 = (diff >> 2) & 0x03FFFFFF;
                    kept = word & 0xFC000000;
                    a64_patch_inst(site, kept | imm26);
                } else {
                    int imm19;
                    imm19 = (diff >> 2) & 0x7FFFF;
                    kept = word & 0xFF00001F;
                    a64_patch_inst(site, kept | (imm19 << 5));
                }
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
 * Program generation -- mirrors gen_program() but uses hx_gen_func().
 * ============================================================================ */

static void hx_gen_program(Node *prog, int compile_only) {
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
    cg_object_mode = compile_only;

    collect_globals(prog);

    if (!compile_only) {
        cg_func_name[cg_nfuncs] = "__save_envp";
        cg_func_off[cg_nfuncs]  = a64_off;
        cg_nfuncs = cg_nfuncs + 1;
        a64_ret();
    }

    entry_off = a64_off;

    if (!compile_only) emit_crt0_to_buf();

    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) hx_gen_func(fn);
        fn = fn->next;
    }

    /* Copy lexer string pool */
    i = 0;
    while (i < lex_strpool_len && i < CG_STRING_POOL_SZ) {
        cg_str_pool[i] = lex_strpool[i]; i = i + 1;
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

    if (compile_only) return;

    elf_init();
    elf_set_text(a64_buf, a64_off);
    if (cg_rodata_len > 0)                  elf_set_rodata(cg_rodata, cg_rodata_len);
    if (cg_data_len > 0 || cg_bss_size > 0) elf_set_data(cg_data, cg_data_len);
    if (cg_bss_size > 0)                    elf_set_bss(cg_bss_size);
    elf_set_entry(entry_off);

    resolve_calls();
    resolve_relocations();
}

#endif /* HIR_CODEGEN_A64_H */
