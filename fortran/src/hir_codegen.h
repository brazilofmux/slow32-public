/* COPIED from selfhost/stage08/hir_codegen.h at 849dd791.  See note in hir.h. */
/* hir_codegen.h -- HIR to SLOW-32 assembly for s12cc
 *
 * Phase 8: Linear scan register allocation (r11-r28, callee-saved).
 * Uses ra_reg[] from hir_regalloc.h for physical register assignments.
 * Rematerializable values (ICONST, ALLOCA, GADDR, SADDR, FADDR)
 * are regenerated on demand. Spilled values use r1/r2 as scratch.
 */

/* --- Output buffer --- */
#define CG_MAX_OUT 4194304

static char cg_out[CG_MAX_OUT];
static int  cg_olen;

/* --- Asm emission helpers --- */

static void cg_c(int ch) {
    if (cg_olen < CG_MAX_OUT - 1) {
        cg_out[cg_olen] = ch;
        cg_olen = cg_olen + 1;
    }
}

static void cg_s(char *s) {
    int i;
    i = 0;
    while (s[i] != 0) {
        cg_c(s[i]);
        i = i + 1;
    }
}

static void cg_n(int v) {
    char buf[12];
    int i;
    int neg;
    if (v == 0) { cg_c(48); return; }
    neg = 0;
    if (v == -2147483647 - 1) {
        /* 0 - INT_MIN overflows back to INT_MIN and the digit loop
         * never runs: a global holding 0x80000000 emitted ".word -" */
        cg_s("-2147483648");
        return;
    }
    if (v < 0) { neg = 1; v = 0 - v; }
    i = 0;
    while (v > 0) {
        buf[i] = 48 + (v % 10);
        v = v / 10;
        i = i + 1;
    }
    if (neg) cg_c(45);
    while (i > 0) {
        i = i - 1;
        cg_c(buf[i]);
    }
}

static void cg_ldef(int l) {
    cg_s(".L");
    cg_n(l);
    cg_s(":\n");
}

static void cg_lref(int l) {
    cg_s(".L");
    cg_n(l);
}

/* Emit "    OP rD, rA, rB\n" */
static void cg_rrr(char *op, int rd, int ra, int rb) {
    cg_s("    ");
    cg_s(op);
    cg_s(" r");
    cg_n(rd);
    cg_s(", r");
    cg_n(ra);
    cg_s(", r");
    cg_n(rb);
    cg_c(10);
}

/* Emit "    OP rD, rA, imm\n" */
static void cg_rri(char *op, int rd, int ra, int imm) {
    cg_s("    ");
    cg_s(op);
    cg_s(" r");
    cg_n(rd);
    cg_s(", r");
    cg_n(ra);
    cg_s(", ");
    cg_n(imm);
    cg_c(10);
}

/* --- Per-function codegen state --- */

static int hcg_locals;     /* fn->locals_size (original) */
static int hcg_frame;      /* total frame size */
static int hcg_frame_escapes; /* an alloca's address may reach a callee:
                                 tail calls must not pop the frame first */
static int hcg_epilog;     /* epilog label */
static int hcg_va_save_size; /* varargs register save area size */

/* Block labels */
static int hcg_blk_lbl[HIR_MAX_BLOCK];

/* --- Branch-target forwarding through empty trampoline blocks ---
 * SSA critical-edge splitting leaves blocks whose entire content is
 * one unconditional HI_BR — and they land between a loop header and
 * its body in block-index order, so every loop iteration paid two
 * extra instructions: a taken branch into the trampoline plus its
 * jal (bcond -> jal -> jal).  hcg_fwd[b] names the block control
 * actually reaches from b; branch emission resolves targets through
 * it and skips the bodies of forwarded blocks.  hcg_next_emit[b] is
 * the next non-forwarded block after b — the real fallthrough
 * neighbor once trampolines stop being emitted. */
static int hcg_fwd[HIR_MAX_BLOCK];
static int hcg_next_emit[HIR_MAX_BLOCK];
static int hcg_emit_ord[HIR_MAX_BLOCK];   /* layout order (see compute_fwd) */
static int hcg_nord;
static char hcg_placed[HIR_MAX_BLOCK];
static int hcg_skip[HIR_MAX_BLOCK]; /* (always 0 now; see hcg_compute_fwd) */
static int hcg_blk_pos[HIR_MAX_BLOCK]; /* estimated byte offset, over-estimated */

/* Deferred jump-table emission (issue #32).  HI_JMPTAB dispatch is emitted
 * inline in .text, but the table of target addresses must live in a
 * readable section (.text is execute-only under W^X), so it is collected
 * here during codegen and emitted in gen_data().  Entries store globally
 * unique block-label numbers (hcg_blk_lbl values), which stay valid after
 * the per-function hcg_blk_lbl[] array is overwritten because cg_lbl is
 * monotonic. */
#define CG_MAX_JT      512
#define CG_MAX_JT_ENT  32768
static int cg_jt_id[CG_MAX_JT];      /* .LJT label number */
static int cg_jt_base[CG_MAX_JT];    /* base into cg_jt_ent */
static int cg_jt_span[CG_MAX_JT];    /* entry count */
static int cg_jt_ent[CG_MAX_JT_ENT]; /* target block-label numbers */
static int cg_njt;
static int cg_njt_ent;

/* Immediate-selection telemetry */
static int hcg_stat_imm_opp_add;
static int hcg_stat_imm_hit_add;
static int hcg_stat_imm_miss_add;
static int hcg_stat_imm_opp_sub;
static int hcg_stat_imm_hit_sub;
static int hcg_stat_imm_miss_sub;
static int hcg_stat_imm_opp_logic;
static int hcg_stat_imm_hit_logic;
static int hcg_stat_imm_miss_logic;
static int hcg_stat_imm_opp_shift;
static int hcg_stat_imm_hit_shift;
static int hcg_stat_imm_miss_shift;
static int hcg_stat_imm_opp_cmp;
static int hcg_stat_imm_hit_cmp;
static int hcg_stat_imm_miss_cmp;
static int hcg_stat_li_total;
static int hcg_stat_li_small;
static int hcg_stat_li_lui_only;
static int hcg_stat_li_lui_addi;
static int hcg_stat_copy_emit;
static int hcg_stat_addi0_elide;
static int hcg_stat_divrem_pow2;
static int hcg_stat_tailcall;
static int hcg_stat_brc_fuse;
static int hcg_stat_br_fallthru;
static int hcg_cur_blk;                  /* current block being emitted */

/* Spill slots must start below any lowered HI_ALLOCA temp, not just
 * fn->locals_size, because lowering can introduce extra stack temps. */
static int hcg_hir_frame_base(Node *fn) {
    int base;
    int i;
    int off;

    base = fn->locals_size;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_ALLOCA && h_val[i] < 0) {
            off = 0 - h_val[i];
            if (off > base) base = off;
        }
        i = i + 1;
    }
    return base;
}

/* --- Load immediate into register --- */
static void hcg_li(int reg, int v) {
    int hi;
    int lo;
    hcg_stat_li_total = hcg_stat_li_total + 1;
    if (v >= -2048 && v <= 2047) {
        hcg_stat_li_small = hcg_stat_li_small + 1;
        cg_rri("addi", reg, 0, v);
    } else {
        hi = (v + 2048) >> 12;
        hi = hi & 1048575;
        lo = v & 4095;
        if (lo >= 2048) lo = lo - 4096;
        cg_s("    lui r");
        cg_n(reg);
        cg_s(", ");
        cg_n(hi);
        cg_c(10);
        /* Avoid no-op addi when low part is 0. */
        if (lo != 0) {
            hcg_stat_li_lui_addi = hcg_stat_li_lui_addi + 1;
            cg_rri("addi", reg, reg, lo);
        } else {
            hcg_stat_li_lui_only = hcg_stat_li_lui_only + 1;
        }
    }
}

/* Load address of symbol into register */
static void hcg_la(int reg, char *sym) {
    cg_s("    lui r");
    cg_n(reg);
    cg_s(", %hi(");
    cg_s(sym);
    cg_s(")\n");
    cg_s("    addi r");
    cg_n(reg);
    cg_s(", r");
    cg_n(reg);
    cg_s(", %lo(");
    cg_s(sym);
    cg_s(")\n");
}

/* --- Materialize a HIR value into a specific register --- */

static void hcg_into(int reg, int inst) {
    int k;
    int off;

    if (inst < 0) {
        cg_rri("addi", reg, 0, 0);
        return;
    }

    /* Check register allocation first */
    if (ra_reg[inst] >= 0) {
        if (reg == ra_reg[inst]) return;
        cg_rri("addi", reg, ra_reg[inst], 0);
        return;
    }

    k = h_kind[inst];

    /* Rematerializable instructions */
    if (k == HI_ICONST) {
        hcg_li(reg, h_val[inst]);
        return;
    }
    if (k == HI_ALLOCA) {
        off = h_val[inst];
        if (off >= -2048 && off <= 2047) {
            cg_rri("addi", reg, 30, off);
        } else {
            hcg_li(reg, off);
            cg_rrr("add", reg, 30, reg);
        }
        return;
    }
    if (k == HI_GADDR) {
        hcg_la(reg, h_name[inst]);
        return;
    }
    if (k == HI_SADDR) {
        cg_s("    lui r");
        cg_n(reg);
        cg_s(", %hi(.LS");
        cg_n(h_val[inst]);
        cg_s(")\n    addi r");
        cg_n(reg);
        cg_s(", r");
        cg_n(reg);
        cg_s(", %lo(.LS");
        cg_n(h_val[inst]);
        cg_s(")\n");
        return;
    }
    if (k == HI_FADDR) {
        hcg_la(reg, h_name[inst]);
        return;
    }
    if (k == HI_GETFP) {
        cg_rri("addi", reg, 30, 0);
        return;
    }

    /* Spilled: load from spill slot */
    off = ra_spill_off[inst];
    if (off != 0) {
        if (off >= -2048 && off <= 2047) {
            cg_s("    ldw r");
            cg_n(reg);
            cg_s(", r30, ");
            cg_n(off);
            cg_c(10);
        } else {
            hcg_li(reg, off);
            cg_rrr("add", reg, 30, reg);
            cg_s("    ldw r");
            cg_n(reg);
            cg_s(", r");
            cg_n(reg);
            cg_s(", 0\n");
        }
        return;
    }

    /* Fallback: value not available */
    cg_rri("addi", reg, 0, 0);
}

/* --- Source register helper ---
 * Returns the physical register containing inst's value.
 * If inst is in a register, returns that register (no code emitted).
 * If inst is rematerializable or spilled, loads into scratch and returns scratch. */
static int hcg_src(int inst, int scratch) {
    if (inst < 0) return 0;
    if (ra_reg[inst] >= 0) return ra_reg[inst];
    hcg_into(scratch, inst);
    return scratch;
}

/* Recover a small constant through COPY/ADDI chains for immediate emission. */
static int hcg_const_imm_inst(int inst, int *out) {
    int k;
    int acc;
    int lim;

    acc = 0;
    lim = 0;
    while (inst >= 0 && lim < 64) {
        k = h_kind[inst];
        if (k == HI_ICONST) {
            *out = acc + h_val[inst];
            return 1;
        }
        if (k == HI_COPY) {
            inst = h_src1[inst];
            lim = lim + 1;
            continue;
        }
        if (k == HI_ADDI) {
            acc = acc + h_val[inst];
            inst = h_src1[inst];
            lim = lim + 1;
            continue;
        }
        return 0;
    }
    return 0;
}

static int hcg_is_i12(int v) {
    return (v >= -2048 && v <= 2047);
}

static int hcg_is_u12(int v) {
    return (v >= 0 && v <= 4095);
}

static int hcg_const_is_zero(int inst) {
    int c;
    if (!hcg_const_imm_inst(inst, &c)) return 0;
    return (c == 0);
}

static int hcg_addr_base_off(int inst, int *base_out, int *off_out) {
    int k;
    int off;
    int lim;

    off = 0;
    lim = 0;
    while (inst >= 0 && lim < 64) {
        k = h_kind[inst];
        if (k == HI_COPY) {
            inst = h_src1[inst];
            lim = lim + 1;
            continue;
        }
        if (k == HI_ADDI) {
            off = off + h_val[inst];
            inst = h_src1[inst];
            lim = lim + 1;
            continue;
        }
        *base_out = inst;
        *off_out = off;
        return (inst >= 0);
    }
    return 0;
}

/* --- Destination register helper ---
 * Returns the physical register for the result.
 * If allocated, returns the physical register.
 * If spilled, returns r1 (caller must store r1 to spill slot). */
static int hcg_dst(int idx) {
    if (ra_reg[idx] >= 0) return ra_reg[idx];
    return 1;
}

/* --- Store an arbitrary register to idx's spill slot ---
 * Used for post-call CALLHI (hi word in r2): materializing a spilled
 * CALLHI through the r1 spill scratch (`addi r1, r2`) destroyed the
 * call's own lo word before ITS spill — dtoa_r's spilled
 * `1ull << j` came back with lo == hi.  r3 is safe address scratch
 * after a call: caller-saved registers hold nothing live there. */
static void hcg_spill_from(int idx, int reg) {
    int off;
    if (ra_reg[idx] >= 0) return;
    if (hi_inst_remat(idx)) return;
    off = ra_spill_off[idx];
    if (off == 0) return;
    if (off >= -2048 && off <= 2047) {
        cg_s("    stw r30, r");
        cg_n(reg);
        cg_s(", ");
        cg_n(off);
        cg_c(10);
    } else {
        hcg_li(3, off);
        cg_rrr("add", 3, 30, 3);
        cg_s("    stw r3, r");
        cg_n(reg);
        cg_s(", 0\n");
    }
}

/* --- Store r1 to spill slot if instruction is spilled --- */
static void hcg_maybe_spill(int idx) {
    int off;
    if (ra_reg[idx] >= 0) return;
    if (hi_inst_remat(idx)) return;
    off = ra_spill_off[idx];
    if (off == 0) return;
    if (off >= -2048 && off <= 2047) {
        cg_s("    stw r30, r1, ");
        cg_n(off);
        cg_c(10);
    } else {
        hcg_li(2, off);
        cg_rrr("add", 2, 30, 2);
        cg_s("    stw r2, r1, 0\n");
    }
}

/* --- Cycle-safe parameter entry sequence ---
 * The allocator may color a register param anywhere, including a
 * permutation of the ABI registers (param0 in r4, param1 in r3).
 * Emitting each param's ABI->assigned move independently in
 * instruction order clobbers a later move's source in that case
 * (found via strcmp comparing a string with itself once phi
 * coalescing went live).  So the first HI_PARAM emits the WHOLE
 * entry sequence, ordered for safety:
 *   1. spilled register params — read ABI reg, write only r1+memory;
 *   2. register-resident params — parallel copy with cycle breaking
 *      through the reserved r2 temp (same discipline as
 *      hcg_phi_copies' fast path);
 *   3. stack-passed params — read memory, write registers whose
 *      pending readers (step 2 sources) are already consumed.
 * Subsequent HI_PARAMs emit nothing. */
static int hcg_params_emitted;
static int hcg_pe_dst[8];
static int hcg_pe_src[8];
static int hcg_pe_active[8];
static int hcg_argmap[HIR_MAX_CARG]; /* per-call ABI register map */

static void hcg_emit_param_entry(void) {
    int i;
    int j;
    int nc;
    int rem;
    int progress;
    int blocked;
    int srcv;
    int pidx;

    /* Step 1: spilled register params (order-independent). */
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PARAM && h_val[i] < hl_param_nflat &&
            hl_param_map[h_val[i]] >= 0 && ra_reg[i] < 0) {
            cg_rri("addi", 1, hl_param_map[h_val[i]], 0);
            hcg_maybe_spill(i);
        }
        i = i + 1;
    }

    /* Step 2: register-resident params — parallel copy. */
    nc = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PARAM && h_val[i] < hl_param_nflat &&
            hl_param_map[h_val[i]] >= 0 && ra_reg[i] >= 0 && nc < 8) {
            hcg_pe_dst[nc] = ra_reg[i];
            hcg_pe_src[nc] = hl_param_map[h_val[i]];
            hcg_pe_active[nc] = 1;
            nc = nc + 1;
        }
        i = i + 1;
    }
    rem = 0;
    j = 0;
    while (j < nc) {
        if (hcg_pe_dst[j] == hcg_pe_src[j]) {
            hcg_pe_active[j] = 0;
        } else {
            rem = rem + 1;
        }
        j = j + 1;
    }
    while (rem > 0) {
        progress = 0;
        j = 0;
        while (j < nc) {
            if (hcg_pe_active[j]) {
                blocked = 0;
                i = 0;
                while (i < nc) {
                    if (hcg_pe_active[i] && i != j &&
                        hcg_pe_src[i] == hcg_pe_dst[j]) {
                        blocked = 1;
                        break;
                    }
                    i = i + 1;
                }
                if (!blocked) {
                    cg_rri("addi", hcg_pe_dst[j], hcg_pe_src[j], 0);
                    hcg_pe_active[j] = 0;
                    rem = rem - 1;
                    progress = 1;
                }
            }
            j = j + 1;
        }
        if (!progress) {
            /* Copy cycle: snapshot one source in the reserved r2 temp
             * and redirect its readers. */
            srcv = -1;
            j = 0;
            while (j < nc) {
                if (hcg_pe_active[j]) { srcv = hcg_pe_src[j]; break; }
                j = j + 1;
            }
            if (srcv < 0) break;
            cg_rri("addi", 2, srcv, 0);
            j = 0;
            while (j < nc) {
                if (hcg_pe_active[j] && hcg_pe_src[j] == srcv) {
                    hcg_pe_src[j] = 2;
                }
                j = j + 1;
            }
        }
    }

    /* Step 3: stack-passed params (ABI walk spilled them). */
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_PARAM && h_val[i] < hl_param_nflat &&
            hl_param_map[h_val[i]] < 0) {
            pidx = h_val[i];
            if (hl_param_stkord[pidx] > 2047) {
                /* Earlier byval slots pushed this offset past LDW's imm
                 * range: form the address in the r1 scratch. */
                hcg_li(1, hl_param_stkord[pidx]);
                cg_rrr("add", 1, 30, 1);
                if (ra_reg[i] >= 0) {
                    cg_rri("ldw", ra_reg[i], 1, 0);
                } else {
                    cg_rri("ldw", 1, 1, 0);
                    hcg_maybe_spill(i);
                }
            } else if (ra_reg[i] >= 0) {
                cg_rri("ldw", ra_reg[i], 30, hl_param_stkord[pidx]);
            } else {
                cg_rri("ldw", 1, 30, hl_param_stkord[pidx]);
                hcg_maybe_spill(i);
            }
        }
        i = i + 1;
    }
}

/* --- Typed load/store helpers --- */

/* Emit load from [areg+0] into dreg with appropriate width */
/* Emit symbol reference for SADDR/GADDR/FADDR, with ADDI offset folding.
 * Uses bg_ssym[]/bg_soff[] computed before BURG folding. */
static void hcg_emit_sym(int inst) {
    int base;
    int off;
    base = bg_ssym[inst];
    off = bg_soff[inst];
    if (base < 0) base = inst;
    if (h_kind[base] == HI_SADDR) {
        cg_s(".LS");
        cg_n(h_val[base]);
    } else {
        cg_s(h_name[base]);
    }
    if (off != 0) {
        cg_s("+");
        cg_n(off);
    }
}

/* Emit load from symbol address: lui r1, %hi(sym); ldX rd, r1, %lo(sym) */
static void hcg_load_saddr(int dreg, int sinst, int ty) {
    cg_s("    lui r1, %hi(");
    hcg_emit_sym(sinst);
    cg_s(")\n");
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r");
        else                  cg_s("    ldb r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) cg_s("    ldhu r");
        else                  cg_s("    ldh r");
    } else {
        cg_s("    ldw r");
    }
    cg_n(dreg);
    cg_s(", r1, %lo(");
    hcg_emit_sym(sinst);
    cg_s(")\n");
}

/* Emit store to symbol address: lui r1, %hi(sym); stX r1, vreg, %lo(sym) */
static void hcg_store_saddr(int vreg, int sinst, int ty) {
    cg_s("    lui r1, %hi(");
    hcg_emit_sym(sinst);
    cg_s(")\n");
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        cg_s("    stb r1, r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        cg_s("    sth r1, r");
    } else {
        cg_s("    stw r1, r");
    }
    cg_n(vreg);
    cg_s(", %lo(");
    hcg_emit_sym(sinst);
    cg_s(")\n");
}

static void hcg_load_mem(int dreg, int areg, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r");
        else                  cg_s("    ldb r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) cg_s("    ldhu r");
        else                  cg_s("    ldh r");
    } else {
        cg_s("    ldw r");
    }
    cg_n(dreg);
    cg_s(", r");
    cg_n(areg);
    cg_s(", 0\n");
}

/* Emit load from [base+off] into dreg with appropriate width (small offset) */
static void hcg_load_off(int dreg, int base, int off, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r");
        else                  cg_s("    ldb r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED) cg_s("    ldhu r");
        else                  cg_s("    ldh r");
    } else {
        cg_s("    ldw r");
    }
    cg_n(dreg);
    cg_s(", r");
    cg_n(base);
    cg_s(", ");
    cg_n(off);
    cg_c(10);
}

/* Emit store vreg to [areg+0] with appropriate width */
static void hcg_store_mem(int areg, int vreg, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        cg_s("    stb r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        cg_s("    sth r");
    } else {
        cg_s("    stw r");
    }
    cg_n(areg);
    cg_s(", r");
    cg_n(vreg);
    cg_s(", 0\n");
}

/* Emit store vreg to [base+off] with appropriate width (small offset) */
static void hcg_store_off(int base, int vreg, int off, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        cg_s("    stb r");
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        cg_s("    sth r");
    } else {
        cg_s("    stw r");
    }
    cg_n(base);
    cg_s(", r");
    cg_n(vreg);
    cg_s(", ");
    cg_n(off);
    cg_c(10);
}

/* --- Emit phi copies for a branch from_blk -> to_blk --- */

static int hcg_phi_tmp[SSA_MAX_PROMO];
static int hcg_phi_push_ix[SSA_MAX_PROMO];
static int hcg_phi_is_const[SSA_MAX_PROMO];
static int hcg_phi_const_val[SSA_MAX_PROMO];
static int hcg_phi_src_reg[SSA_MAX_PROMO];
static int hcg_phi_dst_reg[SSA_MAX_PROMO];
static int hcg_phi_active[SSA_MAX_PROMO];
static int hcg_phi_src_inst[SSA_MAX_PROMO];

/* DIVERGENCE (f77, port upstream candidate): is the (from -> to) edge
 * free of REAL phi copies?  The direct conditional-branch shapes used
 * to demand phi-free targets; after coalescing, a rotated loop's back
 * edge usually carries only no-op copies (source and destination
 * coalesced into the same register), and refusing the direct shape
 * cost a jal per iteration.  A constant or spilled phi source still
 * needs a real copy and keeps the trampoline. */
static int hcg_edge_nocopy(int from_blk, int to_blk) {
    int i;
    int v;
    i = ssa_phi_head[to_blk];
    while (i >= 0) {
        if (h_kind[i] == HI_PHI) {
            if (ra_reg[i] < 0) return 0;
            v = ssa_phi_find_arg(i, from_blk);
            if (v < 0 || ra_reg[v] < 0) return 0;
            if (ra_reg[v] != ra_reg[i]) return 0;
        }
        i = ssa_phi_next[i];
    }
    return 1;
}

static void hcg_phi_copies(int from_blk, int to_blk) {
    int i;
    int n;
    int fast_ok;
    int rem;
    int progress;
    int blocked;
    int srcv;
    int src;
    int dst;
    int npush;
    int j;
    int v;
    int c;
    int off;
    int phi;
    int dreg;

    /* Collect PHIs in to_blk */
    n = 0;
    i = ssa_phi_head[to_blk];
    while (i >= 0) {
        if (h_kind[i] == HI_PHI) {
            hcg_phi_tmp[n] = i;
            n = n + 1;
        }
        i = ssa_phi_next[i];
    }
    if (n == 0) return;


    /* Fast path.  DIVERGENCE (f77, port upstream): spilled phis no
     * longer force the whole edge onto the push/pop slow path -- one
     * spilled back-edge phi used to cost every value on the edge a
     * push and a pop per iteration.  A memory DESTINATION can never
     * be part of a register cycle, so those copies are emitted first
     * (through the r2 temp); a memory SOURCE behaves like a constant
     * (hcg_is_const == 2), loaded straight into its destination when
     * that register falls free.  The remaining true hazards keep the
     * slow path: a slot both read and written on the same edge (a
     * spilled phi feeding another phi), a far (>12-bit) destination
     * slot, or r2 itself appearing as a copy endpoint. */
    fast_ok = 1;
    j = 0;
    while (j < n) {
        phi = hcg_phi_tmp[j];
        v = ssa_phi_find_arg(phi, from_blk);
        hcg_phi_src_inst[j] = v;
        hcg_phi_active[j] = 1;
        hcg_phi_dst_reg[j] = ra_reg[phi];
        if (hcg_const_imm_inst(v, &c)) {
            hcg_phi_is_const[j] = 1;
            hcg_phi_const_val[j] = c;
            hcg_phi_src_reg[j] = -1;
        } else if (v < 0) {
            fast_ok = 0;
            break;
        } else if (ra_reg[v] >= 0) {
            if (ra_reg[v] == 2) { fast_ok = 0; break; }
            hcg_phi_is_const[j] = 0;
            hcg_phi_src_reg[j] = ra_reg[v];
        } else {
            /* memory (or remat) source: const-like, no register held */
            hcg_phi_is_const[j] = 2;
            hcg_phi_src_reg[j] = -1;
        }
        if (ra_reg[phi] < 0) {
            off = ra_spill_off[phi];
            if (off < -2048 || off > 2047) { fast_ok = 0; break; }
            /* Slot written here and read by another copy on this
             * edge?  (The reader's source can only be this phi.) */
            i = 0;
            while (i < n) {
                if (i != j &&
                    ssa_phi_find_arg(hcg_phi_tmp[i], from_blk) == phi &&
                    ra_reg[phi] < 0) { fast_ok = 0; break; }
                i = i + 1;
            }
            if (!fast_ok) break;
        } else if (ra_reg[phi] == 2) {
            fast_ok = 0;
            break;
        }
        j = j + 1;
    }

    if (fast_ok) {
        /* Memory destinations first: cycle-free by construction. */
        rem = n;
        j = 0;
        while (j < n) {
            phi = hcg_phi_tmp[j];
            if (hcg_phi_dst_reg[j] < 0) {
                off = ra_spill_off[phi];
                v = hcg_phi_src_inst[j];
                if (off == 0) {
                    /* no slot: the value is never read; drop the copy */
                } else if (hcg_phi_is_const[j] == 0) {
                    cg_s("    stw r30, r");
                    cg_n(hcg_phi_src_reg[j]);
                    cg_s(", ");
                    cg_n(off);
                    cg_c(10);
                } else if (hcg_phi_is_const[j] == 2 && v >= 0 &&
                           ra_spill_off[v] == off) {
                    /* same slot: no-op */
                } else {
                    hcg_into(2, v);
                    cg_s("    stw r30, r2, ");
                    cg_n(off);
                    cg_c(10);
                }
                hcg_phi_active[j] = 0;
                rem = rem - 1;
            }
            j = j + 1;
        }
        while (rem > 0) {
            progress = 0;
            j = 0;
            while (j < n) {
                if (!hcg_phi_active[j]) { j = j + 1; continue; }
                dst = hcg_phi_dst_reg[j];
                blocked = 0;
                if (hcg_phi_is_const[j]) {
                    i = 0;
                    while (i < n) {
                        if (hcg_phi_active[i] && !hcg_phi_is_const[i] && hcg_phi_src_reg[i] == dst) {
                            blocked = 1;
                            break;
                        }
                        i = i + 1;
                    }
                } else {
                    src = hcg_phi_src_reg[j];
                    if (src != dst) {
                        i = 0;
                        while (i < n) {
                            if (hcg_phi_active[i] && !hcg_phi_is_const[i] && hcg_phi_src_reg[i] == dst) {
                                blocked = 1;
                                break;
                            }
                            i = i + 1;
                        }
                    }
                }
                if (!blocked) {
                    if (hcg_phi_is_const[j] == 2) {
                        /* memory/remat source: load straight into dst */
                        hcg_into(dst, hcg_phi_src_inst[j]);
                    } else if (hcg_phi_is_const[j]) {
                        c = hcg_phi_const_val[j];
                        if (c == 0) cg_rri("addi", dst, 0, 0);
                        else if (hcg_is_i12(c)) cg_rri("addi", dst, 0, c);
                        else hcg_li(dst, c);
                    } else {
                        src = hcg_phi_src_reg[j];
                        if (dst != src) cg_rri("addi", dst, src, 0);
                    }
                    hcg_phi_active[j] = 0;
                    rem = rem - 1;
                    progress = 1;
                }
                j = j + 1;
            }
            if (progress) continue;

            /* Break copy cycle: snapshot one source in temp r2. */
            j = 0;
            srcv = -1;
            while (j < n) {
                if (hcg_phi_active[j] && !hcg_phi_is_const[j]) {
                    dst = hcg_phi_dst_reg[j];
                    src = hcg_phi_src_reg[j];
                    if (dst != src) { srcv = src; break; }
                }
                j = j + 1;
            }
            if (srcv < 0) break;
            cg_rri("addi", 2, srcv, 0);
            j = 0;
            while (j < n) {
                if (hcg_phi_active[j] && !hcg_phi_is_const[j] && hcg_phi_src_reg[j] == srcv) {
                    hcg_phi_src_reg[j] = 2;
                }
                j = j + 1;
            }
        }
        return;
    }

    /* Push non-constant argument values onto runtime stack. */
    npush = 0;
    j = 0;
    while (j < n) {
        v = ssa_phi_find_arg(hcg_phi_tmp[j], from_blk);
        if (hcg_const_imm_inst(v, &c)) {
            hcg_phi_is_const[j] = 1;
            hcg_phi_const_val[j] = c;
            hcg_phi_push_ix[j] = -1;
        } else {
            hcg_phi_is_const[j] = 0;
            hcg_phi_push_ix[j] = npush;
            hcg_into(1, v);
            cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
            npush = npush + 1;
        }
        j = j + 1;
    }

    /* Resolve PHI destinations in reverse order. */
    j = n - 1;
    while (j >= 0) {
        phi = hcg_phi_tmp[j];
        if (hcg_phi_is_const[j]) {
            c = hcg_phi_const_val[j];
            if (ra_reg[phi] >= 0) {
                dreg = ra_reg[phi];
                if (c == 0) cg_rri("addi", dreg, 0, 0);
                else if (hcg_is_i12(c)) cg_rri("addi", dreg, 0, c);
                else hcg_li(dreg, c);
            } else {
                off = ra_spill_off[phi];
                if (off >= -2048 && off <= 2047) {
                    if (c == 0) {
                        cg_s("    stw r30, r0, ");
                        cg_n(off);
                        cg_c(10);
                    } else {
                        if (hcg_is_i12(c)) cg_rri("addi", 1, 0, c);
                        else hcg_li(1, c);
                        cg_s("    stw r30, r1, ");
                        cg_n(off);
                        cg_c(10);
                    }
                } else {
                    hcg_li(2, off);
                    cg_rrr("add", 2, 30, 2);
                    if (c == 0) cg_s("    stw r2, r0, 0\n");
                    else {
                        if (hcg_is_i12(c)) cg_rri("addi", 1, 0, c);
                        else hcg_li(1, c);
                        cg_s("    stw r2, r1, 0\n");
                    }
                }
            }
        } else if (ra_reg[phi] >= 0) {
            /* Load pushed value directly into physical register. */
            dreg = ra_reg[phi];
            cg_s("    ldw r");
            cg_n(dreg);
            cg_s(", r29, ");
            cg_n((npush - 1 - hcg_phi_push_ix[j]) * 4);
            cg_c(10);
        } else {
            /* Load pushed value into r1, then store to spill slot. */
            cg_s("    ldw r1, r29, ");
            cg_n((npush - 1 - hcg_phi_push_ix[j]) * 4);
            cg_c(10);
            off = ra_spill_off[phi];
            if (off >= -2048 && off <= 2047) {
                cg_s("    stw r30, r1, ");
                cg_n(off);
                cg_c(10);
            } else {
                hcg_li(2, off);
                cg_rrr("add", 2, 30, 2);
                cg_s("    stw r2, r1, 0\n");
            }
        }
        j = j - 1;
    }

    /* Clean up runtime stack for pushed PHI args. */
    if (npush > 0) {
        cg_rri("addi", 29, 29, npush * 4);
    }
}

/* --- Binop opcode name lookup --- */

static char *hcg_binop_name(int k) {
    if (k == HI_ADD)  return "add";
    if (k == HI_SUB)  return "sub";
    if (k == HI_MUL)  return "mul";
    if (k == HI_DIV)  return "div";
    if (k == HI_REM)  return "rem";
    if (k == HI_AND)  return "and";
    if (k == HI_OR)   return "or";
    if (k == HI_XOR)  return "xor";
    if (k == HI_SLL)  return "sll";
    if (k == HI_SRA)  return "sra";
    if (k == HI_SRL)  return "srl";
    if (k == HI_SEQ)  return "seq";
    if (k == HI_SNE)  return "sne";
    if (k == HI_SLT)  return "slt";
    if (k == HI_SGT)  return "sgt";
    if (k == HI_SLE)  return "sle";
    if (k == HI_SGE)  return "sge";
    if (k == HI_SLTU) return "sltu";
    if (k == HI_SGTU) return "sgtu";
    if (k == HI_SLEU) return "sleu";
    if (k == HI_SGEU) return "sgeu";
    return "add";
}

/* --- Tail call detection ---
 * Returns 1 if CALL at idx is immediately followed by RET(CALL_result)
 * in the same block, with no CALLHI and no stack args. */
static int hcg_is_tailcall(int idx) {
    int blk;
    int end;
    int j;
    int jk;

    /* Conservative guards */
    if (hcg_va_save_size > 0) return 0;       /* varargs fn */
    if (h_val[idx] > 8) return 0;              /* stack args */
    if (hcg_frame_escapes) return 0;          /* callee may see our frame */

    blk = h_blk[idx];
    end = bb_end[blk];

    /* Scan forward for next non-NOP instruction */
    j = idx + 1;
    while (j < end && h_kind[j] == HI_NOP) j = j + 1;
    if (j >= end) return 0;
    jk = h_kind[j];

    /* CALLHI means 64-bit return — skip tail call */
    if (jk == HI_CALLHI) return 0;

    /* Must be RET with src1 = this CALL */
    if (jk != HI_RET) return 0;
    if (h_src1[j] != idx) return 0;
    if (h_src2[j] >= 0) return 0;             /* 64-bit return value */
    return 1;
}

/* Forward prototype — fixes "static declaration follows non-static" on
 * Alpine gcc and some other strict single-TU builds. */
static void hcg_restore_reg(int reg, int off);

/* Emit inline epilogue for tail call (same as normal epilogue but no return) */
static void hcg_emit_epilogue_inline(void) {
    int i;
    int fs;
    fs = hcg_frame;

    /* Restore callee-saved registers */
    i = 0;
    while (i < ra_ncsave) {
        hcg_restore_reg(ra_csave_reg[i], ra_csave_off[i]);
        i = i + 1;
    }

    /* Restore r31, r30, adjust sp */
    if (fs <= 2047) {
        cg_s("    ldw r31, r29, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    ldw r30, r29, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_rri("addi", 29, 29, fs);
    } else {
        cg_rri("addi", 29, 30, 0);
        cg_s("    ldw r31, r29, -4\n");
        cg_s("    ldw r30, r29, -8\n");
    }
}

/* --- Generate code for one HIR instruction (BURG-dispatched) --- */

/* Conservatively: can a conditional branch in block a reach block b? */
static int hcg_bnear(int a, int b) {
    /* DIVERGENCE (f77, port with the assembler prerequisite): always
     * near.  The range gate predates assembler branch relaxation
     * (GitHub #22): a bcond emitted directly to a target beyond
     * +/-4096 is now rewritten by the assembler into the inverted
     * branch over a jal -- exactly the fallback shape this gate used
     * to force codegen to emit -- so the direct shape is never worse
     * and usually one instruction better.  The phi-free target
     * checks at the call sites still guard correctness. */
    if (a < 0 || b < 0 || a >= bb_nblk || b >= bb_nblk) return 0;
    return 1;
}

/* --- Shared conditional-branch tail ---
 * bt branches when the condition is TRUE (to the taken block), bf
 * when it is FALSE, comparing r<ra> to r<rb>.  Picks the cheapest
 * layout: when the true block is the fallthrough neighbor, one
 * branch-if-false to the false block; when the false block is, one
 * branch-if-true to the true block; else the generic
 * branch-over-jump shape.  The direct shapes require phi-free
 * targets — their edge copies would otherwise be skipped (critical
 * edges are split, so conditional edges are copy-free in practice;
 * the guard keeps this honest). */
static void hcg_condbr_finish(int idx, char *bt, char *bf, int ra, int rb) {
    int t;
    int f;
    int ft;
    int ff;
    int ne;
    int skip;

    t = h_src2[idx];
    f = h_val[idx];
    ft = hcg_fwd[t];
    ff = hcg_fwd[f];
    ne = hcg_next_emit[hcg_cur_blk];

    if (hcg_edge_nocopy(hcg_cur_blk, t) && hcg_edge_nocopy(hcg_cur_blk, f)) {
        if (ft == ne && hcg_bnear(hcg_cur_blk, ff)) {
            cg_s("    ");
            cg_s(bf);
            cg_s(" r");
            cg_n(ra);
            cg_s(", r");
            cg_n(rb);
            cg_s(", ");
            cg_lref(hcg_blk_lbl[ff]);
            cg_c(10);
            hcg_stat_br_fallthru = hcg_stat_br_fallthru + 1;
            return;
        }
        if (ff == ne && hcg_bnear(hcg_cur_blk, ft)) {
            cg_s("    ");
            cg_s(bt);
            cg_s(" r");
            cg_n(ra);
            cg_s(", r");
            cg_n(rb);
            cg_s(", ");
            cg_lref(hcg_blk_lbl[ft]);
            cg_c(10);
            hcg_stat_br_fallthru = hcg_stat_br_fallthru + 1;
            return;
        }
    }

    /* Generic: branch-if-false over the taken jump */
    skip = cg_label();
    cg_s("    ");
    cg_s(bf);
    cg_s(" r");
    cg_n(ra);
    cg_s(", r");
    cg_n(rb);
    cg_s(", ");
    cg_lref(skip);
    cg_c(10);
    hcg_phi_copies(h_blk[idx], t);
    cg_s("    jal r0, ");
    cg_lref(hcg_blk_lbl[ft]);
    cg_c(10);
    cg_ldef(skip);
    hcg_phi_copies(h_blk[idx], f);
    if (ff >= bb_nblk || ff != ne) {
        cg_s("    jal r0, ");
        cg_lref(hcg_blk_lbl[ff]);
        cg_c(10);
    } else {
        hcg_stat_br_fallthru = hcg_stat_br_fallthru + 1;
    }
}

/* Pop a call's stack-arg bytes.  Byval slots can push the total past
 * ADDI's +2047 ceiling, and after the call r1/r2 hold return words, so
 * a big pop is chunked instead of materialized in a scratch reg. */
static void hcg_pop_stack_args(int bytes) {
    while (bytes > 2047) {
        cg_rri("addi", 29, 29, 2044);
        bytes = bytes - 2044;
    }
    if (bytes > 0) {
        cg_rri("addi", 29, 29, bytes);
    }
}

/* Push a call's stack-assigned args in reverse order so the first
 * spilled slot lands at the lowest address.  A byval slot (tag 16+k)
 * reserves k words with the struct-copy pointer stored at the slot
 * base — clang's convention — so a following slot's offset already
 * accounts for the struct's full rounded size. */
static void hcg_push_stack_args(int base, int nargs) {
    int i;
    int t;
    int sz;
    i = nargs - 1;
    while (i >= 0) {
        if (hcg_argmap[i] < 0) {
            t = h_carg_tag[base + i];
            sz = (t >= HI_TAG_BYVAL) ? (t - HI_TAG_BYVAL) * 4 : 4;
            if (sz == 4 && hcg_const_is_zero(h_carg[base + i])) {
                cg_s("    addi r29, r29, -4\n    stw r29, r0, 0\n");
            } else {
                hcg_into(1, h_carg[base + i]);
                if (sz <= 2047) {
                    cg_rri("addi", 29, 29, 0 - sz);
                } else {
                    hcg_li(2, sz);
                    cg_rrr("sub", 29, 29, 2);
                }
                cg_s("    stw r29, r1, 0\n");
            }
        }
        i = i - 1;
    }
}

/* --- HW FP emission for the __fp64_* pair libcalls ---
 * Lowering synthesizes doubles as HI_CALLs to one-instruction wrappers
 * in builtins_fp64.s (fadd.d behind a jal).  Codegen recognises those
 * names and emits the SLOW-32 FP instruction inline instead.  The
 * HI_CALL keeps call clobber semantics for the allocator, and every
 * argument value is call-crossing (ra_mark_call_crossing sees it live
 * AT the call, so it is never colored r3-r10) — marshalling into the
 * even-aligned scratch pairs r4:r5 / r6:r7 therefore cannot disturb
 * an argument's home register.  Results land in r1 (lo) / r2 (hi) so
 * the shared post-call result path runs unchanged.  f64 instructions
 * take EVEN base registers (rd:rd+1); 2-operand forms are written
 * with a trailing r0 like the wrappers and the f32 emitters. */
static int hcg_fp64_kind(char *nm) {
    if (!nm) return -1;
    /* DIVERGENCE FROM selfhost (fortran/ only): __fp32_sqrt and
     * __fp64_sqrt are recognised here so SQRT/DSQRT reach the hardware
     * FSQRT.S / FSQRT.D instructions.  The C compiler routes sqrt
     * through HI_FSQRT, which this backend does not implement -- and
     * silently emits NOTHING for -- so Fortran would otherwise have to
     * call a libm function whose entire body is one instruction. */
    if (nm[0] == '_' && nm[1] == '_' && nm[2] == 'f' && nm[3] == 'p' &&
        nm[4] == '3' && nm[5] == '2' && nm[6] == '_' &&
        strcmp(nm + 7, "sqrt") == 0) return 15;
    if (nm[0] != '_' || nm[1] != '_' || nm[2] != 'f' || nm[3] != 'p' ||
        nm[4] != '6' || nm[5] != '4' || nm[6] != '_') return -1;
    nm = nm + 7;
    if (strcmp(nm, "sqrt") == 0) return 14;
    if (strcmp(nm, "add") == 0) return 0;
    if (strcmp(nm, "sub") == 0) return 1;
    if (strcmp(nm, "mul") == 0) return 2;
    if (strcmp(nm, "div") == 0) return 3;
    if (strcmp(nm, "neg") == 0) return 4;
    if (strcmp(nm, "eq") == 0) return 5;
    if (strcmp(nm, "lt") == 0) return 6;
    if (strcmp(nm, "le") == 0) return 7;
    if (strcmp(nm, "cvt_itoD") == 0) return 8;
    if (strcmp(nm, "cvt_ftoD") == 0) return 9;
    if (strcmp(nm, "cvt_DtoI") == 0) return 10;
    if (strcmp(nm, "cvt_DtoF") == 0) return 11;
    if (strcmp(nm, "cvt_ltoD") == 0) return 12;
    if (strcmp(nm, "cvt_DtoL") == 0) return 13;
    return -1;
}

/* DIVERGENCE FROM selfhost (fortran/ only), part 2 of 2.
 *
 * fadd.d/fmul.d etc. address a register PAIR (r_n, r_n+1) and require n
 * even (CHECK_F64_REG).  The helper-call shape forced every operand
 * through the fixed r4:r5 / r6:r7 pair and returned via r1:r2, costing
 * ~8 register moves around a single instruction -- measured at 23 moves
 * per fp64 op against clang's 0.6 (fortran/bench/RESULTS.md).
 *
 * If the allocator already placed a double's two halves in an aligned
 * consecutive pair, the instruction can name that pair directly and the
 * moves vanish.  This checks for that; hcg_pair_pref() below then makes
 * it happen often rather than by luck. */
static int hcg_fp64_cur = -1;   /* CALL being inlined, or -1 */
static int hcg_fp64_dst = -1;   /* home pair for its result, or -1 */
static int hcg_fp64_spill_direct;  /* result will be spilled from r4:r5 */

static int hcg_pair_reg(int lo_inst, int hi_inst) {
    int a;
    int b;
    if (lo_inst < 0 || hi_inst < 0) return -1;
    a = ra_reg[lo_inst];
    b = ra_reg[hi_inst];
    if (a < 0 || b < 0) return -1;      /* spilled: no pair */
    if (a & 1) return -1;               /* must be even */
    if (a >= 31) return -1;
    if (b != a + 1) return -1;
    return a;
}

static void hcg_fp64_emit(int fpk, int base) {
    /* Binary arithmetic and compares.  Each operand uses its own aligned
     * pair when the allocator gave it one, otherwise it is moved into
     * the scratch pair (r4:r5 for the left, r6:r7 for the right). */
    if (fpk <= 3 || (fpk >= 5 && fpk <= 7)) {
        int pa;
        int pb;
        pa = hcg_pair_reg(h_carg[base + 0], h_carg[base + 1]);
        pb = hcg_pair_reg(h_carg[base + 2], h_carg[base + 3]);

        /* The right operand's scratch pair is r6:r7; if the left operand
         * already lives there, move it out first so loading the right
         * cannot clobber it. */
        if (pa == 6 && pb < 0) {
            cg_rri("addi", 4, 6, 0);
            cg_rri("addi", 5, 7, 0);
            pa = 4;
        }
        if (pa < 0) {
            hcg_into(4, h_carg[base + 0]);
            hcg_into(5, h_carg[base + 1]);
            pa = 4;
        }
        if (pb < 0) {
            hcg_into(6, h_carg[base + 2]);
            hcg_into(7, h_carg[base + 3]);
            pb = 6;
        }

        if (fpk == 5) { cg_rrr("feq.d", 1, pa, pb); return; }
        if (fpk == 6) { cg_rrr("flt.d", 1, pa, pb); return; }
        if (fpk == 7) { cg_rrr("fle.d", 1, pa, pb); return; }

        /* Write the result straight into its home pair when it has one,
         * which also skips the r1:r2 hop the generic call path uses. */
        hcg_fp64_dst = -1;
        hcg_fp64_spill_direct = 0;
        if (hcg_fp64_cur >= 0) {
            hcg_fp64_dst = hcg_pair_reg(hcg_fp64_cur, hcg_fp64_cur + 1);
            if (hcg_fp64_dst < 0 && ra_reg[hcg_fp64_cur] < 0 &&
                ra_reg[hcg_fp64_cur + 1] < 0)
                hcg_fp64_spill_direct = 1;
        }
        {
            int rd;
            rd = (hcg_fp64_dst >= 0) ? hcg_fp64_dst : 4;
            if (fpk == 0) cg_rrr("fadd.d", rd, pa, pb);
            else if (fpk == 1) cg_rrr("fsub.d", rd, pa, pb);
            else if (fpk == 2) cg_rrr("fmul.d", rd, pa, pb);
            else cg_rrr("fdiv.d", rd, pa, pb);
            if (hcg_fp64_dst < 0 && !hcg_fp64_spill_direct) {
                cg_rri("addi", 1, 4, 0);
                cg_rri("addi", 2, 5, 0);
            }
        }
        return;
    }
    /* Square root, f64: pair r4:r5 (see the divergence note above). */
    if (fpk == 14) {
        hcg_into(4, h_carg[base + 0]);
        hcg_into(5, h_carg[base + 1]);
        cg_rrr("fsqrt.d", 4, 4, 0);
        cg_rri("addi", 1, 4, 0);
        cg_rri("addi", 2, 5, 0);
        return;
    }
    /* Square root, f32: single word. */
    if (fpk == 15) {
        hcg_into(3, h_carg[base + 0]);
        cg_rrr("fsqrt.s", 1, 3, 0);
        return;
    }
    /* Negate: pair r4:r5 */
    if (fpk == 4) {
        hcg_into(4, h_carg[base + 0]);
        hcg_into(5, h_carg[base + 1]);
        cg_rrr("fneg.d", 4, 4, 0);
        cg_rri("addi", 1, 4, 0);
        cg_rri("addi", 2, 5, 0);
        return;
    }
    /* int/float word → double pair */
    if (fpk == 8 || fpk == 9) {
        hcg_into(3, h_carg[base + 0]);
        if (fpk == 8) cg_rrr("fcvt.d.w", 4, 3, 0);
        else cg_rrr("fcvt.d.s", 4, 3, 0);
        cg_rri("addi", 1, 4, 0);
        cg_rri("addi", 2, 5, 0);
        return;
    }
    /* double pair → int/float word */
    if (fpk == 10 || fpk == 11) {
        hcg_into(4, h_carg[base + 0]);
        hcg_into(5, h_carg[base + 1]);
        if (fpk == 10) cg_rrr("fcvt.w.d", 1, 4, 0);
        else cg_rrr("fcvt.s.d", 1, 4, 0);
        return;
    }
    /* llong pair ↔ double pair */
    hcg_into(4, h_carg[base + 0]);
    hcg_into(5, h_carg[base + 1]);
    if (fpk == 12) cg_rrr("fcvt.d.l", 4, 4, 0);
    else cg_rrr("fcvt.l.d", 4, 4, 0);
    cg_rri("addi", 1, 4, 0);
    cg_rri("addi", 2, 5, 0);
}

/* True when this ADDI is only ever consumed as the address of a
 * LOAD/STORE that will fold it into a 12-bit displacement, so emitting
 * it would produce a dead instruction.  DCE cannot know this: it runs
 * long before BURG chooses the fold.  Every `x = base + 4` feeding the
 * hi word of a double load was costing one dead instruction.
 */
static int hcg_addi_lnt(int i) {
    int pat;
    pat = bg_sel[i];
    if (pat >= 0) return bg_plnt[pat];
    if (h_src1[i] >= 0 && h_kind[h_src1[i]] == HI_ALLOCA) return BG_FADDR;
    return -1;
}

static int hcg_dbg_addi[6];
static int hcg_addi_folds_away_d(int idx, int depth);

static int hcg_addi_folds_away(int idx) {
    return hcg_addi_folds_away_d(idx, 0);
}

static int hcg_addi_folds_away_d(int idx, int depth) {
    int i;
    int users;
    int folded;
    int base_i;
    int off;
    int lnt_i;

    hcg_dbg_addi[0]++;
    if (depth > 4) return 0;
    if (h_kind[idx] != HI_ADDI) return 0;
    if (ra_reg[idx] < 0) { hcg_dbg_addi[1]++; return 0; }
    if (bg_uses[idx] <= 0) { hcg_dbg_addi[2]++; return 0; }

    users = 0;
    folded = 0;
    i = 0;
    while (i < h_ninst) {
        int k2;
        k2 = h_kind[i];
        if (h_src1[i] == idx && (k2 == HI_LOAD || k2 == HI_STORE)) {
            users = users + 1;
            lnt_i = hcg_addi_lnt(i);
            if (lnt_i != BG_FADDR && lnt_i != BG_SADDR &&
                hcg_addr_base_off(idx, &base_i, &off) && hcg_is_i12(off))
                folded = folded + 1;
        } else if (h_src1[i] == idx && k2 == HI_ADDI) {
            /* DIVERGENCE (f77, port upstream): an ADDI stacked on an
             * ADDI -- a double's +4 hi-word address on top of a folded
             * subscript displacement -- counts as folded exactly when
             * the whole chain above it folds.  The address-fold walk
             * (hcg_addr_base_off) already sees through chains; this
             * predicate refused them and materialized dead ADDIs. */
            users = users + 1;
            if (hcg_addi_folds_away_d(i, depth + 1)) folded = folded + 1;
        } else if (h_src1[i] == idx || h_src2[i] == idx) {
            hcg_dbg_addi[3]++;
            return 0;                        /* used as a value somewhere */
        }
        i = i + 1;
    }
    /* Requiring users == bg_uses[idx] proves this scan saw EVERY use --
     * bg_uses also counts call arguments and phi operands, which this
     * walk does not look at.  One ADDI commonly feeds both the hi load
     * and the hi store of the same double, so several uses is normal;
     * what matters is that they all fold. */
    if (!(users > 0 && users == folded && users == bg_uses[idx])) {
        hcg_dbg_addi[4]++;
        return 0;
    }
    hcg_dbg_addi[5]++;
    return 1;
}

static void hcg_inst(int idx) {
    int k;
    int ty;
    int s1;
    int s2;
    int pat;
    int lnt;
    int rnt;
    int nargs;
    int regc;
    int nstk;
    regc = 0;
    nstk = regc;
    int base;
    int i;
    int skip;
    int off;
    int rs1;
    int rs2;
    int rd;
    int vreg;
    int cond;
    int base_i;
    int n;
    int tmp;

    k = h_kind[idx];
    if (k == HI_NOP) return;
    if (hi_inst_remat(idx)) return;
    if (k == HI_PHI) return;

    /* Promoted constant (h_no_remat): materialize once at the def. */
    if (k == HI_ICONST) {
        rd = hcg_dst(idx);
        if (h_val[idx] == 0) cg_rri("addi", rd, 0, 0);
        else if (hcg_is_i12(h_val[idx])) cg_rri("addi", rd, 0, h_val[idx]);
        else hcg_li(rd, h_val[idx]);
        hcg_maybe_spill(idx);
        return;
    }

    ty = h_ty[idx];
    s1 = h_src1[idx];
    s2 = h_src2[idx];
    pat = bg_sel[idx];

    /* Determine left child NT from selected pattern.
     * If pat < 0 (BURG skipped or chain rule), fall back to
     * checking h_kind[s1] directly (same as original codegen). */
    lnt = -1;
    rnt = -1;
    if (pat >= 0) {
        lnt = bg_plnt[pat];
        rnt = bg_prnt[pat];
    } else if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
        lnt = BG_FADDR;
    }

    if (k == HI_PARAM) {
        /* The first HI_PARAM emits the whole cycle-safe entry
         * sequence (register moves, spill stores, stack loads) for
         * every param; the rest emit nothing.  See
         * hcg_emit_param_entry for why per-param emission is unsafe
         * once the allocator may permute the ABI registers. */
        if (!hcg_params_emitted) {
            hcg_params_emitted = 1;
            hcg_emit_param_entry();
        }
        return;
    }

    if (k == HI_GETFP) {
        rd = hcg_dst(idx);
        cg_rri("addi", rd, 30, 0);
        hcg_maybe_spill(idx);
        return;
    }

    /* ADD/SUB with immediate operand */
    if (k == HI_ADD) {
        int imm_opp;
        int c;
        int imm_ok;
        int base_inst;
        imm_opp = 0;
        imm_ok = 0;
        base_inst = -1;
        if (hcg_const_imm_inst(s2, &c) && hcg_is_i12(c)) {
            imm_opp = 1;
            off = c;
            imm_ok = 1;
            base_inst = s1;
        } else if (hcg_const_imm_inst(s1, &c) && hcg_is_i12(c)) {
            imm_opp = 1;
            off = c;
            imm_ok = 1;
            base_inst = s2;
        } else if (pat >= 0 &&
                   ((lnt == BG_REG && rnt == BG_IMM) || (lnt == BG_IMM && rnt == BG_REG))) {
            if (lnt == BG_REG) c = h_val[s2];
            else c = h_val[s1];
            if (hcg_is_i12(c)) {
                imm_opp = 1;
                imm_ok = 1;
            }
            if (lnt == BG_REG) {
                base_inst = s1;
                off = c;
            } else {
                base_inst = s2;
                off = c;
            }
        }
        if (imm_opp) hcg_stat_imm_opp_add = hcg_stat_imm_opp_add + 1;
        if (imm_ok) {
            rd = hcg_dst(idx);
            rs1 = hcg_src(base_inst, 1);
            cg_rri("addi", rd, rs1, off);
            hcg_stat_imm_hit_add = hcg_stat_imm_hit_add + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_add = hcg_stat_imm_miss_add + 1;
    }

    if (k == HI_SUB) {
        int imm_opp;
        int c;
        int imm_ok;
        imm_opp = 0;
        imm_ok = 0;
        if (hcg_const_imm_inst(s2, &c)) {
            if (c != -2147483647 - 1 && hcg_is_i12(0 - c)) {
                imm_opp = 1;
                imm_ok = 1;
                off = c;
            }
        } else if (pat >= 0 && lnt == BG_REG && rnt == BG_IMM) {
            c = h_val[s2];
            if (c != -2147483647 - 1 && hcg_is_i12(0 - c)) {
                imm_opp = 1;
                imm_ok = 1;
                off = c;
            }
        }
        if (imm_opp) hcg_stat_imm_opp_sub = hcg_stat_imm_opp_sub + 1;
        if (imm_ok) {
            rd = hcg_dst(idx);
            rs1 = hcg_src(s1, 1);
            off = 0 - off;
            cg_rri("addi", rd, rs1, off);
            hcg_stat_imm_hit_sub = hcg_stat_imm_hit_sub + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_sub = hcg_stat_imm_miss_sub + 1;
    }

    if (k == HI_AND || k == HI_OR || k == HI_XOR) {
        char *opi;
        int imm_opp;
        int c;
        int imm_ok;
        int base_inst;
        rd = hcg_dst(idx);
        imm_opp = 0;
        imm_ok = 0;
        base_inst = -1;

        if (hcg_const_imm_inst(s2, &c) && hcg_is_u12(c)) {
            imm_opp = 1;
            off = c;
            imm_ok = 1;
            base_inst = s1;
        } else if (hcg_const_imm_inst(s1, &c) && hcg_is_u12(c)) {
            imm_opp = 1;
            off = c;
            imm_ok = 1;
            base_inst = s2;
        } else if (pat >= 0 &&
                   ((lnt == BG_REG && rnt == BG_IMM) || (lnt == BG_IMM && rnt == BG_REG))) {
            if (lnt == BG_REG) c = h_val[s2];
            else c = h_val[s1];
            if (hcg_is_u12(c)) {
                imm_opp = 1;
                imm_ok = 1;
            }
            if (lnt == BG_REG) {
                base_inst = s1;
                off = c;
            } else {
                base_inst = s2;
                off = c;
            }
        }
        if (imm_opp) hcg_stat_imm_opp_logic = hcg_stat_imm_opp_logic + 1;
        if (!imm_ok) {
            /* fall through to generic reg-reg emission */
        } else {
            rs1 = hcg_src(base_inst, 1);
            if (k == HI_AND) opi = "andi";
            else if (k == HI_OR) opi = "ori";
            else opi = "xori";
            cg_rri(opi, rd, rs1, off);
            hcg_stat_imm_hit_logic = hcg_stat_imm_hit_logic + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_logic = hcg_stat_imm_miss_logic + 1;
    }

    if (k == HI_SLL || k == HI_SRL || k == HI_SRA) {
        char *opi;
        int imm_opp;
        int c;
        int have_imm;
        rd = hcg_dst(idx);
        imm_opp = 0;
        have_imm = 0;
        if (hcg_const_imm_inst(s2, &c) && c >= 0 && c <= 31) {
            imm_opp = 1;
            have_imm = 1;
            off = c;
        }
        if (!have_imm && pat >= 0 && lnt == BG_REG && rnt == BG_IMM) {
            c = h_val[s2];
            if (c >= 0 && c <= 31) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
            }
        }
        if (imm_opp) hcg_stat_imm_opp_shift = hcg_stat_imm_opp_shift + 1;
        if (have_imm) {
            rs1 = hcg_src(s1, 1);
            if (k == HI_SLL) opi = "slli";
            else if (k == HI_SRL) opi = "srli";
            else opi = "srai";
            cg_rri(opi, rd, rs1, off);
            hcg_stat_imm_hit_shift = hcg_stat_imm_hit_shift + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_shift = hcg_stat_imm_miss_shift + 1;
    }

    if (k == HI_SLT || k == HI_SLTU) {
        int imm_opp;
        int c;
        int have_imm;
        imm_opp = 0;
        have_imm = 0;
        if (hcg_const_imm_inst(s2, &c) && hcg_is_i12(c)) {
            imm_opp = 1;
            have_imm = 1;
            off = c;
        }
        if (!have_imm && pat >= 0 && lnt == BG_REG && rnt == BG_IMM) {
            c = h_val[s2];
            if (hcg_is_i12(c)) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
            }
        }
        if (imm_opp) hcg_stat_imm_opp_cmp = hcg_stat_imm_opp_cmp + 1;
        if (have_imm) {
            rd = hcg_dst(idx);
            rs1 = hcg_src(s1, 1);
            if (k == HI_SLT) cg_rri("slti", rd, rs1, off);
            else cg_rri("sltiu", rd, rs1, off);
            hcg_stat_imm_hit_cmp = hcg_stat_imm_hit_cmp + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_cmp = hcg_stat_imm_miss_cmp + 1;
    }

    if (k == HI_SGT || k == HI_SGTU || k == HI_SGE || k == HI_SGEU ||
        k == HI_SLE || k == HI_SLEU) {
        int imm_opp;
        int c;
        int have_imm;
        int invert;
        int const_res;
        int const_valid;
        imm_opp = 0;
        have_imm = 0;
        invert = 0;
        const_valid = 0;
        const_res = 0;

        if ((k == HI_SGT || k == HI_SGTU) && hcg_const_imm_inst(s1, &c) && hcg_is_i12(c)) {
            /* c > x   => x < c */
            imm_opp = 1;
            have_imm = 1;
            off = c;
            rs1 = hcg_src(s2, 2);
        } else if ((k == HI_SGE || k == HI_SGEU) && hcg_const_imm_inst(s2, &c) && hcg_is_i12(c)) {
            /* x >= c  => !(x < c) */
            imm_opp = 1;
            have_imm = 1;
            off = c;
            rs1 = hcg_src(s1, 1);
            invert = 1;
        } else if ((k == HI_SGT || k == HI_SGTU) &&
                   hcg_const_imm_inst(s2, &c)) {
            /* x > c  => !(x < c+1) */
            imm_opp = 1;
            if (k == HI_SGT && c == 2147483647) {
                const_valid = 1;
                const_res = 0;
            } else if (k == HI_SGTU && c == -1) {
                const_valid = 1;
                const_res = 0;
            } else if (hcg_is_i12(c + 1)) {
                have_imm = 1;
                off = c + 1;
                rs1 = hcg_src(s1, 1);
                invert = 1;
            }
        } else if ((k == HI_SLE || k == HI_SLEU) &&
                   hcg_const_imm_inst(s2, &c)) {
            /* x <= c => x < c+1 */
            imm_opp = 1;
            if (k == HI_SLE && c == 2147483647) {
                const_valid = 1;
                const_res = 1;
            } else if (k == HI_SLEU && c == -1) {
                const_valid = 1;
                const_res = 1;
            } else if (hcg_is_i12(c + 1)) {
                have_imm = 1;
                off = c + 1;
                rs1 = hcg_src(s1, 1);
            }
        } else if ((k == HI_SLE || k == HI_SLEU) &&
                   hcg_const_imm_inst(s1, &c) && hcg_is_i12(c)) {
            /* c <= x => !(x < c) */
            imm_opp = 1;
            have_imm = 1;
            off = c;
            rs1 = hcg_src(s2, 2);
            invert = 1;
        } else if (pat >= 0 && lnt == BG_IMM && rnt == BG_REG &&
                   (k == HI_SGT || k == HI_SGTU || k == HI_SLE || k == HI_SLEU)) {
            c = h_val[s1];
            if (hcg_is_i12(c)) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
                rs1 = hcg_src(s2, 2);
                if (k == HI_SLE || k == HI_SLEU) invert = 1;
            }
        } else if (pat >= 0 && lnt == BG_REG && rnt == BG_IMM &&
                   (k == HI_SGE || k == HI_SGEU)) {
            c = h_val[s2];
            if (hcg_is_i12(c)) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
                rs1 = hcg_src(s1, 1);
                invert = 1;
            }
        } else if (pat >= 0 && lnt == BG_REG && rnt == BG_IMM &&
                   (k == HI_SGT || k == HI_SGTU || k == HI_SLE || k == HI_SLEU)) {
            c = h_val[s2];
            imm_opp = 1;
            if ((k == HI_SGT && c == 2147483647) || (k == HI_SGTU && c == -1)) {
                const_valid = 1;
                const_res = 0;
            } else if ((k == HI_SLE && c == 2147483647) || (k == HI_SLEU && c == -1)) {
                const_valid = 1;
                const_res = 1;
            } else if (hcg_is_i12(c + 1)) {
                have_imm = 1;
                off = c + 1;
                rs1 = hcg_src(s1, 1);
                if (k == HI_SGT || k == HI_SGTU) invert = 1;
            }
        }

        if (imm_opp) hcg_stat_imm_opp_cmp = hcg_stat_imm_opp_cmp + 1;
        if (const_valid) {
            rd = hcg_dst(idx);
            cg_rri("addi", rd, 0, const_res);
            hcg_stat_imm_hit_cmp = hcg_stat_imm_hit_cmp + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (have_imm) {
            rd = hcg_dst(idx);
            if (k == HI_SGT || k == HI_SGE || k == HI_SLE) cg_rri("slti", rd, rs1, off);
            else cg_rri("sltiu", rd, rs1, off);
            if (invert) cg_rrr("seq", rd, rd, 0);
            hcg_stat_imm_hit_cmp = hcg_stat_imm_hit_cmp + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_cmp = hcg_stat_imm_miss_cmp + 1;
    }

    if (k == HI_SEQ || k == HI_SNE) {
        int imm_opp;
        int c;
        int have_imm;
        int have_addi_cmp;
        imm_opp = 0;
        have_imm = 0;
        have_addi_cmp = 0;
        if (hcg_const_imm_inst(s2, &c) && hcg_is_u12(c)) {
            imm_opp = 1;
            have_imm = 1;
            off = c;
            rs1 = hcg_src(s1, 1);
        } else if (hcg_const_imm_inst(s1, &c) && hcg_is_u12(c)) {
            imm_opp = 1;
            have_imm = 1;
            off = c;
            rs1 = hcg_src(s2, 2);
        } else if (pat >= 0 && lnt == BG_REG && rnt == BG_IMM) {
            c = h_val[s2];
            if (hcg_is_u12(c)) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
                rs1 = hcg_src(s1, 1);
            }
        } else if (pat >= 0 && lnt == BG_IMM && rnt == BG_REG) {
            c = h_val[s1];
            if (hcg_is_u12(c)) {
                imm_opp = 1;
                have_imm = 1;
                off = c;
                rs1 = hcg_src(s2, 2);
            }
        }
        if (imm_opp) hcg_stat_imm_opp_cmp = hcg_stat_imm_opp_cmp + 1;
        if (have_imm) {
            rd = hcg_dst(idx);
            cg_rri("xori", rd, rs1, off);
            if (k == HI_SEQ) cg_rrr("seq", rd, rd, 0);
            else cg_rrr("sne", rd, rd, 0);
            hcg_stat_imm_hit_cmp = hcg_stat_imm_hit_cmp + 1;
            hcg_maybe_spill(idx);
            return;
        }
        /* Signed-12 equality sink: x == c  => (x + (-c)) == 0.
         * This avoids separate constant materialization for small negative constants. */
        if (hcg_const_imm_inst(s2, &c)) {
            if (c != (-2147483647 - 1)) {
                off = 0 - c;
                if (hcg_is_i12(off)) {
                    have_addi_cmp = 1;
                    rs1 = hcg_src(s1, 1);
                }
            }
        } else if (hcg_const_imm_inst(s1, &c)) {
            if (c != (-2147483647 - 1)) {
                off = 0 - c;
                if (hcg_is_i12(off)) {
                    have_addi_cmp = 1;
                    rs1 = hcg_src(s2, 2);
                }
            }
        }
        if (have_addi_cmp) {
            rd = hcg_dst(idx);
            cg_rri("addi", rd, rs1, off);
            if (k == HI_SEQ) cg_rrr("seq", rd, rd, 0);
            else cg_rrr("sne", rd, rd, 0);
            hcg_stat_imm_hit_cmp = hcg_stat_imm_hit_cmp + 1;
            hcg_maybe_spill(idx);
            return;
        }
        if (imm_opp) hcg_stat_imm_miss_cmp = hcg_stat_imm_miss_cmp + 1;
    }

    /* Signed DIV/REM by power-of-2 — peephole at codegen time.
     * Signed DIV by 2^n:  srai rT,rS,31; srli rT,rT,(32-n); add rD,rS,rT; srai rD,rD,n
     * Signed REM by 2^n:  srai rT,rS,31; srli rT,rT,(32-n); add rT,rS,rT;
     *                     srai rT,rT,n; slli rT,rT,n; sub rD,rS,rT
     * Only for signed (not TY_UNSIGNED). Unsigned already handled by hir_opt.h. */
    if ((k == HI_DIV || k == HI_REM) && !(ty & TY_UNSIGNED) &&
        hcg_const_imm_inst(s2, &off) && off > 1 && (off & (off - 1)) == 0) {
        n = 0;
        tmp = off;
        while (tmp > 1) { n = n + 1; tmp = tmp >> 1; }
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        /* r2 = bias: srai r2, rS, 31 -> srli r2, r2, (32-n) */
        cg_rri("srai", 2, rs1, 31);
        cg_rri("srli", 2, 2, 32 - n);
        if (k == HI_DIV) {
            /* add rD, rS, r2; srai rD, rD, n */
            cg_rrr("add", rd, rs1, 2);
            cg_rri("srai", rd, rd, n);
        } else {
            /* add r2, rS, r2; srai r2, r2, n; slli r2, r2, n; sub rD, rS, r2 */
            cg_rrr("add", 2, rs1, 2);
            cg_rri("srai", 2, 2, n);
            cg_rri("slli", 2, 2, n);
            cg_rrr("sub", rd, rs1, 2);
        }
        hcg_stat_divrem_pow2 = hcg_stat_divrem_pow2 + 1;
        hcg_maybe_spill(idx);
        return;
    }

    /* Binary arithmetic/logic/comparison */
    if (k >= HI_ADD && k <= HI_SGEU) {
        if (hcg_const_imm_inst(s1, &off) && off == 0) rs1 = 0;
        else rs1 = hcg_src(s1, 1);
        if (hcg_const_imm_inst(s2, &off) && off == 0) rs2 = 0;
        else rs2 = hcg_src(s2, 2);
        rd = hcg_dst(idx);
        cg_rrr(hcg_binop_name(k), rd, rs1, rs2);
        hcg_maybe_spill(idx);
        return;
    }

    /* Floating-point binary arithmetic (f32) */
    if (k >= HI_FADD && k <= HI_FDIV) {
        rs1 = hcg_src(s1, 1);
        rs2 = hcg_src(s2, 2);
        rd = hcg_dst(idx);
        if (k == HI_FADD) cg_rrr("fadd.s", rd, rs1, rs2);
        else if (k == HI_FSUB) cg_rrr("fsub.s", rd, rs1, rs2);
        else if (k == HI_FMUL) cg_rrr("fmul.s", rd, rs1, rs2);
        else if (k == HI_FDIV) cg_rrr("fdiv.s", rd, rs1, rs2);
        hcg_maybe_spill(idx);
        return;
    }

    /* Floating-point negate (f32) */
    if (k == HI_FNEG) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rrr("fneg.s", rd, rs1, 0);
        hcg_maybe_spill(idx);
        return;
    }

    /* Floating-point comparisons (f32) → int result */
    if (k == HI_FEQ || k == HI_FLT || k == HI_FLE) {
        rs1 = hcg_src(s1, 1);
        rs2 = hcg_src(s2, 2);
        rd = hcg_dst(idx);
        if (k == HI_FEQ) cg_rrr("feq.s", rd, rs1, rs2);
        else if (k == HI_FLT) cg_rrr("flt.s", rd, rs1, rs2);
        else cg_rrr("fle.s", rd, rs1, rs2);
        hcg_maybe_spill(idx);
        return;
    }

    /* Float-int conversions (f32) */
    if (k == HI_FCVT_ItoF) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rrr("fcvt.s.w", rd, rs1, 0);
        hcg_maybe_spill(idx);
        return;
    }
    if (k == HI_FCVT_FtoI) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rrr("fcvt.w.s", rd, rs1, 0);
        hcg_maybe_spill(idx);
        return;
    }

    /* Unary: negate */
    if (k == HI_NEG) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rrr("sub", rd, 0, rs1);
        hcg_maybe_spill(idx);
        return;
    }

    /* Unary: logical not */
    if (k == HI_NOT) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rrr("seq", rd, rs1, 0);
        hcg_maybe_spill(idx);
        return;
    }

    /* Unary: bitwise not — xori zero-extends, so use addi+xor */
    if (k == HI_BNOT) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rri("addi", 2, 0, -1);
        cg_rrr("xor", rd, rs1, 2);
        hcg_maybe_spill(idx);
        return;
    }

    /* Load — dispatched by BURG left-child NT */
    if (k == HI_LOAD) {
        rd = hcg_dst(idx);
        if (lnt == BG_FADDR) {
            /* LOAD(faddr): direct load from fp + offset */
            off = bg_foff[s1];
            if (off >= -2048 && off <= 2047) {
                hcg_load_off(rd, 30, off, ty);
            } else {
                hcg_li(rd, off);
                cg_rrr("add", rd, 30, rd);
                hcg_load_mem(rd, rd, ty);
            }
        } else if (lnt == BG_SADDR) {
            /* LOAD(saddr): lui + ldw with %lo */
            hcg_load_saddr(rd, s1, ty);
        } else {
            /* LOAD(reg): try to fold ADDI-chain base+offset shape. */
            if (hcg_addr_base_off(s1, &base_i, &off) && hcg_is_i12(off)) {
                rs1 = hcg_src(base_i, 1);
                hcg_load_off(rd, rs1, off, ty);
            } else {
                rs1 = hcg_src(s1, 1);
                hcg_load_mem(rd, rs1, ty);
            }
        }
        hcg_maybe_spill(idx);
        return;
    }

    /* Store — dispatched by BURG left-child NT */
    if (k == HI_STORE) {
        if (lnt == BG_FADDR) {
            /* STORE(faddr, reg): direct store to fp + offset */
            off = bg_foff[s1];
            if (hcg_const_is_zero(s2)) vreg = 0;
            else vreg = hcg_src(s2, 2);
            if (off >= -2048 && off <= 2047) {
                hcg_store_off(30, vreg, off, ty);
            } else {
                hcg_li(1, off);
                cg_rrr("add", 1, 30, 1);
                hcg_store_mem(1, vreg, ty);
            }
        } else if (lnt == BG_SADDR) {
            /* STORE(saddr, reg): lui + stw with %lo */
            if (hcg_const_is_zero(s2)) vreg = 0;
            else vreg = hcg_src(s2, 2);
            hcg_store_saddr(vreg, s1, ty);
        } else {
            /* STORE(reg, reg) */
            if (hcg_addr_base_off(s1, &base_i, &off) && hcg_is_i12(off)) {
                rs1 = hcg_src(base_i, 1);
                if (hcg_const_is_zero(s2)) vreg = 0;
                else vreg = hcg_src(s2, 2);
                hcg_store_off(rs1, vreg, off, ty);
            } else {
                rs1 = hcg_src(s1, 1);
                if (hcg_const_is_zero(s2)) vreg = 0;
                else vreg = hcg_src(s2, 2);
                hcg_store_mem(rs1, vreg, ty);
            }
        }
        return;
    }

    /* ADDI — dispatched by BURG left-child NT */
    if (k == HI_ADDI) {
        /* Suppress an address ADDI that every consumer folds into its
         * own displacement -- see hcg_addi_folds_away. */
        if (hcg_addi_folds_away(idx)) return;
        rd = hcg_dst(idx);
        if (lnt == BG_FADDR) {
            /* ADDI(faddr, imm): combined offset precomputed */
            off = bg_foff[idx];
            if (off >= -2048 && off <= 2047) {
                cg_rri("addi", rd, 30, off);
            } else {
                hcg_li(rd, off);
                cg_rrr("add", rd, 30, rd);
            }
        } else {
            /* ADDI(reg, imm): fold addi chains at emission time */
            off = h_val[idx];
            base_i = s1;
            while (base_i >= 0 && h_kind[base_i] == HI_ADDI) {
                off = off + h_val[base_i];
                base_i = h_src1[base_i];
            }
            rs1 = hcg_src(base_i, 1);
            if (off == 0) {
                /* Zero-offset add is a move/no-op at emission time. */
                if (rd != rs1) {
                    cg_rri("addi", rd, rs1, 0);
                } else {
                    hcg_stat_addi0_elide = hcg_stat_addi0_elide + 1;
                }
            } else if (off >= -2048 && off <= 2047) {
                cg_rri("addi", rd, rs1, off);
            } else {
                hcg_li(2, off);
                cg_rrr("add", rd, rs1, 2);
            }
        }
        hcg_maybe_spill(idx);
        return;
    }

    /* Branch (unconditional) */
    if (k == HI_BR) {
        int tb;
        hcg_phi_copies(h_blk[idx], h_val[idx]);
        tb = hcg_fwd[h_val[idx]];
        /* Fallthrough elimination: skip jal if target is the next
         * emitted (non-forwarded) block */
        if (tb >= bb_nblk || tb != hcg_next_emit[hcg_cur_blk]) {
            cg_s("    jal r0, ");
            cg_lref(hcg_blk_lbl[tb]);
            cg_c(10);
        } else {
            hcg_stat_br_fallthru = hcg_stat_br_fallthru + 1;
        }
        return;
    }

    /* Conditional branch */
    if (k == HI_BRC) {
        int cmp_idx;
        int ck;
        int ca;
        int cb;
        int ra;
        int rb;
        char *brop;
        char *bropt;
        int fall_blk;

        if (hcg_const_imm_inst(s1, &off)) {
            if (off != 0) {
                int tb2;
                hcg_phi_copies(h_blk[idx], s2);
                tb2 = hcg_fwd[s2];
                if (tb2 >= bb_nblk || tb2 != hcg_next_emit[hcg_cur_blk]) {
                    cg_s("    jal r0, ");
                    cg_lref(hcg_blk_lbl[tb2]);
                    cg_c(10);
                }
            } else {
                int tb2;
                hcg_phi_copies(h_blk[idx], h_val[idx]);
                tb2 = hcg_fwd[h_val[idx]];
                if (tb2 >= bb_nblk || tb2 != hcg_next_emit[hcg_cur_blk]) {
                    cg_s("    jal r0, ");
                    cg_lref(hcg_blk_lbl[tb2]);
                    cg_c(10);
                } else {
                    hcg_stat_br_fallthru = hcg_stat_br_fallthru + 1;
                }
            }
            return;
        }

        /* Compare-and-branch fusion */
        cmp_idx = hcg_brc_fuse[idx];
        if (cmp_idx >= 0) {
            ck = hcg_cmp_kind[cmp_idx];
            ca = h_src1[cmp_idx];
            cb = h_src2[cmp_idx];

            if (ck == HI_SEQ || ck == HI_SNE || ck == HI_SLT ||
                ck == HI_SGE || ck == HI_SLTU || ck == HI_SGEU) {
                if (ck == HI_SEQ) { bropt = "beq"; brop = "bne"; }
                else if (ck == HI_SNE) { bropt = "bne"; brop = "beq"; }
                else if (ck == HI_SLT) { bropt = "blt"; brop = "bge"; }
                else if (ck == HI_SGE) { bropt = "bge"; brop = "blt"; }
                else if (ck == HI_SLTU) { bropt = "bltu"; brop = "bgeu"; }
                else { bropt = "bgeu"; brop = "bltu"; }
                ra = hcg_const_is_zero(ca) ? 0 : hcg_src(ca, 1);
                rb = hcg_const_is_zero(cb) ? 0 : hcg_src(cb, 2);
                hcg_condbr_finish(idx, bropt, brop, ra, rb);
                hcg_stat_brc_fuse = hcg_stat_brc_fuse + 1;
                return;
            }
            if (ck == HI_SGT || ck == HI_SGTU) {
                /* a > b => r1 = (b < a); condition true when r1 != 0 */
                ra = hcg_const_is_zero(cb) ? 0 : hcg_src(cb, 1);
                rb = hcg_const_is_zero(ca) ? 0 : hcg_src(ca, 2);
                if (ck == HI_SGT) brop = "slt"; else brop = "sltu";
                cg_rrr(brop, 1, ra, rb);
                hcg_condbr_finish(idx, "bne", "beq", 1, 0);
                hcg_stat_brc_fuse = hcg_stat_brc_fuse + 1;
                return;
            }
            /* HI_SLE / HI_SLEU: a <= b => r1 = (b < a); true when r1 == 0 */
            ra = hcg_const_is_zero(cb) ? 0 : hcg_src(cb, 1);
            rb = hcg_const_is_zero(ca) ? 0 : hcg_src(ca, 2);
            if (ck == HI_SLE) brop = "slt"; else brop = "sltu";
            cg_rrr(brop, 1, ra, rb);
            hcg_condbr_finish(idx, "beq", "bne", 1, 0);
            hcg_stat_brc_fuse = hcg_stat_brc_fuse + 1;
            return;
        }

        /* Unfused conditional branch: materialized 0/1 condition */
        cond = hcg_src(s1, 1);
        hcg_condbr_finish(idx, "bne", "beq", cond, 0);
        return;
    }

    /* Return */
    if (k == HI_RET) {
        /* 64-bit return: put hi word in r2 first */
        if (s2 >= 0) {
            hcg_into(2, s2);
        }
        if (s1 >= 0) {
            if (hcg_const_imm_inst(s1, &off)) {
                if (off == 0) cg_rri("addi", 1, 0, 0);
                else if (hcg_is_i12(off)) cg_rri("addi", 1, 0, off);
                else hcg_li(1, off);
            } else {
                hcg_into(1, s1);
            }
        }
        cg_s("    jal r0, ");
        cg_lref(hcg_epilog);
        cg_c(10);
        return;
    }

    /* Jump-table dispatch (issue #32).  src1 = index, already normalised to
     * [0,span) and bounds-checked by the preceding BRC, so the table lookup
     * always lands on a valid entry.  No phi copies: the lowering routes every
     * JMPTAB edge to a single-predecessor block.  r1/r2 are never allocated to
     * values (the allocatable pool is r3-r28), so they are free scratch. */
    if (k == HI_JMPTAB) {
        int jtid;
        int base;
        int span;
        int t;
        base = hjt_base[idx];
        span = hjt_span[idx];
        if (cg_njt >= CG_MAX_JT || cg_njt_ent + span > CG_MAX_JT_ENT) {
            fdputs("s12cc: too many jump-table entries\n", 2);
            exit(1);
        }
        jtid = cg_label();
        cg_jt_id[cg_njt] = jtid;
        cg_jt_base[cg_njt] = cg_njt_ent;
        cg_jt_span[cg_njt] = span;
        t = 0;
        while (t < span) {
            cg_jt_ent[cg_njt_ent] = hcg_blk_lbl[hjt_target[base + t]]; /* BISECT: no JT fwd */
            cg_njt_ent = cg_njt_ent + 1;
            t = t + 1;
        }
        cg_njt = cg_njt + 1;

        hcg_into(1, s1);            /* r1 = index */
        cg_rri("slli", 1, 1, 2);    /* r1 = index * 4 */
        cg_s("    lui r2, %hi(.LJT");
        cg_n(jtid);
        cg_s(")\n    addi r2, r2, %lo(.LJT");
        cg_n(jtid);
        cg_s(")\n");
        cg_rrr("add", 1, 2, 1);     /* r1 = &table[index] */
        cg_rri("ldw", 1, 1, 0);     /* r1 = table[index] (target address) */
        cg_s("    jalr r0, r1, 0\n");
        return;
    }

    /* Direct call */
    if (k == HI_CALL) {
        int has_callhi;
        int rd2;
        int is_tail;
        int fpk;
        nargs = h_val[idx];
        base = h_cbase[idx];

        fpk = hcg_fp64_kind(h_name[idx]);
        if (fpk >= 0) {
            /* HW FP inline instead of the wrapper call.  Results land in
             * r1/r2 so the shared post-call path below runs as-is,
             * unless the value has a home pair -- hcg_fp64_dst records
             * that so the writeback can be skipped. */
            hcg_fp64_cur = (idx + 1 < bb_end[h_blk[idx]] &&
                            h_kind[idx + 1] == HI_CALLHI) ? idx : -1;
            hcg_fp64_dst = -1;
            hcg_fp64_emit(fpk, base);
            if (hcg_fp64_dst >= 0) return;   /* already in its home pair */
            /* Result is in r4:r5.  If it is going to be spilled anyway,
             * store straight from there rather than moving it through
             * the r1:r2 call-return convention first -- two fewer
             * instructions per spilled operation, and in fp64-heavy
             * code most temporaries do spill. */
            if (fpk <= 3 && hcg_fp64_cur == idx &&
                ra_reg[idx] < 0 && ra_reg[idx + 1] < 0) {
                hcg_spill_from(idx, 4);
                hcg_spill_from(idx + 1, 5);
                return;
            }
        } else {

        /* ABI walk: aligned f64 pairs, back-filled ints, ordered
         * stack spill (matches clang's CC_SLOW32). */
        nstk = hi_abi_assign(&h_carg_tag[base], nargs, hcg_argmap);

        /* Load register args */
        i = 0;
        while (i < nargs) {
            if (hcg_argmap[i] >= 0) hcg_into(hcg_argmap[i], h_carg[base + i]);
            i = i + 1;
        }

        /* Check for tail call BEFORE emitting stack args or jal */
        is_tail = (nstk == 0) && hcg_is_tailcall(idx);

        if (is_tail) {
            /* Tail call: epilogue + jump (no link) */
            hcg_emit_epilogue_inline();
            cg_s("    jal r0, ");
            cg_s(h_name[idx]);
            cg_c(10);
            hcg_stat_tailcall = hcg_stat_tailcall + 1;
            return;
        }

        /* Normal call: push stack args, call with link */
        hcg_push_stack_args(base, nargs);

        cg_s("    jal r31, ");
        cg_s(h_name[idx]);
        cg_c(10);

        hcg_pop_stack_args(nstk * 4);

        }

        /* Check for CALLHI following this CALL.  A spilled CALLHI is
         * stored straight from r2 (hcg_spill_from): the old path went
         * addi r1, r2 then spilled r1 — clobbering the call's lo word
         * before its own spill, so both words spilled == hi. */
        has_callhi = 0;
        if (idx + 1 < bb_end[h_blk[idx]] && h_kind[idx + 1] == HI_CALLHI) {
            has_callhi = 1;
            rd2 = hcg_dst(idx + 1);
            if (ra_reg[idx + 1] >= 0) {
                if (rd2 != 2) cg_rri("addi", rd2, 2, 0);
            } else {
                hcg_spill_from(idx + 1, 2);
            }
        }

        rd = hcg_dst(idx);
        if (rd != 1) {
            cg_rri("addi", rd, 1, 0);
        } else {
            hcg_maybe_spill(idx);
        }
        return;
    }

    /* CALLHI — already handled by preceding CALL */
    if (k == HI_CALLHI) {
        return;
    }

    /* Indirect call */
    if (k == HI_CALLP) {
        int has_callhi2;
        int rd2b;
        nargs = h_val[idx];
        base = h_cbase[idx];

        nstk = hi_abi_assign(&h_carg_tag[base], nargs, hcg_argmap);

        i = 0;
        while (i < nargs) {
            if (hcg_argmap[i] >= 0) hcg_into(hcg_argmap[i], h_carg[base + i]);
            i = i + 1;
        }

        hcg_into(1, s1);
        cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");

        hcg_push_stack_args(base, nargs);

        if (nstk * 4 <= 2047) {
            cg_s("    ldw r2, r29, ");
            cg_n(nstk * 4);
            cg_c(10);
        } else {
            /* Function-pointer slot out of LDW's imm range: form the
             * address in r2 (free scratch before the call). */
            hcg_li(2, nstk * 4);
            cg_rrr("add", 2, 29, 2);
            cg_rri("ldw", 2, 2, 0);
        }
        cg_s("    jalr r31, r2, 0\n");

        hcg_pop_stack_args((nstk + 1) * 4);

        /* Check for CALLHI following this CALLP (same r2-direct spill
         * as the direct-call path — see hcg_spill_from). */
        has_callhi2 = 0;
        if (idx + 1 < bb_end[h_blk[idx]] && h_kind[idx + 1] == HI_CALLHI) {
            has_callhi2 = 1;
            rd2b = hcg_dst(idx + 1);
            if (ra_reg[idx + 1] >= 0) {
                if (rd2b != 2) cg_rri("addi", rd2b, 2, 0);
            } else {
                hcg_spill_from(idx + 1, 2);
            }
        }

        rd = hcg_dst(idx);
        if (rd != 1) {
            cg_rri("addi", rd, 1, 0);
        } else {
            hcg_maybe_spill(idx);
        }
        return;
    }

    /* COPY */
    if (k == HI_COPY) {
        rd = hcg_dst(idx);
        if (hcg_const_imm_inst(s1, &off)) {
            hcg_li(rd, off);
            hcg_maybe_spill(idx);
            return;
        }
        rs1 = hcg_src(s1, 1);
        if (rd != rs1) {
            cg_rri("addi", rd, rs1, 0);
            hcg_stat_copy_emit = hcg_stat_copy_emit + 1;
        }
        hcg_maybe_spill(idx);
        return;
    }
}

/* --- Promote big loop-used constants out of remat ---
 * A big (non-i12) HI_ICONST is rematerialized as lui+addi at every
 * use; when the use sits inside a loop that is two instructions per
 * iteration for a loop-invariant value (bench_arith paid it on the
 * Weyl increment).  Mark such constants h_no_remat: they get a
 * register and one materialization at the def (constants live in
 * the entry block, so the def IS the preheader).  A constant used
 * only outside loops stays remat — no live-range cost. */
static void hcg_mark_loop_consts(void) {
    int i;
    int j;
    int k;
    int b;
    int inloop;
    int a;
    int p;

    i = 0;
    while (i < h_ninst) {
        h_no_remat[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k != HI_NOP) {
            b = h_blk[i];
            inloop = 0;
            if (b >= 0 && b < bb_nblk && licm_in_any_loop[b]) inloop = 1;
            if (k == HI_PHI) {
                /* A phi argument is used at the END of its incoming
                 * predecessor block — the copy runs there. */
                if (h_pbase[i] >= 0) {
                    j = 0;
                    while (j < h_pcnt[i]) {
                        a = h_pval[h_pbase[i] + j];
                        p = h_pblk[h_pbase[i] + j];
                        if (a >= 0 && p >= 0 && p < bb_nblk &&
                            licm_in_any_loop[p] &&
                            h_kind[a] == HI_ICONST && !hcg_is_i12(h_val[a])) {
                            h_no_remat[a] = 1;
                        }
                        j = j + 1;
                    }
                }
            } else if (inloop) {
                a = h_src1[i];
                if (a >= 0 && h_kind[a] == HI_ICONST && !hcg_is_i12(h_val[a]))
                    h_no_remat[a] = 1;
                if (ho_src2_is_ref(k)) {
                    a = h_src2[i];
                    if (a >= 0 && h_kind[a] == HI_ICONST && !hcg_is_i12(h_val[a]))
                        h_no_remat[a] = 1;
                }
                if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
                    j = 0;
                    while (j < h_val[i]) {
                        a = h_carg[h_cbase[i] + j];
                        if (a >= 0 && h_kind[a] == HI_ICONST && !hcg_is_i12(h_val[a]))
                            h_no_remat[a] = 1;
                        j = j + 1;
                    }
                }
            }
        }
        i = i + 1;
    }
}

/* --- Compute the trampoline-forwarding maps for this function ---
 * A block forwards when it is nothing but one HI_BR: no phis, no
 * LICM hoists, no value instructions — and its target has no phis
 * (a phi target's edge copies live in the trampoline, so skipping
 * it would skip them; in practice a copy-carrying trampoline never
 * qualifies, and a phi-ful target keeps its preds intact).  Block 0
 * never forwards: it is entered by fallthrough from the prologue,
 * not by a branch that could be redirected. */
static void hcg_compute_fwd(void) {
    int b;
    int i;
    int k;
    int tgt;
    int ok;
    int hops;
    int nxt;

    b = 0;
    while (b < bb_nblk) {
        hcg_fwd[b] = b;
        b = b + 1;
    }

    b = 1;
    while (b < bb_nblk) {
        if (ssa_phi_head[b] < 0 && licm_head[b] < 0 && split_head[b] < 0) {
            tgt = -1;
            ok = 1;
            i = bb_start[b];
            while (i < bb_end[b]) {
                k = h_kind[i];
                if (k == HI_BR) {
                    if (tgt >= 0) { ok = 0; break; }
                    tgt = h_val[i];
                } else if (k != HI_NOP) {
                    ok = 0;
                    break;
                }
                i = i + 1;
            }
            if (ok && tgt >= 0 && tgt < bb_nblk && tgt != b &&
                ssa_phi_head[tgt] < 0) {
                hcg_fwd[b] = tgt;
            }
        }
        b = b + 1;
    }

    /* Collapse chains (bounded; guards degenerate cycles) */
    b = 0;
    while (b < bb_nblk) {
        tgt = hcg_fwd[b];
        hops = 0;
        while (hcg_fwd[tgt] != tgt && hops < 8) {
            tgt = hcg_fwd[tgt];
            hops = hops + 1;
        }
        hcg_fwd[b] = tgt;
        b = b + 1;
    }

    /* Every block is emitted, forwarded or not: a fully-forwarded
     * trampoline is 4 dead bytes of .text, and keeping it makes
     * implicit fallthrough and branch-range questions moot.  (An
     * earlier revision skipped them; the complexity wasn't worth
     * four bytes.)
     *
     * DIVERGENCE (f77, port upstream candidate): blocks are LAID OUT
     * by greedy fallthrough chains instead of creation order.  The
     * frontend creates a DO loop's exit block before the loop body
     * exists, so creation order put the exit stub INSIDE the loop and
     * the body paid a taken jal over it every iteration.  Each placed
     * block is followed by its preferred successor when still
     * unplaced (BR target; BRC then-arm, the frontends' fallthrough
     * arm), otherwise by the lowest-numbered unplaced block.  Block 0
     * stays first (the prologue falls into it).  Branch-range safety
     * is unchanged: hcg_bnear gates bcond shapes on the recomputed
     * positions, and the assembler now relaxes any bcond that still
     * ends up long. */
    b = 0;
    while (b < bb_nblk) {
        hcg_skip[b] = 0;
        hcg_placed[b] = 0;
        b = b + 1;
    }
    {
        int cur;
        int pick;
        int term;
        int scan;
        hcg_nord = 0;
        cur = 0;
        while (cur >= 0) {
            hcg_emit_ord[hcg_nord] = cur;
            hcg_nord = hcg_nord + 1;
            hcg_placed[cur] = 1;
            /* Preferred successor: where control falls if this block
             * ends in a branch. */
            pick = -1;
            term = -1;
            i = bb_end[cur] - 1;
            while (i >= bb_start[cur]) {
                if (hi_is_terminator(h_kind[i])) { term = i; break; }
                i = i - 1;
            }
            if (term >= 0 && h_kind[term] == HI_BR) {
                scan = hcg_fwd[h_val[term]];
                if (scan < bb_nblk && !hcg_placed[scan]) pick = scan;
            } else if (term >= 0 && h_kind[term] == HI_BRC) {
                scan = hcg_fwd[h_src2[term]];        /* then-arm */
                if (scan < bb_nblk && !hcg_placed[scan]) pick = scan;
                if (pick < 0) {
                    scan = hcg_fwd[h_val[term]];     /* else-arm */
                    if (scan < bb_nblk && !hcg_placed[scan]) pick = scan;
                }
            }
            if (pick < 0) {
                scan = 0;
                while (scan < bb_nblk && hcg_placed[scan]) scan = scan + 1;
                pick = (scan < bb_nblk) ? scan : -1;
            }
            cur = pick;
        }
    }
    b = 0;
    while (b < bb_nblk) {
        hcg_next_emit[hcg_emit_ord[b]] =
            (b + 1 < bb_nblk) ? hcg_emit_ord[b + 1] : bb_nblk;
        b = b + 1;
    }

    /* Estimated byte position of each block, deliberately
     * OVER-estimated (24 bytes per HIR instruction, calls charged
     * per argument, phi copies charged to the phi's own block).
     * Conditional branches reach only +/-4096 bytes; a bcond may be
     * redirected past a trampoline, or emitted in a direct
     * fallthrough shape, ONLY when hcg_bnear says the target is
     * conservatively within range.  The trampoline's jal (+/-1MB)
     * remains the long-range path — that is what these blocks ARE:
     * branch islands.  Over-estimation only costs an optimization,
     * never correctness. */
    nxt = 0;
    b = 0;
    while (b < bb_nblk) {
        int ob;
        ob = hcg_emit_ord[b];            /* positions follow LAYOUT order */
        hcg_blk_pos[ob] = nxt;
        nxt = nxt + 32;
        i = bb_start[ob];
        while (i < bb_end[ob]) {
            k = h_kind[i];
            if (k != HI_NOP) {
                if (k == HI_CALL || k == HI_CALLP) {
                    nxt = nxt + 24 + 8 * h_val[i];
                } else {
                    nxt = nxt + 24;
                }
            }
            i = i + 1;
        }
        i = licm_head[ob];
        while (i >= 0) {
            nxt = nxt + 24;
            i = licm_next[i];
        }
        i = split_head[ob];
        while (i >= 0) {
            nxt = nxt + 24;
            i = licm_next[i];
        }
        i = ssa_phi_head[ob];
        while (i >= 0) {
            nxt = nxt + 24;
            i = ssa_phi_next[i];
        }
        b = b + 1;
    }
}



/* --- Generate code for one basic block --- */

static void hcg_block(int b) {
    int i;
    int term;
    int k;
    hcg_cur_blk = b;
    cg_ldef(hcg_blk_lbl[b]);

    /* Skipped trampoline: every reference was redirected to its
     * final target and nothing falls into it — emit only the label. */
    if (hcg_skip[b]) return;

    /* Split-pass reloads: top of block, before every use. */
    i = split_head[b];
    while (i >= 0) {
        hcg_inst(i);
        i = licm_next[i];
    }

    /* Find the terminator (last non-NOP: BR/BRC/RET/JMPTAB) */
    term = -1;
    i = bb_end[b] - 1;
    while (i >= bb_start[b]) {
        k = h_kind[i];
        if (hi_is_terminator(k)) {
            term = i;
            break;
        }
        if (k != HI_NOP) break;
        i = i - 1;
    }

    /* Emit regular instructions up to (but not including) the terminator */
    i = bb_start[b];
    while (i < bb_end[b]) {
        if (i == term) break;
        hcg_inst(i);
        i = i + 1;
    }

    /* Emit hoisted (LICM) instructions before the terminator */
    i = licm_head[b];
    while (i >= 0) {
        hcg_inst(i);
        i = licm_next[i];
    }

    /* Emit the terminator */
    if (term >= 0) {
        hcg_inst(term);
    }
}

/* --- Save/restore callee-saved register at fp+off --- */

static void hcg_save_reg(int reg, int off) {
    if (off >= -2048 && off <= 2047) {
        cg_s("    stw r30, r");
        cg_n(reg);
        cg_s(", ");
        cg_n(off);
        cg_c(10);
    } else {
        hcg_li(1, off);
        cg_rrr("add", 1, 30, 1);
        cg_s("    stw r1, r");
        cg_n(reg);
        cg_s(", 0\n");
    }
}

static void hcg_restore_reg(int reg, int off) {
    if (off >= -2048 && off <= 2047) {
        cg_s("    ldw r");
        cg_n(reg);
        cg_s(", r30, ");
        cg_n(off);
        cg_c(10);
    } else {
        /* Use the register being restored as its own address scratch —
         * it's dead until the load.  r1 here clobbered the return value
         * in large-frame epilogues (sbasic parse_primary, 4456-byte
         * frame: every expr_t* it returned came back as a stack addr). */
        hcg_li(reg, off);
        cg_rrr("add", reg, 30, reg);
        cg_s("    ldw r");
        cg_n(reg);
        cg_s(", r");
        cg_n(reg);
        cg_s(", 0\n");
    }
}

/* --- Generate one function --- */

static void hcg_func(Node *fn) {
    int fs;
    int i;
    int b;

    /* Lower AST to HIR */
    hl_func(fn);

    /* Run SSA construction */
    hir_ssa_construct();

    /* Run SSA optimizations */
    hir_opt();

    /* Loop-invariant code motion */
    hir_licm();

    /* Promote big loop-used constants out of remat (needs the loop
     * map hir_licm just built; must precede regalloc node creation) */
    hcg_mark_loop_consts();

    /* BURG instruction selection: labels + selects patterns */
    hir_burg();

    /* Compare-and-branch fusion: identify candidates before regalloc
     * so live ranges can be extended for comparison operands */
    hcg_identify_fusions();

    /* Spill slots must not overlap lowering-introduced allocas. */
    hl_temp_stack = hcg_hir_frame_base(fn);

    /* Register allocation: assigns ra_reg[], ra_spill_off[],
     * callee-save info, and updates hl_temp_stack */
    hir_regalloc();

    /* Optional per-function regalloc dump (Issue #31 diagnostic) */
    if (s12cc_dump_intervals) {
        ra_dump_intervals(fn->name);
    }

    /* Note: there is no separate "leaf function" save-elision here.  The
     * allocator-side caller-saved feature (hir_regalloc.h:
     * ra_caller_saved_enabled_count, ra_mark_call_crossing, two-phase
     * gc_select) already gives leaf functions exactly that win: any value
     * whose live range does not cross a call is colored from r3-r10, so
     * ra_used[0..RA_NCALLEE-1] stays 0 and ra_assign_spills naturally
     * leaves ra_ncsave = 0.  An older codegen-side `if (!saw_call)`
     * shortcut here also force-zeroed ra_ncsave, which silently clobbered
     * the caller's r11..r28 whenever a leaf with >8 simultaneously-live
     * values still had to spill into the callee pool. */

    /* Compute frame size (hl_temp_stack includes spills + any callee-saves
     * the allocator could not avoid). */
    fs = hl_temp_stack;
    fs = ((fs + 3) / 4) * 4;
    hcg_locals = fn->locals_size;
    hcg_frame = fs;

    /* Frame-escape scan for the tail-call guard.  A tail call pops this
     * frame BEFORE entering the callee; if any local's address escaped
     * (alloca used outside a direct LOAD/STORE address position, or
     * passed as a call argument), the callee could read/write dead
     * stack that its own frame then reuses.  parse_stmt's
     * `return parse_assign(p, var.text)` handed the callee a pointer
     * into its own popped frame — lexer_next's frame landed exactly on
     * var and shredded the name. */
    hcg_frame_escapes = 0;
    {
        int fe_i;
        int fe_j;
        int fe_k;
        fe_i = 0;
        while (fe_i < h_ninst) {
            fe_k = h_kind[fe_i];
            if (fe_k != HI_NOP) {
                if (h_src1[fe_i] >= 0 && h_kind[h_src1[fe_i]] == HI_ALLOCA &&
                    fe_k != HI_LOAD && fe_k != HI_STORE) {
                    hcg_frame_escapes = 1;
                }
                /* src2 is a block index for BR/BRC and a phi slot for PHI;
                 * only real value refs count (mirrors ssa_find_promo). */
                if (h_src2[fe_i] >= 0 && fe_k != HI_BR && fe_k != HI_BRC &&
                    fe_k != HI_PHI &&
                    h_kind[h_src2[fe_i]] == HI_ALLOCA) {
                    hcg_frame_escapes = 1;
                }
                if (fe_k == HI_CALL || fe_k == HI_CALLP) {
                    fe_j = 0;
                    while (fe_j < h_val[fe_i]) {
                        int fe_a;
                        fe_a = h_carg[h_cbase[fe_i] + fe_j];
                        if (fe_a >= 0 && h_kind[fe_a] == HI_ALLOCA) {
                            hcg_frame_escapes = 1;
                        }
                        fe_j = fe_j + 1;
                    }
                }
            }
            fe_i = fe_i + 1;
        }
    }

    /* Reset the one-shot param entry emission for this function */
    hcg_params_emitted = 0;

    /* Branch-target forwarding through empty trampoline blocks */
    hcg_compute_fwd();

    /* Allocate labels for blocks and epilog */
    b = 0;
    while (b < bb_nblk) {
        hcg_blk_lbl[b] = cg_label();
        b = b + 1;
    }
    hcg_epilog = cg_label();

    /* Function label */
    cg_s(".global ");
    cg_s(fn->name);
    cg_c(10);
    cg_s(fn->name);
    cg_s(":\n");

    /* Varargs register save area — placed before regular prologue so
       it's contiguous with caller's stack arguments */
    hcg_va_save_size = 0;
    if (fn->is_varargs) {
        int nfixed;
        int nsave;
        int j;
        nfixed = fn->nparams;
        nsave = 8 - nfixed;
        if (nsave < 0) nsave = 0;
        if (nsave > 0) {
            hcg_va_save_size = nsave * 4;
            cg_rri("addi", 29, 29, 0 - hcg_va_save_size);
            j = 0;
            while (j < nsave) {
                cg_s("    stw r29, r");
                cg_n(3 + nfixed + j);
                cg_s(", ");
                cg_n(j * 4);
                cg_c(10);
                j = j + 1;
            }
        }
    }

    /* Prologue — handle large frames (>2047 bytes) */
    if (fs <= 2047) {
        cg_rri("addi", 29, 29, 0 - fs);
        cg_s("    stw r29, r31, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    stw r29, r30, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_rri("addi", 30, 29, fs);
    } else {
        cg_s("    stw r29, r31, -4\n");
        cg_s("    stw r29, r30, -8\n");
        cg_rri("addi", 30, 29, 0);
        hcg_li(1, fs);
        cg_rrr("sub", 29, 29, 1);
    }

    /* Save callee-saved registers (after fp is set up) */
    i = 0;
    while (i < ra_ncsave) {
        hcg_save_reg(ra_csave_reg[i], ra_csave_off[i]);
        i = i + 1;
    }

    /* Emit all basic blocks, in layout order */
    b = 0;
    while (b < bb_nblk) {
        hcg_block(hcg_emit_ord[b]);
        b = b + 1;
    }

    /* Epilogue: restore callee-saved registers */
    cg_ldef(hcg_epilog);
    i = 0;
    while (i < ra_ncsave) {
        hcg_restore_reg(ra_csave_reg[i], ra_csave_off[i]);
        i = i + 1;
    }

    if (fs <= 2047) {
        cg_s("    ldw r31, r29, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    ldw r30, r29, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_rri("addi", 29, 29, fs);
    } else {
        cg_rri("addi", 29, 30, 0);
        cg_s("    ldw r31, r29, -4\n");
        cg_s("    ldw r30, r29, -8\n");
    }
    if (hcg_va_save_size > 0) {
        cg_rri("addi", 29, 29, hcg_va_save_size);
    }
    cg_s("    jalr r0, r31, 0\n\n");
}

/* --- Emit .data and .bss sections --- */

static void gen_data(void) {
    int i;
    int j;
    int len;
    int elem_sz;
    char *sp;

    cg_s(".data\n");

    /* Jump tables (issue #32).  Emitted first, 4-byte aligned, so the
     * `ldw` in the dispatch reads aligned words.  Entries are absolute
     * .word relocations to code-block labels (resolved by the linker). */
    if (cg_njt > 0) {
        cg_s(".align 2\n");
        i = 0;
        while (i < cg_njt) {
            cg_s(".LJT");
            cg_n(cg_jt_id[i]);
            cg_s(":\n");
            j = 0;
            while (j < cg_jt_span[i]) {
                cg_s("    .word .L");
                cg_n(cg_jt_ent[cg_jt_base[i] + j]);
                cg_c(10);
                j = j + 1;
            }
            i = i + 1;
        }
    }

    /* String literals */
    i = 0;
    while (i < lex_str_count) {
        cg_s(".LS");
        cg_n(i);
        cg_s(":\n    .byte ");
        sp = lex_strpool + lex_str_off[i];
        len = lex_str_len[i];
        j = 0;
        while (j < len) {
            if (j > 0) {
                /* Chunk long literals: the host assembler caps a line
                 * at 512 tokens (doom's f_finale victory text is one
                 * ~1800-byte string). */
                if ((j & 15) == 0) cg_s("\n    .byte ");
                else cg_s(", ");
            }
            cg_n(sp[j] & 255);
            j = j + 1;
        }
        if (j > 0) {
            if ((j & 15) == 0) cg_s("\n    .byte ");
            else cg_s(", ");
        }
        cg_s("0\n");
        i = i + 1;
    }

    /* Global variables (initialized -> .data) */
    i = 0;
    while (i < ps_nglobals) {
        if (ps_ginit_start[i] >= 0) {
            /* Array/struct initializer list.
             * ps_ginit_count is the byte count; the pool stores one byte
             * per slot.  Emit .byte per slot — relocations (handled inside
             * the loop) emit a 4-byte .word that advances by reloc_size. */
            int reli;
            int relend;
            int off;
            /* DIVERGENCE (f77): word-align every emitted global.  The
             * string literals just above are byte streams of arbitrary
             * length, so a global emitted after an odd-length FORMAT
             * text would land misaligned -- f77's DATA images carry
             * word and doubleword values.  Candidate to port upstream:
             * stage08's gen_data has the same latent hazard. */
            cg_s(".align 2\n");
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n");
            reli = ps_girel_start[i];
            relend = reli + ps_girel_count[i];
            off = 0;
            while (off < ps_ginit_count[i]) {
                if (reli < relend && ps_girel_off[reli] == off) {
                    if (ps_girel_kind[reli] == GIRELOC_STRING) {
                        cg_s("    .word .LS");
                        cg_n(ps_girel_idx[reli]);
                    } else {
                        cg_s("    .word ");
                        /* Named symbol relocs carry the target in
                         * ps_girel_name; ps_girel_idx is 0 for them
                         * (every named reloc used to emit gname[0] —
                         * doom's doom_defaults pointed at stdin). */
                        if (ps_girel_name[reli] != 0)
                            cg_s(ps_girel_name[reli]);
                        else
                            cg_s(ps_gname[ps_girel_idx[reli]]);
                        if (ps_girel_add[reli] != 0) {
                            cg_c(43); /* '+' */
                            cg_n(ps_girel_add[reli]);
                        }
                    }
                    cg_c(10);
                    off = off + ps_girel_size[reli];
                    reli = reli + 1;
                } else {
                    cg_s("    .byte ");
                    cg_n(ps_ginit_pool[ps_ginit_start[i] + off]);
                    cg_c(10);
                    off = off + 1;
                }
            }
            /* Remaining bytes zero-filled */
            len = ps_gsize[i] - ps_ginit_count[i];
            if (len > 0) {
                cg_s("    .space ");
                cg_n(len);
                cg_c(10);
            }
        } else if (ps_gsize[i] == 0 && ps_gstr[i] >= 0) {
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .word .LS");
            cg_n(ps_gstr[i]);
            cg_c(10);
        } else if (ps_gsize[i] == 0 && ty_is_llong(ps_gtype[i]) && (ps_ginit[i] != 0 || ps_ginit_hi[i] != 0)) {
            /* 64-bit initialized global */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .word ");
            cg_n(ps_ginit[i]);
            cg_s("\n    .word ");
            cg_n(ps_ginit_hi[i]);
            cg_c(10);
        } else if (ps_gsize[i] == 0 && ps_ginit[i] != 0) {
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .word ");
            cg_n(ps_ginit[i]);
            cg_c(10);
        }
        i = i + 1;
    }

    /* BSS section */
    cg_s(".bss\n");
    i = 0;
    while (i < ps_nglobals) {
        if (ps_gextern[i]) {
            /* extern declaration: storage lives in another TU; emitting
             * .space here made every stdio.h includer DEFINE stdout et
             * al. — the stage08 linker merged them common-style, the
             * host linker correctly refused the multiple definition. */
        } else if (ps_ginit_start[i] >= 0) {
            /* Already emitted in .data */
        } else if (ps_gsize[i] > 0) {
            /* DIVERGENCE (f77): word-align, as in the .data walk. */
            cg_s(".align 2\n");
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space ");
            cg_n(ps_gsize[i]);
            cg_c(10);
        } else if ((ty_is_llong(ps_gtype[i]) || ty_is_double(ps_gtype[i])) &&
                   ps_ginit[i] == 0 && ps_ginit_hi[i] == 0 && ps_gstr[i] < 0) {
            /* 64-bit uninitialized global (llong or double: an 8-byte
             * store to a 4-byte slot stomps the next global) */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space 8\n");
        } else if (ps_ginit[i] == 0 && ps_gstr[i] < 0) {
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space 4\n");
        }
        i = i + 1;
    }
}

/* --- Generate entire program --- */

static void gen_program(Node *prog) {
    Node *fn;

    cg_njt = 0;
    cg_njt_ent = 0;
    cg_s(".text\n\n");
    fn = prog->body;
    while (fn) {
        hcg_func(fn);
        fn = fn->next;
    }

    gen_data();
}
