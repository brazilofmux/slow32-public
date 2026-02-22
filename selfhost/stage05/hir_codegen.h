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
static int hcg_epilog;     /* epilog label */
static int hcg_va_save_size; /* varargs register save area size */

/* Block labels */
static int hcg_blk_lbl[HIR_MAX_BLOCK];

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

/* --- Store r1 to spill slot if instruction is spilled --- */
static void hcg_maybe_spill(int idx) {
    int off;
    if (ra_reg[idx] >= 0) return;
    if (hi_is_remat(h_kind[idx])) return;
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

    /* Fast path: all destinations and non-constant sources are in registers.
     * Emit cycle-safe parallel copies without runtime stack traffic. */
    fast_ok = 1;
    j = 0;
    while (j < n) {
        phi = hcg_phi_tmp[j];
        if (ra_reg[phi] < 0) { fast_ok = 0; break; }
        hcg_phi_dst_reg[j] = ra_reg[phi];
        if (hcg_phi_dst_reg[j] == 2) { fast_ok = 0; break; } /* keep r2 as temp */
        v = ssa_phi_find_arg(phi, from_blk);
        if (hcg_const_imm_inst(v, &c)) {
            hcg_phi_is_const[j] = 1;
            hcg_phi_const_val[j] = c;
            hcg_phi_src_reg[j] = -1;
        } else {
            if (v < 0 || ra_reg[v] < 0) { fast_ok = 0; break; }
            if (ra_reg[v] == 2) { fast_ok = 0; break; }
            hcg_phi_is_const[j] = 0;
            hcg_phi_src_reg[j] = ra_reg[v];
        }
        hcg_phi_active[j] = 1;
        j = j + 1;
    }

    if (fast_ok) {
        rem = n;
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
                    if (hcg_phi_is_const[j]) {
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
    if (hi_is_remat(k)) return;
    if (k == HI_PHI) return;

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
        rd = hcg_dst(idx);
        cg_rri("addi", rd, 3 + h_val[idx], 0);
        hcg_maybe_spill(idx);
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
        hcg_phi_copies(h_blk[idx], h_val[idx]);
        cg_s("    jal r0, ");
        cg_lref(hcg_blk_lbl[h_val[idx]]);
        cg_c(10);
        return;
    }

    /* Conditional branch */
    if (k == HI_BRC) {
        if (hcg_const_imm_inst(s1, &off)) {
            if (off != 0) {
                hcg_phi_copies(h_blk[idx], s2);
                cg_s("    jal r0, ");
                cg_lref(hcg_blk_lbl[s2]);
                cg_c(10);
            } else {
                hcg_phi_copies(h_blk[idx], h_val[idx]);
                cg_s("    jal r0, ");
                cg_lref(hcg_blk_lbl[h_val[idx]]);
                cg_c(10);
            }
            return;
        }
        cond = hcg_src(s1, 1);
        skip = cg_label();
        cg_s("    beq r");
        cg_n(cond);
        cg_s(", r0, ");
        cg_lref(skip);
        cg_c(10);
        hcg_phi_copies(h_blk[idx], s2);
        cg_s("    jal r0, ");
        cg_lref(hcg_blk_lbl[s2]);
        cg_c(10);
        cg_ldef(skip);
        hcg_phi_copies(h_blk[idx], h_val[idx]);
        cg_s("    jal r0, ");
        cg_lref(hcg_blk_lbl[h_val[idx]]);
        cg_c(10);
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

    /* Direct call */
    if (k == HI_CALL) {
        int has_callhi;
        int rd2;
        int is_tail;
        nargs = h_val[idx];
        base = h_cbase[idx];

        regc = nargs;
        if (regc > 8) regc = 8;

        /* Load register args (r3..r10) */
        i = 0;
        while (i < regc) {
            hcg_into(3 + i, h_carg[base + i]);
            i = i + 1;
        }

        /* Check for tail call BEFORE emitting stack args or jal */
        is_tail = hcg_is_tailcall(idx);

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
        i = nargs - 1;
        while (i >= regc) {
            if (hcg_const_is_zero(h_carg[base + i])) {
                cg_s("    addi r29, r29, -4\n    stw r29, r0, 0\n");
            } else {
                hcg_into(1, h_carg[base + i]);
                cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
            }
            i = i - 1;
        }

        cg_s("    jal r31, ");
        cg_s(h_name[idx]);
        cg_c(10);

        if (nargs > regc) {
            cg_rri("addi", 29, 29, (nargs - regc) * 4);
        }

        /* Check for CALLHI following this CALL */
        has_callhi = 0;
        if (idx + 1 < bb_end[h_blk[idx]] && h_kind[idx + 1] == HI_CALLHI) {
            has_callhi = 1;
            rd2 = hcg_dst(idx + 1);
            /* Capture r2 (hi word) before any register shuffling */
            if (rd2 != 2) {
                cg_rri("addi", rd2, 2, 0);
            }
            hcg_maybe_spill(idx + 1);
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

        regc = nargs;
        if (regc > 8) regc = 8;

        i = 0;
        while (i < regc) {
            hcg_into(3 + i, h_carg[base + i]);
            i = i + 1;
        }

        hcg_into(1, s1);
        cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");

        i = nargs - 1;
        while (i >= regc) {
            if (hcg_const_is_zero(h_carg[base + i])) {
                cg_s("    addi r29, r29, -4\n    stw r29, r0, 0\n");
            } else {
                hcg_into(1, h_carg[base + i]);
                cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
            }
            i = i - 1;
        }

        cg_s("    ldw r2, r29, ");
        cg_n((nargs - regc) * 4);
        cg_c(10);
        cg_s("    jalr r31, r2, 0\n");

        cg_rri("addi", 29, 29, (nargs - regc + 1) * 4);

        /* Check for CALLHI following this CALLP */
        has_callhi2 = 0;
        if (idx + 1 < bb_end[h_blk[idx]] && h_kind[idx + 1] == HI_CALLHI) {
            has_callhi2 = 1;
            rd2b = hcg_dst(idx + 1);
            if (rd2b != 2) {
                cg_rri("addi", rd2b, 2, 0);
            }
            hcg_maybe_spill(idx + 1);
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

/* --- Generate code for one basic block --- */

static void hcg_block(int b) {
    int i;
    int term;
    int k;
    cg_ldef(hcg_blk_lbl[b]);

    /* Find the terminator (last non-NOP: BR/BRC/RET) */
    term = -1;
    i = bb_end[b] - 1;
    while (i >= bb_start[b]) {
        k = h_kind[i];
        if (k == HI_BR || k == HI_BRC || k == HI_RET) {
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
        hcg_li(1, off);
        cg_rrr("add", 1, 30, 1);
        cg_s("    ldw r");
        cg_n(reg);
        cg_s(", r1, 0\n");
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

    /* BURG instruction selection: labels + selects patterns */
    hir_burg();

    /* Register allocation: assigns ra_reg[], ra_spill_off[],
     * callee-save info, and updates hl_temp_stack */
    hir_regalloc();

    /* Compute frame size (hl_temp_stack now includes spills + callee-saves) */
    fs = hl_temp_stack;
    fs = ((fs + 3) / 4) * 4;
    hcg_locals = fn->locals_size;
    hcg_frame = fs;

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

    /* Emit all basic blocks */
    b = 0;
    while (b < bb_nblk) {
        hcg_block(b);
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
            if (j > 0) cg_s(", ");
            cg_n(sp[j] & 255);
            j = j + 1;
        }
        if (j > 0) cg_s(", ");
        cg_s("0\n");
        i = i + 1;
    }

    /* Global variables (initialized -> .data) */
    i = 0;
    while (i < ps_nglobals) {
        if (ps_ginit_start[i] >= 0) {
            /* Array/struct initializer list */
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n");
            /* Determine element size: char arrays use .byte, else .word */
            elem_sz = 4;
            if (ty_is_ptr(ps_gtype[i]) &&
                (ty_deref(ps_gtype[i]) & TY_BASE_MASK) == TY_CHAR) {
                elem_sz = 1;
            }
            j = 0;
            while (j < ps_ginit_count[i]) {
                if (elem_sz == 1) {
                    cg_s("    .byte ");
                } else {
                    cg_s("    .word ");
                }
                cg_n(ps_ginit_pool[ps_ginit_start[i] + j]);
                cg_c(10);
                j = j + 1;
            }
            /* Remaining bytes zero-filled */
            len = ps_gsize[i] - (ps_ginit_count[i] * elem_sz);
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
        if (ps_ginit_start[i] >= 0) {
            /* Already emitted in .data */
        } else if (ps_gsize[i] > 0) {
            if (!ps_glocal[i]) { cg_s(".global "); cg_s(ps_gname[i]); cg_c(10); }
            cg_s(ps_gname[i]);
            cg_s(":\n    .space ");
            cg_n(ps_gsize[i]);
            cg_c(10);
        } else if (ty_is_llong(ps_gtype[i]) && ps_ginit[i] == 0 && ps_ginit_hi[i] == 0 && ps_gstr[i] < 0) {
            /* 64-bit uninitialized global */
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

    cg_s(".text\n\n");
    fn = prog->body;
    while (fn) {
        hcg_func(fn);
        fn = fn->next;
    }

    gen_data();
}
