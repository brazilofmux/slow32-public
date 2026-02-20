/* hir_codegen.h -- HIR to SLOW-32 assembly for s12cc
 *
 * Phase 8: Linear scan register allocation (r11-r28, callee-saved).
 * Uses ra_reg[] from hir_regalloc.h for physical register assignments.
 * Rematerializable values (ICONST, ALLOCA, GADDR, SADDR, FADDR)
 * are regenerated on demand. Spilled values use r1/r2 as scratch.
 */

/* --- Output buffer --- */
#define CG_MAX_OUT 2097152

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

/* Block labels */
static int hcg_blk_lbl[HIR_MAX_BLOCK];

/* --- Load immediate into register --- */
static void hcg_li(int reg, int v) {
    int hi;
    int lo;
    if (v >= -2048 && v <= 2047) {
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
        cg_rri("addi", reg, reg, lo);
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
static void hcg_load_mem(int dreg, int areg, int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r");
        else                  cg_s("    ldb r");
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

static void hcg_phi_copies(int from_blk, int to_blk) {
    int i;
    int n;
    int j;
    int v;
    int off;
    int src;
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

    /* Push all argument values onto runtime stack */
    j = 0;
    while (j < n) {
        v = ssa_phi_find_arg(hcg_phi_tmp[j], from_blk);
        hcg_into(1, v);
        cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
        j = j + 1;
    }

    /* Pop in reverse order into PHI dest registers or spill slots */
    j = n - 1;
    while (j >= 0) {
        phi = hcg_phi_tmp[j];
        if (ra_reg[phi] >= 0) {
            /* Pop directly into physical register */
            dreg = ra_reg[phi];
            cg_s("    ldw r");
            cg_n(dreg);
            cg_s(", r29, ");
            cg_n((n - 1 - j) * 4);
            cg_c(10);
        } else {
            /* Pop into r1, then store to spill slot */
            cg_s("    ldw r1, r29, ");
            cg_n((n - 1 - j) * 4);
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

    /* Clean up stack */
    if (n > 0) {
        cg_rri("addi", 29, 29, n * 4);
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

/* --- Generate code for one HIR instruction --- */

static void hcg_inst(int idx) {
    int k;
    int ty;
    int s1;
    int s2;
    int nargs;
    int base;
    int i;
    int skip;
    int off;
    int rs1;
    int rs2;
    int rd;
    int src;
    int vreg;
    int cond;

    k = h_kind[idx];
    ty = h_ty[idx];
    s1 = h_src1[idx];
    s2 = h_src2[idx];

    if (k == HI_NOP || k == HI_ICONST || k == HI_ALLOCA ||
        k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) {
        return;
    }

    if (k == HI_PARAM) {
        rd = hcg_dst(idx);
        cg_rri("addi", rd, 3 + h_val[idx], 0);
        hcg_maybe_spill(idx);
        return;
    }

    /* Binary arithmetic/logic/comparison */
    if (k >= HI_ADD && k <= HI_SGEU) {
        rs1 = hcg_src(s1, 1);
        rs2 = hcg_src(s2, 2);
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

    /* Unary: bitwise not */
    if (k == HI_BNOT) {
        rs1 = hcg_src(s1, 1);
        rd = hcg_dst(idx);
        cg_rri("addi", 2, 0, -1);
        cg_rrr("xor", rd, rs1, 2);
        hcg_maybe_spill(idx);
        return;
    }

    /* Load */
    if (k == HI_LOAD) {
        rd = hcg_dst(idx);
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1];
            if (off >= -2048 && off <= 2047) {
                hcg_load_off(rd, 30, off, ty);
            } else {
                hcg_li(rd, off);
                cg_rrr("add", rd, 30, rd);
                hcg_load_mem(rd, rd, ty);
            }
        } else {
            rs1 = hcg_src(s1, 1);
            /* If rd == rs1 (both r1 when spilled), load overwrites address
             * only after reading it — this is fine for ldw rN, rN, 0. */
            hcg_load_mem(rd, rs1, ty);
        }
        hcg_maybe_spill(idx);
        return;
    }

    /* Store */
    if (k == HI_STORE) {
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1];
            vreg = hcg_src(s2, 2);
            if (off >= -2048 && off <= 2047) {
                hcg_store_off(30, vreg, off, ty);
            } else {
                hcg_li(1, off);
                cg_rrr("add", 1, 30, 1);
                hcg_store_mem(1, vreg, ty);
            }
        } else {
            rs1 = hcg_src(s1, 1);
            vreg = hcg_src(s2, 2);
            hcg_store_mem(rs1, vreg, ty);
        }
        return;
    }

    /* ADDI (src1 + immediate) */
    if (k == HI_ADDI) {
        rd = hcg_dst(idx);
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1] + h_val[idx];
            if (off >= -2048 && off <= 2047) {
                cg_rri("addi", rd, 30, off);
            } else {
                hcg_li(rd, off);
                cg_rrr("add", rd, 30, rd);
            }
        } else {
            off = h_val[idx];
            rs1 = hcg_src(s1, 1);
            if (off >= -2048 && off <= 2047) {
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
        if (s1 >= 0) {
            hcg_into(1, s1);
        }
        cg_s("    jal r0, ");
        cg_lref(hcg_epilog);
        cg_c(10);
        return;
    }

    /* Direct call */
    if (k == HI_CALL) {
        nargs = h_val[idx];
        base = h_cbase[idx];
        /* Push all args to stack */
        i = 0;
        while (i < nargs) {
            hcg_into(1, h_carg[base + i]);
            cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
            i = i + 1;
        }
        /* Pop into arg registers */
        i = 0;
        while (i < nargs) {
            cg_s("    ldw r");
            cg_n(3 + i);
            cg_s(", r29, ");
            cg_n((nargs - 1 - i) * 4);
            cg_c(10);
            i = i + 1;
        }
        if (nargs > 0) {
            cg_rri("addi", 29, 29, nargs * 4);
        }
        cg_s("    jal r31, ");
        cg_s(h_name[idx]);
        cg_c(10);
        /* Move result from r1 to allocated register or spill */
        rd = hcg_dst(idx);
        if (rd != 1) {
            cg_rri("addi", rd, 1, 0);
        } else {
            hcg_maybe_spill(idx);
        }
        return;
    }

    /* Indirect call */
    if (k == HI_CALLP) {
        nargs = h_val[idx];
        base = h_cbase[idx];
        /* Push callee address first */
        hcg_into(1, s1);
        cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
        /* Push all args */
        i = 0;
        while (i < nargs) {
            hcg_into(1, h_carg[base + i]);
            cg_s("    addi r29, r29, -4\n    stw r29, r1, 0\n");
            i = i + 1;
        }
        /* Pop args into arg registers */
        i = 0;
        while (i < nargs) {
            cg_s("    ldw r");
            cg_n(3 + i);
            cg_s(", r29, ");
            cg_n((nargs - 1 - i) * 4);
            cg_c(10);
            i = i + 1;
        }
        if (nargs > 0) {
            cg_rri("addi", 29, 29, nargs * 4);
        }
        /* Pop callee into r2 */
        cg_s("    ldw r2, r29, 0\n    addi r29, r29, 4\n");
        cg_s("    jalr r31, r2, 0\n");
        /* Move result from r1 to allocated register or spill */
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
        rs1 = hcg_src(s1, 1);
        if (rd != rs1) {
            cg_rri("addi", rd, rs1, 0);
        }
        hcg_maybe_spill(idx);
        return;
    }

    /* PHI — result is written by hcg_phi_copies, no code here */
    if (k == HI_PHI) {
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
    /* hir_licm(); */

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
    cg_s("    jalr r0, r31, 0\n\n");
}

/* --- Emit .data and .bss sections --- */

static void gen_data(void) {
    int i;
    int j;
    int len;
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
        if (ps_gsize[i] == 0 && ps_gstr[i] >= 0) {
            cg_s(".global ");
            cg_s(ps_gname[i]);
            cg_c(10);
            cg_s(ps_gname[i]);
            cg_s(":\n    .word .LS");
            cg_n(ps_gstr[i]);
            cg_c(10);
        } else if (ps_gsize[i] == 0 && ps_ginit[i] != 0) {
            cg_s(".global ");
            cg_s(ps_gname[i]);
            cg_c(10);
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
        if (ps_gsize[i] > 0) {
            cg_s(".global ");
            cg_s(ps_gname[i]);
            cg_c(10);
            cg_s(ps_gname[i]);
            cg_s(":\n    .space ");
            cg_n(ps_gsize[i]);
            cg_c(10);
        } else if (ps_ginit[i] == 0 && ps_gstr[i] < 0) {
            cg_s(".global ");
            cg_s(ps_gname[i]);
            cg_c(10);
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
