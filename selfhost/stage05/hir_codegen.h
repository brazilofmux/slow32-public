/* hir_codegen.h -- HIR to SLOW-32 assembly for s12cc
 *
 * Phase A: spill-everything strategy.
 * Each value-producing HIR instruction gets a temp stack slot.
 * Rematerializable values (ICONST, ALLOCA, GADDR, SADDR, FADDR)
 * are regenerated on demand instead of loading from spill slots.
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

/* --- Per-function codegen state --- */

static int hcg_locals;     /* fn->locals_size (original) */
static int hcg_frame;      /* total frame size (locals + temps) */
static int hcg_epilog;     /* epilog label */

/* Block labels */
static int hcg_blk_lbl[HIR_MAX_BLOCK];

/* Spill slot for each HIR instruction: offset from fp (negative).
 * 0 means not spilled (rematerializable). */
static int hcg_spill[HIR_MAX_INST];

/* --- Load immediate into register --- */
static void hcg_li(int reg, int v) {
    int hi;
    int lo;
    if (v >= -2048 && v <= 2047) {
        cg_s("    addi r");
        cg_n(reg);
        cg_s(", r0, ");
        cg_n(v);
        cg_c(10);
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
        cg_s("    addi r");
        cg_n(reg);
        cg_s(", r");
        cg_n(reg);
        cg_s(", ");
        cg_n(lo);
        cg_c(10);
    }
}

/* Load address of symbol into register */
static void hcg_la(int reg, char *sym) {
    cg_s("    lui r");
    cg_n(reg);
    cg_s(", %hi(");
    cg_s(sym);
    cg_s(")\n    addi r");
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
        /* -1 means no value (void), load 0 */
        cg_s("    addi r");
        cg_n(reg);
        cg_s(", r0, 0\n");
        return;
    }
    k = h_kind[inst];

    if (k == HI_ICONST) {
        hcg_li(reg, h_val[inst]);
        return;
    }
    if (k == HI_ALLOCA) {
        off = h_val[inst];
        if (off >= -2048 && off <= 2047) {
            cg_s("    addi r");
            cg_n(reg);
            cg_s(", r30, ");
            cg_n(off);
            cg_c(10);
        } else {
            hcg_li(reg, off);
            cg_s("    add r");
            cg_n(reg);
            cg_s(", r30, r");
            cg_n(reg);
            cg_c(10);
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

    /* Load from spill slot */
    if (hcg_spill[inst] != 0) {
        off = hcg_spill[inst];
        if (off >= -2048 && off <= 2047) {
            cg_s("    ldw r");
            cg_n(reg);
            cg_s(", r30, ");
            cg_n(off);
            cg_c(10);
        } else {
            hcg_li(reg, off);
            cg_s("    add r");
            cg_n(reg);
            cg_s(", r30, r");
            cg_n(reg);
            cg_c(10);
            cg_s("    ldw r");
            cg_n(reg);
            cg_s(", r");
            cg_n(reg);
            cg_s(", 0\n");
        }
        return;
    }

    /* Fallback: value not available */
    cg_s("    addi r");
    cg_n(reg);
    cg_s(", r0, 0\n");
}

/* Spill r1 to instruction's temp slot */
static void hcg_spill_res(int inst) {
    int off;
    if (hi_is_remat(h_kind[inst])) return;
    off = hcg_spill[inst];
    if (off >= -2048 && off <= 2047) {
        cg_s("    stw r30, r1, ");
        cg_n(off);
        cg_c(10);
    } else {
        /* Use r2 as scratch to compute address */
        hcg_li(2, off);
        cg_s("    add r2, r30, r2\n");
        cg_s("    stw r2, r1, 0\n");
    }
}

/* Load with appropriate width for type */
static void hcg_load(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED) cg_s("    ldbu r1, r1, 0\n");
        else                  cg_s("    ldb r1, r1, 0\n");
    } else {
        cg_s("    ldw r1, r1, 0\n");
    }
}

/* Store r2 to [r1] with appropriate width */
static void hcg_store(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        cg_s("    stb r1, r2, 0\n");
    } else {
        cg_s("    stw r1, r2, 0\n");
    }
}

/* --- Emit phi copies for a branch from_blk → to_blk --- */

static int hcg_phi_tmp[SSA_MAX_PROMO];

static void hcg_phi_copies(int from_blk, int to_blk) {
    int i;
    int n;
    int j;
    int v;
    int off;

    /* Collect PHIs in to_blk (use linked list for O(1) per block) */
    n = 0;
    i = ssa_phi_head[to_blk];
    while (i >= 0) {
        hcg_phi_tmp[n] = i;
        n = n + 1;
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

    /* Pop in reverse order into PHI spill slots */
    j = n - 1;
    while (j >= 0) {
        cg_s("    ldw r1, r29, ");
        cg_n((n - 1 - j) * 4);
        cg_c(10);
        off = hcg_spill[hcg_phi_tmp[j]];
        if (off >= -2048 && off <= 2047) {
            cg_s("    stw r30, r1, ");
            cg_n(off);
            cg_c(10);
        } else {
            hcg_li(2, off);
            cg_s("    add r2, r30, r2\n");
            cg_s("    stw r2, r1, 0\n");
        }
        j = j - 1;
    }

    /* Clean up stack */
    if (n > 0) {
        cg_s("    addi r29, r29, ");
        cg_n(n * 4);
        cg_c(10);
    }
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

    k = h_kind[idx];
    ty = h_ty[idx];
    s1 = h_src1[idx];
    s2 = h_src2[idx];

    if (k == HI_NOP || k == HI_ICONST || k == HI_ALLOCA ||
        k == HI_GADDR || k == HI_SADDR || k == HI_FADDR) {
        /* Rematerializable: no code emitted at definition site */
        return;
    }

    if (k == HI_PARAM) {
        /* Not used in Phase A; params are copied in prologue */
        return;
    }

    /* Binary arithmetic/logic/comparison */
    if (k >= HI_ADD && k <= HI_SGEU) {
        hcg_into(2, s1);
        hcg_into(1, s2);
        if (k == HI_ADD)  cg_s("    add r1, r2, r1\n");
        if (k == HI_SUB)  cg_s("    sub r1, r2, r1\n");
        if (k == HI_MUL)  cg_s("    mul r1, r2, r1\n");
        if (k == HI_DIV)  cg_s("    div r1, r2, r1\n");
        if (k == HI_REM)  cg_s("    rem r1, r2, r1\n");
        if (k == HI_AND)  cg_s("    and r1, r2, r1\n");
        if (k == HI_OR)   cg_s("    or r1, r2, r1\n");
        if (k == HI_XOR)  cg_s("    xor r1, r2, r1\n");
        if (k == HI_SLL)  cg_s("    sll r1, r2, r1\n");
        if (k == HI_SRA)  cg_s("    sra r1, r2, r1\n");
        if (k == HI_SRL)  cg_s("    srl r1, r2, r1\n");
        if (k == HI_SEQ)  cg_s("    seq r1, r2, r1\n");
        if (k == HI_SNE)  cg_s("    sne r1, r2, r1\n");
        if (k == HI_SLT)  cg_s("    slt r1, r2, r1\n");
        if (k == HI_SGT)  cg_s("    sgt r1, r2, r1\n");
        if (k == HI_SLE)  cg_s("    sle r1, r2, r1\n");
        if (k == HI_SGE)  cg_s("    sge r1, r2, r1\n");
        if (k == HI_SLTU) cg_s("    sltu r1, r2, r1\n");
        if (k == HI_SGTU) cg_s("    sgtu r1, r2, r1\n");
        if (k == HI_SLEU) cg_s("    sleu r1, r2, r1\n");
        if (k == HI_SGEU) cg_s("    sgeu r1, r2, r1\n");
        hcg_spill_res(idx);
        return;
    }

    /* Unary ops */
    if (k == HI_NEG) {
        hcg_into(1, s1);
        cg_s("    sub r1, r0, r1\n");
        hcg_spill_res(idx);
        return;
    }
    if (k == HI_NOT) {
        hcg_into(1, s1);
        cg_s("    seq r1, r1, r0\n");
        hcg_spill_res(idx);
        return;
    }
    if (k == HI_BNOT) {
        hcg_into(1, s1);
        cg_s("    addi r2, r0, -1\n");
        cg_s("    xor r1, r1, r2\n");
        hcg_spill_res(idx);
        return;
    }

    /* Load */
    if (k == HI_LOAD) {
        /* Optimization: if src is ALLOCA, load directly from fp+offset */
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1];
            if (off >= -2048 && off <= 2047) {
                if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                    if (ty & TY_UNSIGNED) cg_s("    ldbu r1, r30, ");
                    else                  cg_s("    ldb r1, r30, ");
                } else {
                    cg_s("    ldw r1, r30, ");
                }
                cg_n(off);
                cg_c(10);
            } else {
                hcg_li(1, off);
                cg_s("    add r1, r30, r1\n");
                hcg_load(ty);
            }
        } else {
            hcg_into(1, s1);
            hcg_load(ty);
        }
        hcg_spill_res(idx);
        return;
    }

    /* Store */
    if (k == HI_STORE) {
        /* Optimization: if dst is ALLOCA, store directly to fp+offset */
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1];
            hcg_into(2, s2);
            if (off >= -2048 && off <= 2047) {
                if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
                    cg_s("    stb r30, r2, ");
                } else {
                    cg_s("    stw r30, r2, ");
                }
                cg_n(off);
                cg_c(10);
            } else {
                hcg_li(1, off);
                cg_s("    add r1, r30, r1\n");
                hcg_store(ty);
            }
        } else {
            hcg_into(1, s1);
            hcg_into(2, s2);
            hcg_store(ty);
        }
        return;
    }

    /* ADDI (src1 + immediate) */
    if (k == HI_ADDI) {
        /* Optimization: if src is ALLOCA, fold offsets */
        if (s1 >= 0 && h_kind[s1] == HI_ALLOCA) {
            off = h_val[s1] + h_val[idx];
            if (off >= -2048 && off <= 2047) {
                cg_s("    addi r1, r30, ");
                cg_n(off);
                cg_c(10);
            } else {
                hcg_li(1, off);
                cg_s("    add r1, r30, r1\n");
            }
        } else {
            off = h_val[idx];
            if (off >= -2048 && off <= 2047) {
                hcg_into(1, s1);
                cg_s("    addi r1, r1, ");
                cg_n(off);
                cg_c(10);
            } else {
                hcg_into(1, s1);
                hcg_li(2, off);
                cg_s("    add r1, r1, r2\n");
            }
        }
        hcg_spill_res(idx);
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
        /* if src1 != 0 goto src2(then_blk) else goto val(else_blk) */
        hcg_into(1, s1);
        skip = cg_label();
        cg_s("    beq r1, r0, ");
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
            cg_s("    addi r29, r29, ");
            cg_n(nargs * 4);
            cg_c(10);
        }
        cg_s("    jal r31, ");
        cg_s(h_name[idx]);
        cg_c(10);
        hcg_spill_res(idx);
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
            cg_s("    addi r29, r29, ");
            cg_n(nargs * 4);
            cg_c(10);
        }
        /* Pop callee into r2 */
        cg_s("    ldw r2, r29, 0\n    addi r29, r29, 4\n");
        cg_s("    jalr r31, r2, 0\n");
        hcg_spill_res(idx);
        return;
    }

    /* COPY (Phase B) */
    if (k == HI_COPY) {
        hcg_into(1, s1);
        hcg_spill_res(idx);
        return;
    }

    /* PHI (Phase B — should be eliminated before codegen) */
    if (k == HI_PHI) {
        return;
    }
}

/* --- Generate code for one basic block --- */

static void hcg_block(int b) {
    int i;
    cg_ldef(hcg_blk_lbl[b]);
    i = bb_start[b];
    while (i < bb_end[b]) {
        hcg_inst(i);
        i = i + 1;
    }
}

/* --- Generate one function --- */

static void hcg_func(Node *fn) {
    int fs;
    int i;
    int b;

    /* Lower AST to HIR */
    hl_func(fn);

    /* Run SSA construction (no-op in Phase A) */
    hir_ssa_construct();

    /* Assign spill slots for non-rematerializable value-producing instructions */
    i = 0;
    while (i < h_ninst) {
        if (hi_has_value(h_kind[i]) && !hi_is_remat(h_kind[i])) {
            hl_temp_stack = hl_temp_stack + 4;
            hcg_spill[i] = 0 - hl_temp_stack;
        } else {
            hcg_spill[i] = 0;
        }
        i = i + 1;
    }

    /* Compute frame size */
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
        cg_s("    addi r29, r29, -");
        cg_n(fs);
        cg_c(10);
        cg_s("    stw r29, r31, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    stw r29, r30, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_s("    addi r30, r29, ");
        cg_n(fs);
        cg_c(10);
    } else {
        /* Save r31/r30 before frame allocation using old r29 */
        cg_s("    stw r29, r31, -4\n");
        cg_s("    stw r29, r30, -8\n");
        /* r30 = old r29 (callee's frame pointer) */
        cg_s("    addi r30, r29, 0\n");
        /* Allocate frame */
        hcg_li(1, fs);
        cg_s("    sub r29, r29, r1\n");
    }

    /* Copy params from arg registers to stack slots */
    i = 0;
    while (i < fn->nparams) {
        cg_s("    stw r30, r");
        cg_n(3 + i);
        cg_s(", ");
        cg_n(-8 - (i + 1) * 4);
        cg_c(10);
        i = i + 1;
    }

    /* Emit all basic blocks */
    b = 0;
    while (b < bb_nblk) {
        hcg_block(b);
        b = b + 1;
    }

    /* Epilogue */
    cg_ldef(hcg_epilog);
    if (fs <= 2047) {
        cg_s("    ldw r31, r29, ");
        cg_n(fs - 4);
        cg_c(10);
        cg_s("    ldw r30, r29, ");
        cg_n(fs - 8);
        cg_c(10);
        cg_s("    addi r29, r29, ");
        cg_n(fs);
        cg_c(10);
    } else {
        /* r30 = old_r29 from prologue; restore via r30 */
        cg_s("    addi r29, r30, 0\n");
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

    /* Global variables (initialized → .data) */
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
