/* hir_opt.h -- SSA optimization passes for s12cc
 *
 * Runs after hir_ssa.h, before hir_codegen.h.
 * 1. Copy propagation (eliminate HI_COPY chains)
 * 2. Constant folding + strength reduction (MUL by pow2 -> SLL)
 * 3. Algebraic simplifications (identity/absorbing elements)
 * 4. Dead block elimination (NOP unreachable blocks after BRC->BR)
 * 5. PHI simplification (trivial phis -> COPY)
 * 6. Dead code elimination (remove unused value-producing instructions)
 */

/* Use count per instruction */
static int ho_use[HIR_MAX_INST];

/* Resolve COPY chains: follow src1 until non-COPY */
static int ho_resolve(int inst) {
    int depth;
    depth = 0;
    while (inst >= 0 && h_kind[inst] == HI_COPY && depth < 100) {
        inst = h_src1[inst];
        depth = depth + 1;
    }
    return inst;
}

/* Is src2 an instruction reference (not a block number)? */
static int ho_src2_is_ref(int k) {
    if (k >= HI_ADD && k <= HI_SGEU) return 1;
    if (k == HI_STORE) return 1;
    if (k == HI_RET) return 1;
    return 0;
}

/* ----------------------------------------------------------------
 * Pass 1: Copy propagation
 * Rewrite all instruction references through COPY chains.
 * Also inline rematerializable COPY sources (COPY of ICONST -> ICONST).
 * ---------------------------------------------------------------- */

static int ho_copy_prop(void) {
    int changed;
    int i;
    int j;
    int k;
    int r;
    int base;
    int cnt;

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        /* Rewrite src1 */
        if (h_src1[i] >= 0) {
            r = ho_resolve(h_src1[i]);
            if (r != h_src1[i]) {
                h_src1[i] = r;
                changed = 1;
            }
        }

        /* Rewrite src2 (only if it's an instruction ref, not a block number) */
        if (h_src2[i] >= 0 && ho_src2_is_ref(k)) {
            r = ho_resolve(h_src2[i]);
            if (r != h_src2[i]) {
                h_src2[i] = r;
                changed = 1;
            }
        }

        /* Rewrite call args */
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
            base = h_cbase[i];
            cnt = h_val[i];
            j = 0;
            while (j < cnt) {
                if (h_carg[base + j] >= 0) {
                    r = ho_resolve(h_carg[base + j]);
                    if (r != h_carg[base + j]) {
                        h_carg[base + j] = r;
                        changed = 1;
                    }
                }
                j = j + 1;
            }
        }

        /* Rewrite PHI args */
        if (k == HI_PHI && h_pbase[i] >= 0) {
            j = 0;
            while (j < h_pcnt[i]) {
                if (h_pval[h_pbase[i] + j] >= 0) {
                    r = ho_resolve(h_pval[h_pbase[i] + j]);
                    if (r != h_pval[h_pbase[i] + j]) {
                        h_pval[h_pbase[i] + j] = r;
                        changed = 1;
                    }
                }
                j = j + 1;
            }
        }

        /* Inline rematerializable COPY sources:
         * COPY of ICONST -> ICONST (saves a spill slot) */
        if (k == HI_COPY && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_ICONST) {
            h_kind[i] = HI_ICONST;
            h_val[i] = h_val[h_src1[i]];
            h_src1[i] = -1;
            changed = 1;
        }

        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Pass 2: Constant folding and algebraic simplifications
 * ---------------------------------------------------------------- */

static int ho_const_fold(void) {
    int changed;
    int i;
    int k;
    int a;
    int b;
    int result;
    int can_fold;
    int s1c;
    int s2c;
    int shift;
    int tmp;
    int ci;

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];

        /* === Binop optimizations === */
        if (k >= HI_ADD && k <= HI_SGEU) {
            s1c = (h_src1[i] >= 0 && h_kind[h_src1[i]] == HI_ICONST);
            s2c = (h_src2[i] >= 0 && h_kind[h_src2[i]] == HI_ICONST);

            if (s1c && s2c) {
                /* Both operands constant: evaluate at compile time */
                a = h_val[h_src1[i]];
                b = h_val[h_src2[i]];
                result = 0;
                can_fold = 1;

                if      (k == HI_ADD) result = a + b;
                else if (k == HI_SUB) result = a - b;
                else if (k == HI_MUL) result = a * b;
                else if (k == HI_DIV) {
                    if (b != 0) result = a / b; else can_fold = 0;
                }
                else if (k == HI_REM) {
                    if (b != 0) result = a % b; else can_fold = 0;
                }
                else if (k == HI_AND) result = a & b;
                else if (k == HI_OR)  result = a | b;
                else if (k == HI_XOR) result = a ^ b;
                else if (k == HI_SLL) result = a << (b & 31);
                else if (k == HI_SRA) result = a >> (b & 31);
                else if (k == HI_SRL) result = (int)((unsigned int)a >> (b & 31));
                else if (k == HI_SEQ) {
                    if (a == b) result = 1; else result = 0;
                }
                else if (k == HI_SNE) {
                    if (a != b) result = 1; else result = 0;
                }
                else if (k == HI_SLT) {
                    if (a < b) result = 1; else result = 0;
                }
                else if (k == HI_SGT) {
                    if (a > b) result = 1; else result = 0;
                }
                else if (k == HI_SLE) {
                    if (a <= b) result = 1; else result = 0;
                }
                else if (k == HI_SGE) {
                    if (a >= b) result = 1; else result = 0;
                }
                else if (k == HI_SLTU) {
                    if ((unsigned int)a < (unsigned int)b) result = 1; else result = 0;
                }
                else if (k == HI_SGTU) {
                    if ((unsigned int)a > (unsigned int)b) result = 1; else result = 0;
                }
                else if (k == HI_SLEU) {
                    if ((unsigned int)a <= (unsigned int)b) result = 1; else result = 0;
                }
                else if (k == HI_SGEU) {
                    if ((unsigned int)a >= (unsigned int)b) result = 1; else result = 0;
                }
                else can_fold = 0;

                if (can_fold) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = result;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
            } else if (s2c) {
                /* Right operand constant: identity/absorbing simplifications */
                b = h_val[h_src2[i]];

                /* x + 0, x - 0, x | 0, x ^ 0, x << 0, x >> 0 -> x */
                if (b == 0 && (k == HI_ADD || k == HI_SUB || k == HI_OR ||
                               k == HI_XOR || k == HI_SLL || k == HI_SRA ||
                               k == HI_SRL)) {
                    h_kind[i] = HI_COPY;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* SNE(cmp,0) -> COPY(cmp) when cmp produces 0/1 */
                else if (k == HI_SNE && b == 0 && h_src1[i] >= 0 &&
                         h_kind[h_src1[i]] >= HI_SEQ &&
                         h_kind[h_src1[i]] <= HI_SGEU) {
                    h_kind[i] = HI_COPY;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* SEQ(cmp,0) -> NOT(cmp) when cmp produces 0/1 */
                else if (k == HI_SEQ && b == 0 && h_src1[i] >= 0 &&
                         h_kind[h_src1[i]] >= HI_SEQ &&
                         h_kind[h_src1[i]] <= HI_SGEU) {
                    h_kind[i] = HI_NOT;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x * 1 -> x, x / 1 -> x */
                else if ((k == HI_MUL || k == HI_DIV) && b == 1) {
                    h_kind[i] = HI_COPY;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x % 1 -> 0 */
                else if (k == HI_REM && b == 1) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 0;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x * 0 -> 0, x & 0 -> 0 */
                else if ((k == HI_MUL || k == HI_AND) && b == 0) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 0;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x & -1 -> x */
                else if (k == HI_AND && b == -1) {
                    h_kind[i] = HI_COPY;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x | -1 -> -1 */
                else if (k == HI_OR && b == -1) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = -1;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x ^ -1 -> ~x */
                else if (k == HI_XOR && b == -1) {
                    h_kind[i] = HI_BNOT;
                    h_src2[i] = -1;
                    changed = 1;
                }
            } else if (s1c) {
                /* Left operand constant: identity/absorbing simplifications */
                a = h_val[h_src1[i]];

                /* 0 + x -> x */
                if (k == HI_ADD && a == 0) {
                    h_kind[i] = HI_COPY;
                    h_src1[i] = h_src2[i];
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* 1 * x -> x */
                else if (k == HI_MUL && a == 1) {
                    h_kind[i] = HI_COPY;
                    h_src1[i] = h_src2[i];
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* 0 * x -> 0, 0 & x -> 0 */
                else if ((k == HI_MUL || k == HI_AND) && a == 0) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 0;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* 0 | x -> x, 0 ^ x -> x */
                else if ((k == HI_OR || k == HI_XOR) && a == 0) {
                    h_kind[i] = HI_COPY;
                    h_src1[i] = h_src2[i];
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* -1 & x -> x */
                else if (k == HI_AND && a == -1) {
                    h_kind[i] = HI_COPY;
                    h_src1[i] = h_src2[i];
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* -1 | x -> -1 */
                else if (k == HI_OR && a == -1) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = -1;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* -1 ^ x -> ~x */
                else if (k == HI_XOR && a == -1) {
                    h_kind[i] = HI_BNOT;
                    h_src1[i] = h_src2[i];
                    h_src2[i] = -1;
                    changed = 1;
                }
            }

            /* Strength reduction: MUL by power-of-2 -> SLL */
            if (h_kind[i] == HI_MUL) {
                if (s2c) {
                    b = h_val[h_src2[i]];
                    if (b > 0 && (b & (b - 1)) == 0) {
                        shift = 0; tmp = b;
                        while (tmp > 1) { shift = shift + 1; tmp = tmp >> 1; }
                        ci = hi_emit(HI_ICONST, TY_INT, -1, -1, shift, NULL);
                        h_blk[ci] = h_blk[i];
                        h_kind[i] = HI_SLL;
                        h_src2[i] = ci;
                        changed = 1;
                    }
                } else if (s1c) {
                    a = h_val[h_src1[i]];
                    if (a > 0 && (a & (a - 1)) == 0) {
                        shift = 0; tmp = a;
                        while (tmp > 1) { shift = shift + 1; tmp = tmp >> 1; }
                        ci = hi_emit(HI_ICONST, TY_INT, -1, -1, shift, NULL);
                        h_blk[ci] = h_blk[i];
                        h_kind[i] = HI_SLL;
                        h_src1[i] = h_src2[i];
                        h_src2[i] = ci;
                        changed = 1;
                    }
                }
            }

            /* Strength reduction: unsigned DIV by power-of-2 -> SRL */
            if (h_kind[i] == HI_DIV && (h_ty[i] & TY_UNSIGNED) && s2c) {
                b = h_val[h_src2[i]];
                if (b > 0 && (b & (b - 1)) == 0) {
                    shift = 0; tmp = b;
                    while (tmp > 1) { shift = shift + 1; tmp = tmp >> 1; }
                    ci = hi_emit(HI_ICONST, TY_INT, -1, -1, shift, NULL);
                    h_blk[ci] = h_blk[i];
                    h_kind[i] = HI_SRL;
                    h_src2[i] = ci;
                    changed = 1;
                }
            }

            /* Strength reduction: unsigned REM by power-of-2 -> AND mask */
            if (h_kind[i] == HI_REM && (h_ty[i] & TY_UNSIGNED) && s2c) {
                b = h_val[h_src2[i]];
                if (b > 0 && (b & (b - 1)) == 0) {
                    ci = hi_emit(HI_ICONST, TY_INT, -1, -1, b - 1, NULL);
                    h_blk[ci] = h_blk[i];
                    h_kind[i] = HI_AND;
                    h_src2[i] = ci;
                    changed = 1;
                }
            }

            /* Same-operand simplifications (only if not already folded) */
            if (h_kind[i] == k && h_src1[i] >= 0 && h_src1[i] == h_src2[i]) {
                /* x - x -> 0, x ^ x -> 0, x % x -> 0 */
                if (k == HI_SUB || k == HI_XOR || k == HI_REM) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 0;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x / x -> 1 */
                else if (k == HI_DIV) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 1;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x == x -> 1, x <= x -> 1, x >= x -> 1 */
                else if (k == HI_SEQ || k == HI_SLE || k == HI_SGE ||
                         k == HI_SLEU || k == HI_SGEU) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 1;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x != x -> 0, x < x -> 0, x > x -> 0 */
                else if (k == HI_SNE || k == HI_SLT || k == HI_SGT ||
                         k == HI_SLTU || k == HI_SGTU) {
                    h_kind[i] = HI_ICONST;
                    h_val[i] = 0;
                    h_src1[i] = -1;
                    h_src2[i] = -1;
                    changed = 1;
                }
                /* x & x -> x, x | x -> x */
                else if (k == HI_AND || k == HI_OR) {
                    h_kind[i] = HI_COPY;
                    h_src2[i] = -1;
                    changed = 1;
                }
            }
        }

        /* === Unary ops with constant operand === */
        if (k == HI_NEG && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_ICONST) {
            h_kind[i] = HI_ICONST;
            h_val[i] = 0 - h_val[h_src1[i]];
            h_src1[i] = -1;
            changed = 1;
        }
        if (k == HI_NOT && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_ICONST) {
            h_kind[i] = HI_ICONST;
            if (h_val[h_src1[i]] == 0) h_val[i] = 1;
            else h_val[i] = 0;
            h_src1[i] = -1;
            changed = 1;
        }
        /* NOT(comparison) -> inverted comparison */
        if (k == HI_NOT && h_src1[i] >= 0) {
            int sk;
            int inv;
            sk = h_kind[h_src1[i]];
            inv = -1;
            if (sk == HI_SEQ)  inv = HI_SNE;
            else if (sk == HI_SNE)  inv = HI_SEQ;
            else if (sk == HI_SLT)  inv = HI_SGE;
            else if (sk == HI_SGE)  inv = HI_SLT;
            else if (sk == HI_SGT)  inv = HI_SLE;
            else if (sk == HI_SLE)  inv = HI_SGT;
            else if (sk == HI_SLTU) inv = HI_SGEU;
            else if (sk == HI_SGEU) inv = HI_SLTU;
            else if (sk == HI_SGTU) inv = HI_SLEU;
            else if (sk == HI_SLEU) inv = HI_SGTU;
            if (inv >= 0) {
                h_kind[i] = inv;
                h_src2[i] = h_src2[h_src1[i]];
                h_src1[i] = h_src1[h_src1[i]];
                changed = 1;
            }
        }
        /* NOT(NOT(x)) -> COPY(x) */
        if (k == HI_NOT && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_NOT) {
            h_kind[i] = HI_COPY;
            h_src1[i] = h_src1[h_src1[i]];
            changed = 1;
        }
        if (k == HI_BNOT && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_ICONST) {
            h_kind[i] = HI_ICONST;
            h_val[i] = (-1) ^ h_val[h_src1[i]];
            h_src1[i] = -1;
            changed = 1;
        }

        /* === ADDI peepholes === */
        if (k == HI_ADDI) {
            /* addi x, 0 -> x */
            if (h_val[i] == 0) {
                h_kind[i] = HI_COPY;
                changed = 1;
            }
            /* addi const, c -> const */
            else if (h_src1[i] >= 0 && h_kind[h_src1[i]] == HI_ICONST) {
                h_kind[i] = HI_ICONST;
                h_val[i] = h_val[h_src1[i]] + h_val[i];
                h_src1[i] = -1;
                changed = 1;
            }
        }

        /* === BRC with constant condition -> unconditional BR === */
        if (k == HI_BRC && h_src1[i] >= 0 &&
            h_kind[h_src1[i]] == HI_ICONST) {
            if (h_val[h_src1[i]] != 0) {
                /* Always true: branch to then-block (src2) */
                h_kind[i] = HI_BR;
                h_val[i] = h_src2[i];
            } else {
                /* Always false: branch to else-block (val unchanged) */
                h_kind[i] = HI_BR;
            }
            h_src1[i] = -1;
            h_src2[i] = -1;
            changed = 1;
        }

        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Pass 3: Branch simplification
 * ---------------------------------------------------------------- */

static int ho_branch_simplify(void) {
    int changed;
    int i;

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] == HI_BRC) {
            /* If both BRC successors are identical, collapse to BR. */
            if (h_src2[i] == h_val[i]) {
                h_kind[i] = HI_BR;
                h_val[i] = h_src2[i];
                h_src1[i] = -1;
                h_src2[i] = -1;
                changed = 1;
            }
        }
        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Pass 4: PHI simplification
 * If all non-self-referential PHI args are the same value,
 * replace the PHI with a COPY of that value.
 * ---------------------------------------------------------------- */

static int ho_phi_simplify(void) {
    int changed;
    int i;
    int j;
    int v;
    int unique;

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] != HI_PHI) { i = i + 1; continue; }
        if (h_pbase[i] < 0 || h_pcnt[i] == 0) { i = i + 1; continue; }

        /* Find unique non-self-referential arg */
        unique = -1;
        j = 0;
        while (j < h_pcnt[i]) {
            v = h_pval[h_pbase[i] + j];
            if (v != i) {
                if (unique == -1) {
                    unique = v;
                } else if (v != unique) {
                    unique = -2;  /* multiple distinct values */
                }
            }
            j = j + 1;
        }

        if (unique >= 0) {
            /* All non-self args are the same value -> COPY */
            h_kind[i] = HI_COPY;
            h_src1[i] = unique;
            h_pbase[i] = -1;
            h_pcnt[i] = 0;
            changed = 1;
        }

        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Pass 5: Dead block elimination
 * DFS from block 0, NOP unreachable blocks, fix PHI args from
 * dead predecessors (make self-referential so phi_simplify ignores them).
 * Reuses ho_use[] as visited + DFS stack (non-overlapping regions).
 * ---------------------------------------------------------------- */

static int ho_dead_blocks(void) {
    int changed;
    int i;
    int j;
    int sp;
    int b;
    int t;
    int k;

    if (bb_nblk == 0) return 0;

    /* ho_use[0..bb_nblk-1] = visited, ho_use[bb_nblk..] = DFS stack */
    i = 0;
    while (i < bb_nblk) { ho_use[i] = 0; i = i + 1; }

    /* DFS from block 0 */
    sp = bb_nblk;
    ho_use[0] = 1;
    ho_use[sp] = 0;
    sp = sp + 1;

    while (sp > bb_nblk) {
        sp = sp - 1;
        b = ho_use[sp];

        /* Scan block for branch targets */
        if (bb_start[b] < 0) continue;
        i = bb_start[b];
        while (i < bb_end[b]) {
            k = h_kind[i];
            if (k == HI_BR) {
                t = h_val[i];
                if (t >= 0 && t < bb_nblk && !ho_use[t]) {
                    ho_use[t] = 1;
                    ho_use[sp] = t;
                    sp = sp + 1;
                }
            }
            if (k == HI_BRC) {
                t = h_src2[i];
                if (t >= 0 && t < bb_nblk && !ho_use[t]) {
                    ho_use[t] = 1;
                    ho_use[sp] = t;
                    sp = sp + 1;
                }
                t = h_val[i];
                if (t >= 0 && t < bb_nblk && !ho_use[t]) {
                    ho_use[t] = 1;
                    ho_use[sp] = t;
                    sp = sp + 1;
                }
            }
            i = i + 1;
        }
    }

    /* NOP dead blocks + fixup PHI args from dead predecessors */
    changed = 0;
    i = 0;
    while (i < h_ninst) {
        b = h_blk[i];
        if (b >= 0 && b < bb_nblk && !ho_use[b]) {
            /* Dead block: NOP this instruction */
            if (h_kind[i] != HI_NOP) {
                h_kind[i] = HI_NOP;
                h_src1[i] = -1;
                h_src2[i] = -1;
                changed = 1;
            }
        } else if (h_kind[i] == HI_PHI && h_pbase[i] >= 0) {
            /* Live block PHI: make args from dead predecessors self-ref */
            j = 0;
            while (j < h_pcnt[i]) {
                t = h_pblk[h_pbase[i] + j];
                if (t >= 0 && t < bb_nblk && !ho_use[t]) {
                    h_pval[h_pbase[i] + j] = i;
                    changed = 1;
                }
                j = j + 1;
            }
        }
        i = i + 1;
    }

    return changed;
}

/* ----------------------------------------------------------------
 * Pass 6: Dead code elimination
 * Count uses of each instruction, delete unused value-producers
 * (except calls which have side effects).
 * ---------------------------------------------------------------- */

static void ho_count_uses(void) {
    int i;
    int j;
    int k;
    int base;
    int cnt;

    i = 0;
    while (i < h_ninst) {
        ho_use[i] = 0;
        i = i + 1;
    }

    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        /* src1 is always an instruction ref when >= 0 */
        if (h_src1[i] >= 0)
            ho_use[h_src1[i]] = ho_use[h_src1[i]] + 1;

        /* src2 is an instruction ref only for binops and STORE */
        if (h_src2[i] >= 0 && ho_src2_is_ref(k))
            ho_use[h_src2[i]] = ho_use[h_src2[i]] + 1;

        /* Call arguments */
        if ((k == HI_CALL || k == HI_CALLP) && h_cbase[i] >= 0) {
            base = h_cbase[i];
            cnt = h_val[i];
            j = 0;
            while (j < cnt) {
                if (h_carg[base + j] >= 0)
                    ho_use[h_carg[base + j]] = ho_use[h_carg[base + j]] + 1;
                j = j + 1;
            }
        }

        /* PHI arguments */
        if (k == HI_PHI && h_pbase[i] >= 0) {
            j = 0;
            while (j < h_pcnt[i]) {
                if (h_pval[h_pbase[i] + j] >= 0)
                    ho_use[h_pval[h_pbase[i] + j]] =
                        ho_use[h_pval[h_pbase[i] + j]] + 1;
                j = j + 1;
            }
        }

        i = i + 1;
    }
}

static int ho_dce(void) {
    int changed;
    int i;
    int k;

    ho_count_uses();

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP) { i = i + 1; continue; }

        /* Delete if: produces value, no uses, no side effects */
        if (hi_has_value(k) && ho_use[i] == 0 &&
            k != HI_CALL && k != HI_CALLP) {
            h_kind[i] = HI_NOP;
            h_src1[i] = -1;
            h_src2[i] = -1;
            changed = 1;
        }

        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * CSE (Common Subexpression Elimination)
 * Hash table keyed on (kind, src1, src2, val).
 * Forward scan: if duplicate found, replace with COPY.
 * ---------------------------------------------------------------- */
#define HO_CSE_SLOTS  2048
#define HO_CSE_MASK   2047

static int ho_cse_head[HO_CSE_SLOTS];   /* slot -> first inst index, -1 if empty */
static int ho_cse_next[HIR_MAX_INST];    /* inst -> next in chain, -1 if end */
static int ho_stat_cse;

static int ho_cse_hash(int kind, int s1, int s2, int val) {
    int h;
    h = kind * 73 + (s1 + 1) * 131 + (s2 + 1) * 257 + val * 37;
    if (h < 0) h = 0 - h;
    return h & HO_CSE_MASK;
}

static int ho_cse_eligible(int k) {
    /* Binary arithmetic/logic/comparison (not div/rem) */
    if (k >= HI_ADD && k <= HI_SGEU && k != HI_DIV && k != HI_REM) return 1;
    /* Unary */
    if (k == HI_NEG || k == HI_NOT || k == HI_BNOT) return 1;
    /* ADDI */
    if (k == HI_ADDI) return 1;
    return 0;
}

static int ho_cse(void) {
    int i;
    int k;
    int s1;
    int s2;
    int v;
    int slot;
    int j;
    int found;
    int changed;

    /* Initialize hash table */
    i = 0;
    while (i < HO_CSE_SLOTS) { ho_cse_head[i] = -1; i = i + 1; }

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        k = h_kind[i];
        if (k == HI_NOP || !ho_cse_eligible(k)) { i = i + 1; continue; }

        s1 = h_src1[i];
        s2 = h_src2[i];
        v = h_val[i];
        slot = ho_cse_hash(k, s1, s2, v);

        /* Search chain for match */
        found = -1;
        j = ho_cse_head[slot];
        while (j >= 0) {
            if (h_kind[j] == k && h_src1[j] == s1 && h_src2[j] == s2 &&
                h_val[j] == v && h_blk[j] == h_blk[i]) {
                found = j;
                j = -1;  /* break */
            } else {
                j = ho_cse_next[j];
            }
        }

        if (found >= 0) {
            /* Replace with COPY of the earlier instruction */
            h_kind[i] = HI_COPY;
            h_src1[i] = found;
            h_src2[i] = -1;
            h_val[i] = 0;
            changed = 1;
            ho_stat_cse = ho_stat_cse + 1;
        } else {
            /* Insert into hash table */
            ho_cse_next[i] = ho_cse_head[slot];
            ho_cse_head[slot] = i;
        }

        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Dead store elimination
 * If two STOREs in the same block write to the same address with
 * no intervening LOAD/CALL/CALLP, the first STORE is dead.
 * ---------------------------------------------------------------- */

static int ho_stat_dse;

static int ho_dse_pass(void) {
    int changed;
    int i;
    int j;
    int addr;
    int blk;
    int end;
    int alive;
    int jk;

    changed = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] != HI_STORE) { i = i + 1; continue; }
        addr = h_src1[i];  /* address operand of STORE */
        blk = h_blk[i];
        end = bb_end[blk];

        /* Scan forward in same block for another STORE to same address */
        alive = 1;
        j = i + 1;
        while (j < end && alive) {
            jk = h_kind[j];
            if (jk == HI_NOP) { j = j + 1; continue; }
            /* If any LOAD/CALL/CALLP, the stored value might be observed */
            if (jk == HI_LOAD || jk == HI_CALL || jk == HI_CALLP) {
                alive = 0;
            }
            /* Found another STORE to same address — first store is dead */
            if (jk == HI_STORE && h_src1[j] == addr) {
                h_kind[i] = HI_NOP;
                h_src1[i] = -1;
                h_src2[i] = -1;
                changed = 1;
                ho_stat_dse = ho_stat_dse + 1;
                alive = 0;
            }
            j = j + 1;
        }
        i = i + 1;
    }
    return changed;
}

/* ----------------------------------------------------------------
 * Main optimization driver: iterate until fixpoint
 * ---------------------------------------------------------------- */

/* Cumulative count of HIR instructions eliminated across all functions */
static int ho_stat_elim;

static void hir_opt(void) {
    int changed;
    int iter;
    int before;
    int after;
    int i;

    /* Count live instructions before */
    before = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] != HI_NOP) before = before + 1;
        i = i + 1;
    }

    iter = 0;
    changed = 1;
    while (changed && iter < 10) {
        changed = 0;
        changed = changed | ho_copy_prop();
        changed = changed | ho_const_fold();
        changed = changed | ho_cse();
        changed = changed | ho_branch_simplify();
        changed = changed | ho_dead_blocks();
        changed = changed | ho_phi_simplify();
        changed = changed | ho_dse_pass();
        changed = changed | ho_dce();
        iter = iter + 1;
    }

    /* Count live instructions after */
    after = 0;
    i = 0;
    while (i < h_ninst) {
        if (h_kind[i] != HI_NOP) after = after + 1;
        i = i + 1;
    }

    ho_stat_elim = ho_stat_elim + (before - after);
}
