/* hir_lower.h -- AST to HIR lowering for s12cc
 *
 * Walks each function's AST and emits HIR instructions.
 * Phase A: no SSA, locals accessed via alloca+load/store.
 * Ternary/&&/|| use temp stack slots to merge values.
 */

/* --- Alloca map: fp-offset → HIR instruction --- */
/* Raised from 256 for inlining: every inline instance mints fresh
 * parameter and result slots, so a function with many inlined call
 * sites needs far more entries than a hand-written one.  (dtoa.c hit
 * the old ceiling immediately.)  hl_inl_candidate also refuses to
 * inline once capacity runs low, so exhausting this is a missed
 * optimisation rather than a failed compile. */
#define HL_MAX_ALLOCA 2048
static int hl_aoff[HL_MAX_ALLOCA];
static int hl_ainst[HL_MAX_ALLOCA];
static int hl_nalloca;

/* Temp stack for merge values (&&, ||, ternary) */
static int hl_temp_stack;

/* --- Loop break/continue targets --- */
#define HL_MAX_LOOP 16
static int hl_break_blk[HL_MAX_LOOP];
static int hl_cont_blk[HL_MAX_LOOP];
static int hl_loop_depth;

/* --- Switch state --- */
#define HL_MAX_SW_DEPTH 8
#define HL_MAX_CASE 256
static int hl_sw_val[HL_MAX_CASE];
static int hl_sw_blk[HL_MAX_CASE];
static int hl_sw_base[HL_MAX_SW_DEPTH];
static int hl_sw_count[HL_MAX_SW_DEPTH];
static int hl_sw_def[HL_MAX_SW_DEPTH];
static int hl_sw_cur[HL_MAX_SW_DEPTH];
static int hl_sw_depth;
static int hl_sw_ord[HL_MAX_CASE];

/* --- Jump-table lowering (issue #32) --- */
/* Max span (case-value range hi-lo+1) we will materialise as a jump table.
 * Bounds the .data table size (HL_JT_MAX_SPAN * 4 bytes) and the build
 * buffer below. */
#define HL_JT_MAX_SPAN 1024
static int hl_jt_tgt[HL_JT_MAX_SPAN];   /* per-index target block (build buffer) */

/* --- Goto label map --- */
#define HL_MAX_GOTO 512
static int hl_goto_id[HL_MAX_GOTO];
static int hl_goto_blk[HL_MAX_GOTO];
static int hl_ngoto;

/* --- 64-bit twin-value tracking --- */
/* After hl_expr() for a TY_LLONG expression, hl_hi holds the hi word index.
 * Callers must save hl_hi before calling hl_expr() again. */
static int hl_hi;
static int hl_struct_ret;   /* 1 if current func returns struct via hidden ptr */
static int hl_retptr_alloca; /* alloca index for hidden __retptr param */
static int hl_ret_size;     /* struct return size in bytes */

/* Widen a 32-bit value to 64-bit (sign or zero extend) */
static void hl_widen64(int val, int from_ty, int *out_lo, int *out_hi) {
    int c31;
    *out_lo = val;
    if (from_ty & TY_UNSIGNED) {
        *out_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
    } else {
        c31 = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
        *out_hi = hi_emit(HI_SRA, TY_INT, val, c31, 0, NULL);
    }
}

/* Promote a 32-bit value (int or float) to f64 twin pair via helper call.
 * After return, hl_hi holds the hi word. */
static int hl_promote_to_f64(int val, int from_ty);

/* Number of physical params (set by hl_func, used by hl_expr for VA_START) */
static int hl_nparams;

/* Forward declarations */
static int hl_expr(Node *n);
static int hl_addr(Node *n);
static void hl_stmt(Node *n);

/* --- Helpers --- */

/* =================================================================
 * Function inlining (SLOW-32 and the crosses; gated by hl_inline_max)
 *
 * Not an AST transform: the callee's body AST is LOWERED AGAIN at the
 * call site, with its frame shifted into the caller's.  That buys three
 * things a clone-and-splice would not.  Evaluation order and
 * conditionality are preserved for free -- a call inside `a && f(b)` is
 * lowered where the && already put it, so no call-position whitelist is
 * needed.  No AST cloning means no node renaming and no aliasing
 * hazards.  And the callee's locals become ordinary allocas in the
 * caller's frame, so mem2reg (and ssa_split_pair_allocas for its
 * doubles) promotes them exactly like the caller's own.
 *
 * The frame is relocated by ONE constant shift rather than a
 * per-variable map: reserve locals_size bytes in the caller and map
 * every callee offset to `off - shift`.  Arrays and structs come along
 * without needing their sizes, and 8-alignment survives because the
 * shift is a multiple of 8.
 *
 * `return` inside an inlined body stores to a result slot and branches
 * to a continuation block (hl_inl_ret_blk) instead of emitting HI_RET.
 *
 * Refused: varargs, struct parameters or struct return, bodies
 * containing labels or goto (label scoping is per-function), recursion
 * (direct or mutual, via the active stack), and anything over budget.
 * ================================================================= */
#define HL_INL_MAX_DEPTH 3
static Node *hl_prog;            /* program root; set by the codegen driver */
static int   hl_inline_max;      /* node budget; 0 disables inlining */
static int   hl_inl_depth;
static Node *hl_inl_stack[HL_INL_MAX_DEPTH];
static int   hl_inl_shift;       /* callee frame -> caller frame */
static int   hl_inl_ret_blk;     /* continuation block for `return` */
static int   hl_inl_res;         /* result alloca, -1 when void */
static int   hl_inl_res_ty;
static int   hl_stat_inlined;

/* Callee offsets are relocated by one constant; 0 when not inlining. */
static int hl_map_off(int off) {
    return off - hl_inl_shift;
}

static int hl_get_alloca(int offset, int ty) {
    int i;
    i = 0;
    while (i < hl_nalloca) {
        if (hl_aoff[i] == offset) return hl_ainst[i];
        i = i + 1;
    }
    if (hl_nalloca >= HL_MAX_ALLOCA) {
        fdputs("s12cc: too many allocas\n", 2);
        exit(1);
    }
    hl_aoff[hl_nalloca] = offset;
    hl_ainst[hl_nalloca] = hi_emit(HI_ALLOCA, ty, -1, -1, offset, NULL);
    hl_nalloca = hl_nalloca + 1;
    return hl_ainst[hl_nalloca - 1];
}

static int hl_alloc_temp(void) {
    hl_temp_stack = hl_temp_stack + 4;
    return 0 - hl_temp_stack;
}

/* Emit a fresh HI_ALLOCA for a compiler-generated temp (&&/||, ternary,
 * struct return slot, f64 split, etc.) AND register it in hl_ainst so
 * SSA's mem2reg pass can promote it.  Without registration, the alloca
 * was created but invisible to find_promo, leaving every LOAD/STORE on
 * the temp slot unconverted -- the alloca's spill slot then read stack
 * garbage on entry edges where the parallel-copy never fired.
 *
 * Use this everywhere a non-source-level local needs an HI_ALLOCA. */
static int hl_emit_temp_alloca(int ty, int offset) {
    int inst;
    inst = hi_emit(HI_ALLOCA, ty, -1, -1, offset, NULL);
    if (hl_nalloca < HL_MAX_ALLOCA) {
        hl_aoff[hl_nalloca] = offset;
        hl_ainst[hl_nalloca] = inst;
        hl_nalloca = hl_nalloca + 1;
    }
    return inst;
}

/* Type to use for an HIR address expression (pointer arithmetic, alloca
 * results, struct member offsets, etc.).  On 64-bit hosts this MUST be
 * a wide type so the codegen emits 64-bit ADD/SPILL/RELOAD; otherwise
 * the upper 32 bits of stack/heap addresses get dropped.  On 32-bit
 * SLOW-32 native, TY_INT is the right width. */
#ifdef S12CC_X64_HOST
#define HL_ADDR_TY  (TY_PTR | TY_VOID)
#else
#define HL_ADDR_TY  TY_INT
#endif

static int hl_label_block(int label_id) {
    int i;
    i = 0;
    while (i < hl_ngoto) {
        if (hl_goto_id[i] == label_id) return hl_goto_blk[i];
        i = i + 1;
    }
    if (hl_ngoto >= HL_MAX_GOTO) {
        fdputs("s12cc: too many goto labels\n", 2);
        exit(1);
    }
    hl_goto_id[hl_ngoto] = label_id;
    hl_goto_blk[hl_ngoto] = hir_new_block();
    hl_ngoto = hl_ngoto + 1;
    return hl_goto_blk[hl_ngoto - 1];
}

/* A statement that unconditionally transfers control, so the next case
 * label is NOT reached by fall-through from it. */
static int hl_stmt_terminates(Node *s) {
    if (!s) return 0;
    if (s->kind == ND_RETURN) return 1;
    if (s->kind == ND_BREAK) return 1;
    if (s->kind == ND_CONTINUE) return 1;
    if (s->kind == ND_GOTO) return 1;
    return 0;
}

/* Conservatively report whether any case in the switch body falls through
 * into the next case/default label.  Jump tables target case blocks
 * directly, so a fall-through (which gives a case block a second
 * predecessor and thus a possible phi) would need a phi copy on the
 * un-splittable jump-table edge.  When this returns true we decline the
 * jump table and use the comparison tree, whose per-edge BRCs carry phi
 * copies normally. */
static int hl_switch_has_fallthrough(Node *body) {
    Node *s;
    Node *prev;
    int seen_label;
    if (!body || body->kind != ND_BLOCK) return 1;  /* unknown shape: be safe */
    prev = NULL;
    seen_label = 0;
    s = body->body;
    while (s) {
        if (s->kind == ND_CASE || s->kind == ND_DEFAULT) {
            if (seen_label && !hl_stmt_terminates(prev)) return 1;
            seen_label = 1;
        }
        prev = s;
        s = s->next;
    }
    return 0;
}

static int hl_sw_less_val(int a, int b, int is_unsigned) {
    if (is_unsigned) return ((unsigned)a) < ((unsigned)b);
    return a < b;
}

static void hl_sw_sort_cases(int sw_b, int sw_n, int is_unsigned) {
    int i;
    int j;
    int key;
    int keyv;
    int curv;

    i = 0;
    while (i < sw_n) {
        hl_sw_ord[sw_b + i] = sw_b + i;
        i = i + 1;
    }

    i = 1;
    while (i < sw_n) {
        key = hl_sw_ord[sw_b + i];
        keyv = hl_sw_val[key];
        j = i - 1;
        while (j >= 0) {
            curv = hl_sw_val[hl_sw_ord[sw_b + j]];
            if (!hl_sw_less_val(keyv, curv, is_unsigned)) break;
            hl_sw_ord[sw_b + j + 1] = hl_sw_ord[sw_b + j];
            j = j - 1;
        }
        hl_sw_ord[sw_b + j + 1] = key;
        i = i + 1;
    }
}

static void hl_sw_emit_chain(int lv, int def_blk, int sw_b, int sw_n) {
    int sw_i;
    int cv;
    int cmp;
    int next_blk;

    sw_i = 0;
    while (sw_i < sw_n) {
        cv = hi_emit(HI_ICONST, TY_INT, -1, -1, hl_sw_val[sw_b + sw_i], NULL);
        cmp = hi_emit(HI_SEQ, TY_INT, lv, cv, 0, NULL);
        next_blk = hir_new_block();
        hi_emit(HI_BRC, 0, cmp, hl_sw_blk[sw_b + sw_i], next_blk, NULL);
        hl_switch_block(next_blk);
        sw_i = sw_i + 1;
    }

    hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
}

static void hl_sw_emit_bsearch(int lv, int def_blk, int lt_kind, int lo, int hi) {
    int mid;
    int idx;
    int c;
    int cv;
    int cmp_eq;
    int cmp_lt;
    int neq_blk;
    int left_blk;
    int right_blk;

    if (lo > hi) {
        hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
        return;
    }

    mid = lo + (hi - lo) / 2;
    idx = hl_sw_ord[mid];
    c = hl_sw_val[idx];
    cv = hi_emit(HI_ICONST, TY_INT, -1, -1, c, NULL);
    cmp_eq = hi_emit(HI_SEQ, TY_INT, lv, cv, 0, NULL);
    if (lo == hi) {
        hi_emit(HI_BRC, 0, cmp_eq, hl_sw_blk[idx], def_blk, NULL);
        return;
    }

    neq_blk = hir_new_block();
    hi_emit(HI_BRC, 0, cmp_eq, hl_sw_blk[idx], neq_blk, NULL);

    hl_switch_block(neq_blk);
    cv = hi_emit(HI_ICONST, TY_INT, -1, -1, c, NULL);
    cmp_lt = hi_emit(lt_kind, TY_INT, lv, cv, 0, NULL);
    left_blk = hir_new_block();
    right_blk = hir_new_block();
    hi_emit(HI_BRC, 0, cmp_lt, left_blk, right_blk, NULL);

    hl_switch_block(left_blk);
    hl_sw_emit_bsearch(lv, def_blk, lt_kind, lo, mid - 1);

    hl_switch_block(right_blk);
    hl_sw_emit_bsearch(lv, def_blk, lt_kind, mid + 1, hi);
}

/* Try to lower the switch as an O(1) jump table.  Returns 1 if emitted,
 * 0 if it declined (caller falls back to the comparison tree).
 *
 * Requires the caller to have already verified there is no fall-through
 * (so every case block has the dispatch as its sole predecessor and thus
 * no phi from the jump-table edge).  Table holes route through one
 * trampoline (BR default), itself single-predecessor.  The bounds-check
 * BRC reaches the default via a normal edge that carries any phi copies.
 *
 * Emits in the CURRENT block:
 *     idx = lv - lo                  (skipped when lo == 0)
 *     if ((unsigned)idx < span) -> jt_blk else -> def_blk
 *   jt_blk:  JMPTAB idx -> table[idx]
 *   dtramp:  BR def_blk              (only when there are holes) */
static int hl_sw_emit_jumptable(int lv, int def_blk, int sw_b, int sw_n) {
    int i;
    int lo;
    int hi;
    int v;
    int span;
    int idx;
    int span_c;
    int cmp;
    int jt_blk;
    int dtramp;
    int hole_tgt;
    int has_holes;
    unsigned urange;

    lo = hl_sw_val[sw_b];
    hi = lo;
    i = 1;
    while (i < sw_n) {
        v = hl_sw_val[sw_b + i];
        if (v < lo) lo = v;
        if (v > hi) hi = v;
        i = i + 1;
    }
    /* Compute span (hi-lo+1) via unsigned subtraction.  hi >= lo always, so
     * the unsigned difference is the exact mathematical range with no overflow.
     * The signed `hi - lo + 1` is undefined behaviour when the case values
     * straddle the full int range (e.g. both INT_MIN and INT_MAX present);
     * it happens to wrap to <=0 or >HL_JT_MAX_SPAN and so was caught by the old
     * guards, but that relied on two's-complement wrap.  Bailing once urange
     * reaches HL_JT_MAX_SPAN keeps span a small positive int, which also keeps
     * every `hl_jt_tgt[caseval - lo]` index below in [0,span). */
    urange = (unsigned)hi - (unsigned)lo;
    if (urange >= (unsigned)HL_JT_MAX_SPAN) return 0;  /* too big */
    span = (int)urange + 1;
    if (span > 4 * sw_n) return 0;                     /* too sparse */

    has_holes = (span > sw_n);
    dtramp = -1;
    hole_tgt = def_blk;
    if (has_holes) {
        dtramp = hir_new_block();
        hole_tgt = dtramp;
    }

    /* Build the per-index target table: holes -> trampoline, cases -> block. */
    i = 0;
    while (i < span) { hl_jt_tgt[i] = hole_tgt; i = i + 1; }
    i = 0;
    while (i < sw_n) {
        hl_jt_tgt[hl_sw_val[sw_b + i] - lo] = hl_sw_blk[sw_b + i];
        i = i + 1;
    }

    /* idx = lv - lo */
    if (lo == 0) {
        idx = lv;
    } else {
        v = hi_emit(HI_ICONST, TY_INT, -1, -1, lo, NULL);
        idx = hi_emit(HI_SUB, TY_INT, lv, v, 0, NULL);
    }

    /* Bounds check, unsigned: idx in [0,span) ? jt_blk : def_blk. */
    span_c = hi_emit(HI_ICONST, TY_INT, -1, -1, span, NULL);
    cmp = hi_emit(HI_SLTU, TY_INT, idx, span_c, 0, NULL);
    jt_blk = hir_new_block();
    hi_emit(HI_BRC, 0, cmp, jt_blk, def_blk, NULL);

    /* Dispatch.  Pass def = -1: all successors come from the table (holes
     * already point at the trampoline), so the JMPTAB adds no spurious
     * edge to def_blk. */
    hl_switch_block(jt_blk);
    hi_emit_jmptab(idx, -1, hl_jt_tgt, span);

    if (has_holes) {
        hl_switch_block(dtramp);
        hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
    }
    return 1;
}

/* Promote a 32-bit value (int or float) to f64 twin pair via helper call.
 * After return, hl_hi holds the hi word. */
static int hl_promote_to_f64(int val, int from_ty) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = val;
    h_ncarg = h_ncarg + 1;
    if (ty_is_float(from_ty)) {
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "__fp64_cvt_ftoD");
    } else {
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 1, "__fp64_cvt_itoD");
    }
    h_cbase[r] = cb;
    hl_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
    return r;
}

/* Unsigned 32-bit divide/modulo: SLOW-32 has only signed DIV/REM
 * instructions, so unsigned goes through the platform libcalls
 * (__udivsi3/__umodsi3 in libs32), matching clang.  A signed rem of
 * a large unsigned value went negative — W_LumpNameHash's
 * `hash % numlumps` sent every WAD lookup to the wrong bucket. */
static int hl_udivmod32(int lv, int rv, int is_rem) {
    int cb;
    int r;
    cb = h_ncarg;
    h_carg[h_ncarg] = lv;
    h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = rv;
    h_ncarg = h_ncarg + 1;
    if (is_rem) {
        r = hi_emit(HI_CALL, TY_INT | TY_UNSIGNED, -1, -1, 2, "__umodsi3");
    } else {
        r = hi_emit(HI_CALL, TY_INT | TY_UNSIGNED, -1, -1, 2, "__udivsi3");
    }
    h_cbase[r] = cb;
    return r;
}

/* Map AST binary operator token to HIR instruction kind */
static int hl_binop_kind(int op, int ty) {
    if (op == TK_PLUS) return HI_ADD;
    if (op == TK_MINUS) return HI_SUB;
    if (op == TK_STAR) return HI_MUL;
    if (op == TK_SLASH) return HI_DIV;
    if (op == TK_PERCENT) return HI_REM;
    if (op == TK_AMP) return HI_AND;
    if (op == TK_PIPE) return HI_OR;
    if (op == TK_CARET) return HI_XOR;
    if (op == TK_LSHIFT) return HI_SLL;
    if (op == TK_RSHIFT) {
        if (ty & TY_UNSIGNED) return HI_SRL;
        return HI_SRA;
    }
    if (op == TK_EQ) return HI_SEQ;
    if (op == TK_NE) return HI_SNE;
    if (op == TK_LT) {
        if (ty & TY_UNSIGNED) return HI_SLTU;
        return HI_SLT;
    }
    if (op == TK_GT) {
        if (ty & TY_UNSIGNED) return HI_SGTU;
        return HI_SGT;
    }
    if (op == TK_LE) {
        if (ty & TY_UNSIGNED) return HI_SLEU;
        return HI_SLE;
    }
    if (op == TK_GE) {
        if (ty & TY_UNSIGNED) return HI_SGEU;
        return HI_SGE;
    }
    return HI_ADD;
}

/* --- Bit-field load/store helpers ---
 *
 * Storage units are 1/2/4 bytes (char/short/int base type, enforced by
 * the parser).  Load extracts via SLL/SRA-or-SRL: shift the bit-field's
 * MSB to bit 31, then shift right with the right arithmetic to fill
 * the high bits (sign-extend for signed, zero-extend for unsigned).
 * Store is read-modify-write at the storage-unit granularity. */
static int hl_bf_extract(int unit_val, int width, int boff, int is_unsigned) {
    int left_shift;
    int right_shift;
    int sl_imm;
    int sr_imm;
    int shifted;
    int op;
    left_shift = 32 - width - boff;
    right_shift = 32 - width;
    if (left_shift != 0) {
        sl_imm = hi_emit(HI_ICONST, TY_INT, -1, -1, left_shift, NULL);
        shifted = hi_emit(HI_SLL, TY_INT, unit_val, sl_imm, 0, NULL);
    } else {
        shifted = unit_val;
    }
    if (right_shift == 0) return shifted;
    op = is_unsigned ? HI_SRL : HI_SRA;
    sr_imm = hi_emit(HI_ICONST, TY_INT, -1, -1, right_shift, NULL);
    return hi_emit(op, TY_INT, shifted, sr_imm, 0, NULL);
}

static int hl_bf_load(int base_addr, int unit_ty, int width, int boff,
                      int is_unsigned) {
    int unit_val;
    unit_val = hi_emit(HI_LOAD, unit_ty, base_addr, -1, 0, NULL);
    return hl_bf_extract(unit_val, width, boff, is_unsigned);
}

/* Store the low `width` bits of new_bits into the storage unit at base_addr,
 * preserving the surrounding bits.  Returns the post-truncation low-`width`
 * value (useful for chained assignment results). */
static int hl_bf_store(int base_addr, int unit_ty, int width, int boff,
                       int new_bits) {
    int mask;
    int shifted_mask;
    int inv_mask;
    int mask_const;
    int inv_const;
    int boff_const;
    int new_masked;
    int new_shifted;
    int old_val;
    int cleared;
    int new_unit;
    /* Use unsigned shift so width==32 yields 0 (not undefined).  In C the
     * stage07 compiler uses int, so (1<<32) is UB; we guard width==32. */
    if (width == 32) {
        mask = -1;
    } else {
        mask = (1 << width) - 1;
    }
    shifted_mask = mask << boff;
    inv_mask = ~shifted_mask;
    mask_const = hi_emit(HI_ICONST, TY_INT, -1, -1, mask, NULL);
    new_masked = hi_emit(HI_AND, TY_INT, new_bits, mask_const, 0, NULL);
    if (boff != 0) {
        boff_const = hi_emit(HI_ICONST, TY_INT, -1, -1, boff, NULL);
        new_shifted = hi_emit(HI_SLL, TY_INT, new_masked, boff_const, 0, NULL);
    } else {
        new_shifted = new_masked;
    }
    inv_const = hi_emit(HI_ICONST, TY_INT, -1, -1, inv_mask, NULL);
    old_val = hi_emit(HI_LOAD, unit_ty, base_addr, -1, 0, NULL);
    cleared = hi_emit(HI_AND, TY_INT, old_val, inv_const, 0, NULL);
    new_unit = hi_emit(HI_OR, TY_INT, cleared, new_shifted, 0, NULL);
    hi_emit(HI_STORE, unit_ty, base_addr, new_unit, 0, NULL);
    return new_masked;
}

/* Compute the address of a bit-field's storage unit (the base struct
 * address plus the unit's byte offset). */
static int hl_bf_unit_addr(Node *member) {
    int addr;
    addr = hl_addr(member->lhs);
    return hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, member->val, NULL);
}

/* Storage-unit load type for a bit-field's declared base type.  Use the
 * UNSIGNED form so the load zero-extends; the extract step adds the
 * correct sign bits. */
static int hl_bf_unit_ty(int member_ty) {
    return (member_ty & TY_BASE_MASK) | TY_UNSIGNED;
}

/* --- Address computation (lvalue → HIR addr instruction) --- */

static int hl_addr(Node *n) {
    int addr;

    if (n->kind == ND_VAR) {
        if (n->is_local) {
            /* hl_map_off relocates an inlined callee's frame; identity
             * when not inlining. */
            return hl_get_alloca(hl_map_off(n->offset), n->ty);
        }
        /* Global variable address */
        return hi_emit(HI_GADDR, TY_PTR + (n->ty & TY_BASE_MASK), -1, -1, 0, n->name);
    }

    /* Dereference: address of *p is just value of p */
    if (n->kind == ND_UNARY && n->op == TK_STAR) {
        return hl_expr(n->lhs);
    }

    /* Member access: base address + offset.  Use HL_ADDR_TY (8-byte ptr
     * on a64) so the codegen treats the result as wide and doesn't spill
     * it as a 32-bit int (would truncate the address). */
    if (n->kind == ND_MEMBER) {
        if (n->bit_width > 0) {
            p_error("cannot take address of bit-field (hir)");
            return -1;
        }
        addr = hl_addr(n->lhs);
        /* Keep field-zero aggregate accesses distinct from plain scalar
         * allocas; mem2reg is allowed to promote scalars, not subobjects. */
        return hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, n->val, NULL);
    }

    if (n->kind == ND_COMMA) {
        hl_expr(n->lhs);
        return hl_addr(n->rhs);
    }

    /* Struct-valued call: lowering already materializes the result in a
     * caller temp and yields its address (the retptr), so the "address of"
     * a struct call is just its value.  Covers `return val_integer(0);`
     * and member access on a call result. */
    if ((n->kind == ND_CALL || n->kind == ND_CALL_PTR) && ty_is_struct(n->ty)) {
        return hl_expr(n);
    }

    /* Struct assignment used as a value (a = b = c chains, or taking a
     * member of an assignment): hl_expr returns the destination address. */
    if (n->kind == ND_ASSIGN && ty_is_struct(n->ty)) {
        return hl_expr(n);
    }

    p_error("not an lvalue (hir)");
    return -1;
}

#ifndef S12CC_X64_HOST
/* Byval struct argument (clang's SLOW-32 convention): the caller copies
 * the struct into a fresh temp in its own frame and passes the temp's
 * ADDRESS in a stack slot that reserves the struct's full rounded size
 * (never an argument register) — see hi_abi_assign tag 16+k.  The copy
 * is per-call so a callee that writes through the pointer only touches
 * this call's private copy.  Returns the temp's alloca; kept out of
 * hl_expr so its frame stays under stage07's 12-bit offset ceiling. */
static int hl_byval_arg(int src_addr, int sz) {
    int tmp;
    int ci;
    int so;
    int dn;
    int tv;

    hl_temp_stack = hl_temp_stack + ((sz + 3) / 4) * 4;
    tmp = hl_emit_temp_alloca(HL_ADDR_TY, 0 - hl_temp_stack);
    ci = 0;
    while (ci + 4 <= sz) {
        so = hi_emit(HI_ADDI, HL_ADDR_TY, src_addr, -1, ci, NULL);
        tv = hi_emit(HI_LOAD, TY_INT, so, -1, 0, NULL);
        dn = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, ci, NULL);
        hi_emit(HI_STORE, TY_INT, dn, tv, 0, NULL);
        ci = ci + 4;
    }
    while (ci < sz) {
        so = hi_emit(HI_ADDI, HL_ADDR_TY, src_addr, -1, ci, NULL);
        tv = hi_emit(HI_LOAD, TY_CHAR, so, -1, 0, NULL);
        dn = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, ci, NULL);
        hi_emit(HI_STORE, TY_CHAR, dn, tv, 0, NULL);
        ci = ci + 1;
    }
    return tmp;
}
#endif

#ifndef S12CC_X64_HOST
/* C truthiness of a lowered value: `x != 0`.  A raw pair word is NOT
 * that for wide types — doubles must FP-compare against +0.0 (dtoa's
 * `if (!dval(&u))` fired for 255.0 because the pair's LO word is 0,
 * and -0.0 must be falsy), floats must FEQ against +0.0f bits, and
 * llong pairs must OR both words.  Plain ints pass through. */
static int hl_truthy(int ty, int lv) {
    int d_hi;
    int z;
    int cb;
    int r;

    if (ty_is_double(ty)) {
        d_hi = hl_hi;
        cb = h_ncarg;
        h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
        z = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
        h_carg[h_ncarg] = z; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = z; h_ncarg = h_ncarg + 1;
        r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_eq");
        h_cbase[r] = cb;
        return hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
    }
    if (ty_is_float(ty)) {
        z = hi_emit(HI_ICONST, TY_FLOAT, -1, -1, 0, NULL);
        r = hi_emit(HI_FEQ, TY_INT, lv, z, 0, NULL);
        return hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
    }
    if (ty_is_llong(ty)) {
        return hi_emit(HI_OR, TY_INT, lv, hl_hi, 0, NULL);
    }
    return lv;
}
#else
static int hl_truthy(int ty, int lv) {
    (void)ty;
    return lv;
}
#endif

/* Lower a condition expression to a branchable truth value. */
static int hl_cond_val(Node *n) {
    int lv;
    lv = hl_expr(n);
    return hl_truthy(n->ty, lv);
}

#ifndef S12CC_X64_HOST
/* Pair llong binary op for the RMW paths (compound assign, ++/--),
 * mirroring the ND_BINOP pair lowering: result lo returned, hi left
 * in hl_hi.  op is the TK_ operator token; shifts take rv as a plain
 * int count.  The old scalar RMW tail silently dropped the HI word —
 * dtoa's `dbits |= 0x8000000000000000ull` lost the implicit mantissa
 * bit and every digit came out wrong. */
static int hl_ll_op(int op, int uns, int lv, int lv_hi, int rv, int rv_hi) {
    int r_lo;
    int r_hi;
    int carry;
    int cb;

    if (op == TK_PLUS) {
        r_lo = hi_emit(HI_ADD, TY_INT, lv, rv, 0, NULL);
        carry = hi_emit(HI_SLTU, TY_INT, r_lo, lv, 0, NULL);
        r_hi = hi_emit(HI_ADD, TY_INT, lv_hi, rv_hi, 0, NULL);
        hl_hi = hi_emit(HI_ADD, TY_INT, r_hi, carry, 0, NULL);
        return r_lo;
    }
    if (op == TK_MINUS) {
        carry = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
        r_lo = hi_emit(HI_SUB, TY_INT, lv, rv, 0, NULL);
        r_hi = hi_emit(HI_SUB, TY_INT, lv_hi, rv_hi, 0, NULL);
        hl_hi = hi_emit(HI_SUB, TY_INT, r_hi, carry, 0, NULL);
        return r_lo;
    }
    if (op == TK_AMP) {
        r_lo = hi_emit(HI_AND, TY_INT, lv, rv, 0, NULL);
        hl_hi = hi_emit(HI_AND, TY_INT, lv_hi, rv_hi, 0, NULL);
        return r_lo;
    }
    if (op == TK_PIPE) {
        r_lo = hi_emit(HI_OR, TY_INT, lv, rv, 0, NULL);
        hl_hi = hi_emit(HI_OR, TY_INT, lv_hi, rv_hi, 0, NULL);
        return r_lo;
    }
    if (op == TK_CARET) {
        r_lo = hi_emit(HI_XOR, TY_INT, lv, rv, 0, NULL);
        hl_hi = hi_emit(HI_XOR, TY_INT, lv_hi, rv_hi, 0, NULL);
        return r_lo;
    }
    if (op == TK_LSHIFT || op == TK_RSHIFT) {
        cb = h_ncarg;
        h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = lv_hi; h_ncarg = h_ncarg + 1;
        h_carg[h_ncarg] = rv; h_ncarg = h_ncarg + 1;
        if (op == TK_LSHIFT)
            r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashldi3");
        else if (uns)
            r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__lshrdi3");
        else
            r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashrdi3");
        h_cbase[r_lo] = cb;
        hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
        return r_lo;
    }
    /* mul / div / rem via libcalls */
    cb = h_ncarg;
    h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = lv_hi; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = rv; h_ncarg = h_ncarg + 1;
    h_carg[h_ncarg] = rv_hi; h_ncarg = h_ncarg + 1;
    if (op == TK_STAR)
        r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__muldi3");
    else if (op == TK_SLASH)
        r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, uns ? "__udivdi3" : "__divdi3");
    else if (op == TK_PERCENT)
        r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, uns ? "__umoddi3" : "__moddi3");
    else {
        p_error("unsupported llong compound op (hir)");
        return -1;
    }
    h_cbase[r_lo] = cb;
    hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
    return r_lo;
}
#endif

/* --- Expression lowering --- */

/* Bounded node count: returns a value > limit as soon as it exceeds it,
 * so a huge callee costs a short walk rather than a full traversal. */
static int hl_inl_size(Node *n, int limit) {
    int c;
    if (!n) return 0;
    c = 1;
    if (c > limit) return c;
    c = c + hl_inl_size(n->lhs, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->rhs, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->cond, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->body, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->init, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->step, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->els, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->args, limit - c);
    if (c > limit) return c;
    c = c + hl_inl_size(n->next, limit - c);
    return c;
}

/* Labels and goto are function-scoped in the lowering's fixup table, so
 * a spliced copy would collide with the caller's. */
static int hl_inl_has_labels(Node *n) {
    if (!n) return 0;
    if (n->kind == ND_GOTO || n->kind == ND_LABEL) return 1;
    if (hl_inl_has_labels(n->lhs)) return 1;
    if (hl_inl_has_labels(n->rhs)) return 1;
    if (hl_inl_has_labels(n->cond)) return 1;
    if (hl_inl_has_labels(n->body)) return 1;
    if (hl_inl_has_labels(n->init)) return 1;
    if (hl_inl_has_labels(n->step)) return 1;
    if (hl_inl_has_labels(n->els)) return 1;
    if (hl_inl_has_labels(n->args)) return 1;
    if (hl_inl_has_labels(n->next)) return 1;
    return 0;
}

/* A callee that is itself a loop amortises its own call overhead over
 * its iterations, so inlining it buys almost nothing while inflating
 * the caller (LINPACK's daxpy: inlining it cost 3%, refusing it saved
 * 2.4%).  This is the same shape f77 recorded -- user subprograms are
 * loop bodies, the case inlining helps least. */
static int hl_inl_has_loop(Node *n) {
    if (!n) return 0;
    if (n->kind == ND_WHILE || n->kind == ND_FOR) return 1;
    if (hl_inl_has_loop(n->lhs)) return 1;
    if (hl_inl_has_loop(n->rhs)) return 1;
    if (hl_inl_has_loop(n->cond)) return 1;
    if (hl_inl_has_loop(n->body)) return 1;
    if (hl_inl_has_loop(n->init)) return 1;
    if (hl_inl_has_loop(n->step)) return 1;
    if (hl_inl_has_loop(n->els)) return 1;
    if (hl_inl_has_loop(n->args)) return 1;
    if (hl_inl_has_loop(n->next)) return 1;
    return 0;
}

static Node *hl_inl_find(char *name) {
    Node *f;
    if (!name || !hl_prog) return NULL;
    f = hl_prog->body;
    while (f) {
        if (f->kind == ND_FUNC && f->name && strcmp(f->name, name) == 0 && f->body)
            return f;
        f = f->next;
    }
    return NULL;
}

/* Is `fn` worth and safe to inline at this point? */
static Node *hl_inl_candidate(Node *call) {
    Node *fn;
    Node *pp;
    int i;

    if (hl_inline_max <= 0) return NULL;
    if (hl_inl_depth >= HL_INL_MAX_DEPTH) return NULL;
    fn = hl_inl_find(call->name);
    if (!fn) return NULL;
    if (fn->is_varargs) return NULL;
    if (ty_is_struct(fn->ty)) return NULL;
    /* Recursion, direct or mutual. */
    i = 0;
    while (i < hl_inl_depth) {
        if (hl_inl_stack[i] == fn) return NULL;
        i = i + 1;
    }
    pp = fn->args;
    while (pp) {
        if (ty_is_struct(pp->ty)) return NULL;
        pp = pp->next;
    }
    /* Argument count must match the parameter list exactly; a mismatch
     * means an unprototyped or wrongly-called function, and the
     * out-of-line call keeps whatever behaviour it had. */
    {
        Node *a;
        int na;
        int np;
        na = 0; a = call->args;  while (a)  { na = na + 1; a = a->next; }
        np = 0; pp = fn->args;   while (pp) { np = np + 1; pp = pp->next; }
        if (na != np) return NULL;
    }
    if (hl_inl_has_labels(fn->body)) return NULL;
    {
        int sz;
        sz = hl_inl_size(fn->body, hl_inline_max);
        if (sz > hl_inline_max) return NULL;
        if (hl_inl_has_loop(fn->body)) return NULL;
        /* Degrade gracefully rather than failing the compile.  The HIR
         * headroom is proportional to what this splice will emit --
         * a flat margin let s12cc.c's biggest functions run the arrays
         * out and abort the build. */
        if (hl_nalloca + 64 >= HL_MAX_ALLOCA) return NULL;
        /* Reserve generously: SSA phi insertion and the LICM split /
         * strength-reduction passes all APPEND to h_ninst after
         * lowering finishes, so the budget has to cover them too. */
        /* Already-huge callers get nothing: they are the functions
         * where inlining pays least and costs most, and exact
         * accounting for what a splice will emit is not worth it
         * (s12cc's own hcg_inst, a 1000-line switch, overran the block
         * ceiling mid-splice under a proportional-only rule). */
        if (h_ninst > HIR_MAX_INST / 2) return NULL;
        if (bb_nblk > HIR_MAX_BLOCK / 2) return NULL;
        if (h_ninst + sz * 16 + 4096 >= HIR_MAX_INST) return NULL;
        /* Blocks too: every splice adds the callee's control flow plus
         * a continuation, and hir_regalloc's liveness bitsets are sized
         * against HIR_MAX_BLOCK, so this ceiling must not be raised
         * casually. */
        if (bb_nblk + sz + 64 >= HIR_MAX_BLOCK) return NULL;
    }
    return fn;
}

/* Lower a call by splicing the callee's body in.  Returns the result
 * value (and sets hl_hi for 64-bit results), or -1 for void. */
static int hl_inline_call(Node *call, Node *fn) {
    int av[32];
    int av_hi[32];
    int nargs;
    int save_shift;
    int save_ret_blk;
    int save_res;
    int save_res_ty;
    int save_loop;
    int save_sw;
    int frame;
    int ret_blk;
    int res;
    int rv;
    Node *a;
    Node *pp;
    int i;

    /* 1. Actuals are evaluated in the CALLER's frame, before the shift
     *    moves offsets, and in argument order. */
    nargs = 0;
    a = call->args;
    while (a) {
        if (nargs >= 32) return -1;
        av[nargs] = hl_expr(a);
        av_hi[nargs] = (ty_is_llong(a->ty) || ty_is_double(a->ty)) ? hl_hi : -1;
        nargs = nargs + 1;
        a = a->next;
    }

    /* 2. Reserve the callee's frame in the caller, 8-aligned so its
     *    doubles keep their alignment. */
    frame = fn->locals_size;
    if (frame < 0) frame = 0;
    frame = ((frame + 7) / 8) * 8;
    hl_temp_stack = ((hl_temp_stack + 7) / 8) * 8;

    save_shift = hl_inl_shift;
    save_ret_blk = hl_inl_ret_blk;
    save_res = hl_inl_res;
    save_res_ty = hl_inl_res_ty;
    save_loop = hl_loop_depth;
    save_sw = hl_sw_depth;

    /* Shift by the high-water mark BEFORE reserving, so the callee's
     * offsets (which run [-locals_size, 0)) land INSIDE the region we
     * are about to claim.  Taking the post-bump value put them below
     * the reserved area -- outside the frame the prologue allocates --
     * where they collided with later temps and with the caller's saved
     * registers.  LINPACK's residual check caught it only once a
     * callee big enough to allocate temps afterwards was inlined. */
    hl_inl_shift = hl_temp_stack;
    hl_temp_stack = hl_temp_stack + frame;
    hl_inl_stack[hl_inl_depth] = fn;
    hl_inl_depth = hl_inl_depth + 1;
    hl_inl_depth_dbg = hl_inl_depth;
    /* A `break` in the callee must not bind to a caller loop. */
    hl_loop_depth = 0;
    hl_sw_depth = 0;

    /* 3. Result slot (mem2reg promotes it straight back out). */
    res = -1;
    if (fn->ty != TY_VOID) {
        hl_temp_stack = hl_temp_stack + 8;
        res = hl_emit_temp_alloca(fn->ty, 0 - hl_temp_stack);
    }
    ret_blk = hir_new_block();
    hl_inl_ret_blk = ret_blk;
    hl_inl_res = res;
    hl_inl_res_ty = fn->ty;

    /* 4. Bind parameters: store each actual into the (shifted) slot the
     *    callee's body already refers to. */
    pp = fn->args;
    i = 0;
    while (pp) {
        int slot;
        if (ty_is_llong(pp->ty) || ty_is_double(pp->ty)) {
#ifdef S12CC_X64_HOST
            slot = hl_get_alloca(hl_map_off(pp->offset), pp->ty);
            hi_emit(HI_STORE, pp->ty, slot, av[i], 0, NULL);
#else
            slot = hl_get_alloca(hl_map_off(pp->offset), TY_INT);
            hi_emit(HI_STORE, TY_INT, slot, av[i], 0, NULL);
            {
                int a4;
                a4 = hi_emit(HI_ADDI, TY_INT, slot, -1, 4, NULL);
                hi_emit(HI_STORE, TY_INT, a4, av_hi[i], 0, NULL);
            }
#endif
        } else {
            slot = hl_get_alloca(hl_map_off(pp->offset), pp->ty);
            hi_emit(HI_STORE, pp->ty, slot, av[i], 0, NULL);
        }
        i = i + 1;
        pp = pp->next;
    }

    /* 5. The body.  ND_RETURN inside now stores to `res` and branches. */
    hl_stmt(fn->body);
    if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, ret_blk, NULL);
    hl_switch_block(ret_blk);

    /* 6. Read the result back. */
    rv = -1;
    hl_hi = -1;
    if (res >= 0) {
        if (ty_is_llong(fn->ty) || ty_is_double(fn->ty)) {
#ifdef S12CC_X64_HOST
            rv = hi_emit(HI_LOAD, fn->ty, res, -1, 0, NULL);
#else
            {
                int a4;
                rv = hi_emit(HI_LOAD, TY_INT, res, -1, 0, NULL);
                a4 = hi_emit(HI_ADDI, TY_INT, res, -1, 4, NULL);
                hl_hi = hi_emit(HI_LOAD, TY_INT, a4, -1, 0, NULL);
            }
#endif
        } else {
            rv = hi_emit(HI_LOAD, fn->ty, res, -1, 0, NULL);
        }
    }

    hl_inl_depth = hl_inl_depth - 1;
    hl_inl_shift = save_shift;
    hl_inl_ret_blk = save_ret_blk;
    hl_inl_res = save_res;
    hl_inl_res_ty = save_res_ty;
    hl_loop_depth = save_loop;
    hl_sw_depth = save_sw;
    hl_stat_inlined = hl_stat_inlined + 1;
    return rv;
}

static int hl_expr(Node *n) {
    int lv;
    int rv;
    int cv;
    int addr;
    int val;
    int zero;
    int tmp;
    int tmp_off;
    int elem_sz;
    int scale;
    int kind;
    int rhs_blk;
    int then_blk;
    int else_blk;
    int join_blk;
    int carg_base;
    int nargs;
    int i;
    int old_val;
    int new_val;
    int callee;
    Node *a;

    if (!n) return -1;

    /* Integer literal */
    if (n->kind == ND_NUM) {
        if (ty_is_llong(n->ty)) {
            /* 64-bit literal — HIR's h_val is only 32 bits, so a single
             * HI_ICONST can't carry the full value.  Materialise it the
             * same way ND_FNUM does for native doubles: alloca 8 bytes,
             * store lo at +0 and hi at +4, then LOAD as TY_LLONG.  On
             * 64-bit hosts (cc-x64 / cc-a64) the codegen's spill-load
             * collapse makes the alloca virtual when the value is used
             * in registers; on slow32 native the alloca / store / load
             * fall out as regular memory ops, which is the only way to
             * stage a 64-bit constant on that target. */
            int ilo;
            int ihi;
#ifdef S12CC_X64_HOST
            int tmp_alloca;
            int addr4;
            hl_temp_stack = hl_temp_stack + 8;
            tmp_alloca = hl_emit_temp_alloca(TY_LLONG, 0 - hl_temp_stack);
            ilo = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val, NULL);
            ihi = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val_hi, NULL);
            hi_emit(HI_STORE, TY_INT, tmp_alloca, ilo, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, tmp_alloca, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, ihi, 0, NULL);
            return hi_emit(HI_LOAD, n->ty, tmp_alloca, -1, 0, NULL);
#else
            /* slow32 native: TY_LLONG is two 32-bit halves, passed via
             * the hl_hi side channel. */
            ilo = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val, NULL);
            ihi = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val_hi, NULL);
            hl_hi = ihi;
            return ilo;
#endif
        }
        return hi_emit(HI_ICONST, n->ty, -1, -1, n->val, NULL);
    }

    /* Float/double literal */
    if (n->kind == ND_FNUM) {
        if (ty_is_double(n->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: materialise the 64-bit constant via a temp alloca
             * (two int STOREs lo/hi, then one TY_DOUBLE LOAD).  The
             * codegen will collapse alloca+store+load into a register
             * mov in most cases via SLF.  TODO: dedicated HI_DCONST
             * with rodata pool would be cleaner. */
            int tmp_alloca;
            int flo;
            int fhi;
            int addr4;
            hl_temp_stack = hl_temp_stack + 8;
            tmp_alloca = hl_emit_temp_alloca(TY_DOUBLE, 0 - hl_temp_stack);
            flo = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val, NULL);
            fhi = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val_hi, NULL);
            hi_emit(HI_STORE, TY_INT, tmp_alloca, flo, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, tmp_alloca, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, fhi, 0, NULL);
            return hi_emit(HI_LOAD, TY_DOUBLE, tmp_alloca, -1, 0, NULL);
#else
            /* f64: lo bits in val, hi bits in val_hi — treated like llong */
            int flo;
            int fhi;
            flo = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val, NULL);
            fhi = hi_emit(HI_ICONST, TY_INT, -1, -1, n->val_hi, NULL);
            hl_hi = fhi;
            return flo;
#endif
        }
        /* f32: bits stored as int, same representation */
        return hi_emit(HI_ICONST, TY_FLOAT, -1, -1, n->val, NULL);
    }

    /* String literal */
    if (n->kind == ND_STRING) {
        return hi_emit(HI_SADDR, n->ty, -1, -1, n->val, NULL);
    }

    /* GNU inline asm subset */
    if (n->kind == ND_ASM) {
        if (n->val == ASM_A64_MRS_CNTVCT) {
            val = hi_emit(HI_A64_MRS_CNTVCT, n->lhs ? n->lhs->ty : TY_LLONG,
                          -1, -1, 0, NULL);
            if (n->lhs) {
                addr = hl_addr(n->lhs);
                hi_emit(HI_STORE, n->lhs->ty, addr, val, 0, NULL);
            }
            return val;
        }
        if (n->val == ASM_A64_DBT_TRAMPOLINE) {
            int av[8];
            int base;
            Node *aa;
            aa = n->args;
            nargs = 0;
            while (aa) {
                if (nargs >= 8) p_error("too many asm args");
                av[nargs] = hl_expr(aa);
                nargs = nargs + 1;
                aa = aa->next;
            }
            base = h_ncarg;
            i = 0;
            while (i < nargs) {
                h_carg[h_ncarg] = av[i];
                h_ncarg = h_ncarg + 1;
                i = i + 1;
            }
            val = hi_emit(HI_A64_DBT_TRAMPOLINE, TY_INT, -1, -1, nargs, NULL);
            h_cbase[val] = base;
            return val;
        }
        if (n->val == ASM_A64_DC_CVAU || n->val == ASM_A64_IC_IVAU) {
            if (!n->args) p_error("cache maintenance asm needs one operand");
            val = hl_expr(n->args);
            if (n->val == ASM_A64_DC_CVAU)
                return hi_emit(HI_A64_DC_CVAU, TY_INT, val, -1, 0, NULL);
            return hi_emit(HI_A64_IC_IVAU, TY_INT, val, -1, 0, NULL);
        }
        if (n->val == ASM_A64_DSB_ISH) {
            return hi_emit(HI_A64_DSB_ISH, TY_INT, -1, -1, 0, NULL);
        }
        if (n->val == ASM_A64_ISB) {
            return hi_emit(HI_A64_ISB, TY_INT, -1, -1, 0, NULL);
        }
        if (n->val == ASM_X64_RDTSC) {
            /* Outputs are "=a"(lo), "=d"(hi) — two 32-bit lvalues.
             * Lower as: emit HI_X64_RDTSC with src1 = &lo, src2 = &hi.
             * Codegen issues `lfence;rdtsc` and stores eax/edx via the
             * passed pointers. */
            int lo_addr; int hi_addr;
            Node *out;
            out = n->lhs;
            if (!out || !out->next) p_error("rdtsc asm needs two outputs");
            lo_addr = hl_addr(out);
            hi_addr = hl_addr(out->next);
            return hi_emit(HI_X64_RDTSC, TY_INT, lo_addr, hi_addr, 0, NULL);
        }
        if (n->val == ASM_X64_DBT_TRAMPOLINE) {
            int av[8];
            int base;
            Node *aa;
            aa = n->args;
            nargs = 0;
            while (aa) {
                if (nargs >= 8) p_error("too many asm args");
                av[nargs] = hl_expr(aa);
                nargs = nargs + 1;
                aa = aa->next;
            }
            base = h_ncarg;
            i = 0;
            while (i < nargs) {
                h_carg[h_ncarg] = av[i];
                h_ncarg = h_ncarg + 1;
                i = i + 1;
            }
            val = hi_emit(HI_X64_DBT_TRAMPOLINE, TY_INT, -1, -1, nargs, NULL);
            h_cbase[val] = base;
            return val;
        }
        p_error("unsupported inline asm");
        return -1;
    }

    /* Variable */
    if (n->kind == ND_VAR) {
        if (n->is_array || ty_is_struct(n->ty)) {
            return hl_addr(n);
        }
        addr = hl_addr(n);
#ifdef S12CC_X64_HOST
        if (ty_is_llong(n->ty))
            return hi_emit(HI_LOAD, TY_LLONG, addr, -1, 0, NULL);
#endif
#ifdef S12CC_NATIVE_F64
        if (ty_is_double(n->ty))
            return hi_emit(HI_LOAD, TY_DOUBLE, addr, -1, 0, NULL);
#endif
        if (ty_is_llong(n->ty) || ty_is_double(n->ty)) {
            lv = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
            tmp = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hl_hi = hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
            return lv;
        }
        return hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
    }

    /* Assignment */
    if (n->kind == ND_ASSIGN) {
        /* Bit-field LHS: RMW the storage unit; return rhs as the
         * assignment-expression value (C99 says the converted value, but
         * truncation is only observable on overflow, which most callers
         * don't depend on). */
        if (n->lhs->kind == ND_MEMBER && n->lhs->bit_width > 0) {
            int base_addr;
            int rhs_val;
            int unit_ty;
            base_addr = hl_bf_unit_addr(n->lhs);
            unit_ty = hl_bf_unit_ty(n->lhs->ty);
            rhs_val = hl_expr(n->rhs);
            hl_bf_store(base_addr, unit_ty, n->lhs->bit_width,
                        n->lhs->bit_off, rhs_val);
            return rhs_val;
        }
        if (ty_is_struct(n->ty)) {
            /* Struct assignment: word-by-word copy */
            int sa;
            int da;
            int sz;
            int off;
            int tmp;
            int sp;
            int dp;
            sa = hl_expr(n->rhs);  /* address of source (works for vars, calls, members) */
            da = hl_addr(n->lhs);
            sz = ty_size(n->ty);
            off = 0;
            while (off + 4 <= sz) {
                sp = hi_emit(HI_ADDI, HL_ADDR_TY, sa, -1, off, NULL);
                tmp = hi_emit(HI_LOAD, TY_INT, sp, -1, 0, NULL);
                dp = hi_emit(HI_ADDI, HL_ADDR_TY, da, -1, off, NULL);
                hi_emit(HI_STORE, TY_INT, dp, tmp, 0, NULL);
                off = off + 4;
            }
            while (off < sz) {
                sp = hi_emit(HI_ADDI, HL_ADDR_TY, sa, -1, off, NULL);
                tmp = hi_emit(HI_LOAD, TY_CHAR, sp, -1, 0, NULL);
                dp = hi_emit(HI_ADDI, HL_ADDR_TY, da, -1, off, NULL);
                hi_emit(HI_STORE, TY_CHAR, dp, tmp, 0, NULL);
                off = off + 1;
            }
            return da;
        }
#ifdef S12CC_X64_HOST
        if (ty_is_llong(n->ty)) {
            val = hl_expr(n->rhs);
            addr = hl_addr(n->lhs);
            hi_emit(HI_STORE, TY_LLONG, addr, val, 0, NULL);
            return val;
        }
#endif
        if (ty_is_llong(n->ty)) {
            int val_hi;
            int addr4;
            val = hl_expr(n->rhs);
            val_hi = hl_hi;
            if (!ty_is_llong(n->rhs->ty)) {
                hl_widen64(val, n->rhs->ty, &val, &val_hi);
            }
            addr = hl_addr(n->lhs);
            hi_emit(HI_STORE, TY_INT, addr, val, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, val_hi, 0, NULL);
            hl_hi = val_hi;
            return val;
        }
        if (ty_is_double(n->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: single 8-byte STORE.  Promote the rhs through
             * HI_FCVT_*toD if it isn't already a double. */
            val = hl_expr(n->rhs);
            if (!ty_is_double(n->rhs->ty)) {
                if (ty_is_float(n->rhs->ty)) {
                    val = hi_emit(HI_FCVT_FtoD, TY_DOUBLE, val, -1, 0, NULL);
                } else {
                    val = hi_emit(HI_FCVT_ItoF, TY_DOUBLE, val, -1, 0, NULL);
                }
            }
            addr = hl_addr(n->lhs);
            hi_emit(HI_STORE, TY_DOUBLE, addr, val, 0, NULL);
            return val;
#else
            int val_hi;
            int addr4;
            val = hl_expr(n->rhs);
            val_hi = hl_hi;
            if (!ty_is_double(n->rhs->ty)) {
                val = hl_promote_to_f64(val, n->rhs->ty);
                val_hi = hl_hi;
            }
            addr = hl_addr(n->lhs);
            hi_emit(HI_STORE, TY_INT, addr, val, 0, NULL);
            addr4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, addr4, val_hi, 0, NULL);
            hl_hi = val_hi;
            return val;
#endif
        }
        val = hl_expr(n->rhs);
        addr = hl_addr(n->lhs);
        hi_emit(HI_STORE, n->ty, addr, val, 0, NULL);
        return val;
    }

    /* Unary operations */
    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS) {
#ifdef S12CC_X64_HOST
            if (ty_is_llong(n->ty)) {
                lv = hl_expr(n->lhs);
                return hi_emit(HI_NEG, TY_LLONG, lv, -1, 0, NULL);
            }
#endif
            if (ty_is_llong(n->ty)) {
                int lv_hi;
                int r_lo;
                int r_hi;
                int borrow;
                int z;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
                z = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                r_lo = hi_emit(HI_SUB, TY_INT, z, lv, 0, NULL);
                borrow = hi_emit(HI_SLTU, TY_INT, z, lv, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, z, lv_hi, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, r_hi, borrow, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            lv = hl_expr(n->lhs);
            if (ty_is_double(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
                return hi_emit(HI_FNEG, TY_DOUBLE, lv, -1, 0, NULL);
#else
                int d_hi;
                int cb;
                d_hi = hl_hi;
                cb = h_ncarg;
                h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
                lv = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_neg");
                h_cbase[lv] = cb;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, lv, -1, 0, NULL);
                return lv;
#endif
            }
            if (ty_is_float(n->lhs->ty))
                return hi_emit(HI_FNEG, TY_FLOAT, lv, -1, 0, NULL);
            return hi_emit(HI_NEG, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_BANG) {
#ifdef S12CC_X64_HOST
            if (ty_is_llong(n->lhs->ty)) {
                lv = hl_expr(n->lhs);
                return hi_emit(HI_NOT, TY_INT, lv, -1, 0, NULL);
            }
#endif
            if (ty_is_llong(n->lhs->ty)) {
                int lv_hi;
                int eq_lo;
                int eq_hi;
                int z;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                z = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                eq_lo = hi_emit(HI_SEQ, TY_INT, lv, z, 0, NULL);
                eq_hi = hi_emit(HI_SEQ, TY_INT, lv_hi, z, 0, NULL);
                return hi_emit(HI_AND, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
#ifndef S12CC_X64_HOST
            /* !double / !float are FP compares against +0.0 — a raw
             * word NOT is pair-blind (dtoa returned "0" for 255.0). */
            if (ty_is_fp(n->lhs->ty)) {
                lv = hl_expr(n->lhs);
                lv = hl_truthy(n->lhs->ty, lv);
                return hi_emit(HI_NOT, TY_INT, lv, -1, 0, NULL);
            }
#endif
            lv = hl_expr(n->lhs);
            return hi_emit(HI_NOT, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_TILDE) {
#ifdef S12CC_X64_HOST
            if (ty_is_llong(n->ty)) {
                lv = hl_expr(n->lhs);
                return hi_emit(HI_BNOT, TY_LLONG, lv, -1, 0, NULL);
            }
#endif
            if (ty_is_llong(n->ty)) {
                int lv_hi;
                lv = hl_expr(n->lhs);
                lv_hi = hl_hi;
                if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
                lv = hi_emit(HI_BNOT, TY_INT, lv, -1, 0, NULL);
                hl_hi = hi_emit(HI_BNOT, TY_INT, lv_hi, -1, 0, NULL);
                return lv;
            }
            lv = hl_expr(n->lhs);
            return hi_emit(HI_BNOT, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_STAR) {
            lv = hl_expr(n->lhs);
            if (ty_is_struct(n->ty)) return lv;
#ifdef S12CC_X64_HOST
            if (ty_is_llong(n->ty)) {
                return hi_emit(HI_LOAD, TY_LLONG, lv, -1, 0, NULL);
            }
#endif
#ifdef S12CC_NATIVE_F64
            if (ty_is_double(n->ty)) {
                return hi_emit(HI_LOAD, TY_DOUBLE, lv, -1, 0, NULL);
            }
#endif
            if (ty_is_double(n->ty) || ty_is_llong(n->ty)) {
                int t2;
                val = hi_emit(HI_LOAD, TY_INT, lv, -1, 0, NULL);
                t2 = hi_emit(HI_ADDI, HL_ADDR_TY, lv, -1, 4, NULL);
                hl_hi = hi_emit(HI_LOAD, TY_INT, t2, -1, 0, NULL);
                return val;
            }
            return hi_emit(HI_LOAD, n->ty, lv, -1, 0, NULL);
        }
        if (n->op == TK_AMP) {
            return hl_addr(n->lhs);
        }
        p_error("unknown unary (hir)");
        return -1;
    }

    /* Binary operations */
    if (n->kind == ND_BINOP) {
        /* Short-circuit && */
        if (n->op == TK_LAND) {
            tmp_off = hl_alloc_temp();
            tmp = hl_emit_temp_alloca(TY_INT, tmp_off);
            lv = hl_cond_val(n->lhs);
            rhs_blk = hir_new_block();
            else_blk = hir_new_block();
            join_blk = hir_new_block();
            hi_emit(HI_BRC, 0, lv, rhs_blk, else_blk, NULL);

            hl_switch_block(rhs_blk);
            rv = hl_cond_val(n->rhs);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            val = hi_emit(HI_SNE, TY_INT, rv, zero, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(else_blk);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, zero, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(join_blk);
            return hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
        }

        /* Short-circuit || */
        if (n->op == TK_LOR) {
            tmp_off = hl_alloc_temp();
            tmp = hl_emit_temp_alloca(TY_INT, tmp_off);
            lv = hl_cond_val(n->lhs);
            rhs_blk = hir_new_block();
            then_blk = hir_new_block();
            join_blk = hir_new_block();
            hi_emit(HI_BRC, 0, lv, then_blk, rhs_blk, NULL);

            hl_switch_block(rhs_blk);
            rv = hl_cond_val(n->rhs);
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            val = hi_emit(HI_SNE, TY_INT, rv, zero, 0, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(then_blk);
            val = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(join_blk);
            return hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
        }

        /* 64-bit binary operations */
        if (ty_is_llong(n->ty) || (sema_is_cmp(n->op) && (ty_is_llong(n->lhs->ty) || ty_is_llong(n->rhs->ty)))) {
#ifdef S12CC_X64_HOST
        {
            int llong_cmp;
            llong_cmp = sema_is_cmp(n->op) && (ty_is_llong(n->lhs->ty) || ty_is_llong(n->rhs->ty));
            lv = hl_expr(n->lhs);
            rv = hl_expr(n->rhs);
            /* Widen non-llong operand to llong if the other is llong */
            if (ty_is_llong(n->lhs->ty) && !ty_is_llong(n->rhs->ty)) {
                if (n->rhs->ty & TY_UNSIGNED)
                    rv = hi_emit(HI_ZEXT32, TY_LLONG, rv, -1, 0, NULL);
                else
                    rv = hi_emit(HI_SEXT32, TY_LLONG, rv, -1, 0, NULL);
            }
            if (ty_is_llong(n->rhs->ty) && !ty_is_llong(n->lhs->ty)) {
                if (n->lhs->ty & TY_UNSIGNED)
                    lv = hi_emit(HI_ZEXT32, TY_LLONG, lv, -1, 0, NULL);
                else
                    lv = hi_emit(HI_SEXT32, TY_LLONG, lv, -1, 0, NULL);
            }
            if (n->op == TK_PLUS) return hi_emit(HI_ADD, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_MINUS) return hi_emit(HI_SUB, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_STAR) return hi_emit(HI_MUL, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_SLASH) {
                if (n->ty & TY_UNSIGNED) return hi_emit(HI_DIV, TY_LLONG | TY_UNSIGNED, lv, rv, 0, NULL);
                return hi_emit(HI_DIV, TY_LLONG, lv, rv, 0, NULL);
            }
            if (n->op == TK_PERCENT) {
                if (n->ty & TY_UNSIGNED) return hi_emit(HI_REM, TY_LLONG | TY_UNSIGNED, lv, rv, 0, NULL);
                return hi_emit(HI_REM, TY_LLONG, lv, rv, 0, NULL);
            }
            if (n->op == TK_AMP) return hi_emit(HI_AND, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_PIPE) return hi_emit(HI_OR, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_CARET) return hi_emit(HI_XOR, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_LSHIFT) return hi_emit(HI_SLL, TY_LLONG, lv, rv, 0, NULL);
            if (n->op == TK_RSHIFT) {
                if (n->ty & TY_UNSIGNED) return hi_emit(HI_SRL, TY_LLONG, lv, rv, 0, NULL);
                return hi_emit(HI_SRA, TY_LLONG, lv, rv, 0, NULL);
            }
            if (n->op == TK_EQ) return hi_emit(HI_SEQ, TY_INT, lv, rv, 0, NULL);
            if (n->op == TK_NE) return hi_emit(HI_SNE, TY_INT, lv, rv, 0, NULL);
            if (n->op == TK_LT) {
                if (llong_cmp && (n->lhs->ty & TY_UNSIGNED))
                    return hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                return hi_emit(HI_SLT, TY_INT, lv, rv, 0, NULL);
            }
            if (n->op == TK_GT) {
                if (llong_cmp && (n->lhs->ty & TY_UNSIGNED))
                    return hi_emit(HI_SGTU, TY_INT, lv, rv, 0, NULL);
                return hi_emit(HI_SGT, TY_INT, lv, rv, 0, NULL);
            }
            if (n->op == TK_LE) {
                if (llong_cmp && (n->lhs->ty & TY_UNSIGNED))
                    return hi_emit(HI_SLEU, TY_INT, lv, rv, 0, NULL);
                return hi_emit(HI_SLE, TY_INT, lv, rv, 0, NULL);
            }
            if (n->op == TK_GE) {
                if (llong_cmp && (n->lhs->ty & TY_UNSIGNED))
                    return hi_emit(HI_SGEU, TY_INT, lv, rv, 0, NULL);
                return hi_emit(HI_SGE, TY_INT, lv, rv, 0, NULL);
            }
        }
#endif
            int lv_hi;
            int rv_hi;
            int r_lo;
            int r_hi;
            int carry;
            int carg_base2;
            int eq_lo;
            int eq_hi;
            int hi_lt;
            int hi_eq;
            int lo_lt;
            int tmp2;

            lv = hl_expr(n->lhs);
            lv_hi = hl_hi;
            if (!ty_is_llong(n->lhs->ty)) hl_widen64(lv, n->lhs->ty, &lv, &lv_hi);
            rv = hl_expr(n->rhs);
            rv_hi = hl_hi;
            if (!ty_is_llong(n->rhs->ty)) hl_widen64(rv, n->rhs->ty, &rv, &rv_hi);

            if (n->op == TK_PLUS) {
                r_lo = hi_emit(HI_ADD, TY_INT, lv, rv, 0, NULL);
                carry = hi_emit(HI_SLTU, TY_INT, r_lo, lv, 0, NULL);
                r_hi = hi_emit(HI_ADD, TY_INT, lv_hi, rv_hi, 0, NULL);
                r_hi = hi_emit(HI_ADD, TY_INT, r_hi, carry, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            if (n->op == TK_MINUS) {
                carry = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                r_lo = hi_emit(HI_SUB, TY_INT, lv, rv, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, lv_hi, rv_hi, 0, NULL);
                r_hi = hi_emit(HI_SUB, TY_INT, r_hi, carry, 0, NULL);
                hl_hi = r_hi;
                return r_lo;
            }
            if (n->op == TK_AMP) {
                r_lo = hi_emit(HI_AND, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_AND, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_PIPE) {
                r_lo = hi_emit(HI_OR, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_OR, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_CARET) {
                r_lo = hi_emit(HI_XOR, TY_INT, lv, rv, 0, NULL);
                hl_hi = hi_emit(HI_XOR, TY_INT, lv_hi, rv_hi, 0, NULL);
                return r_lo;
            }
            /* Comparisons — result is TY_INT */
            if (n->op == TK_EQ) {
                eq_lo = hi_emit(HI_SEQ, TY_INT, lv, rv, 0, NULL);
                eq_hi = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                return hi_emit(HI_AND, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
            if (n->op == TK_NE) {
                eq_lo = hi_emit(HI_SNE, TY_INT, lv, rv, 0, NULL);
                eq_hi = hi_emit(HI_SNE, TY_INT, lv_hi, rv_hi, 0, NULL);
                return hi_emit(HI_OR, TY_INT, eq_lo, eq_hi, 0, NULL);
            }
            if (n->op == TK_LT) {
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                return hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
            }
            if (n->op == TK_GT) {
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, rv, lv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                return hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
            }
            if (n->op == TK_LE) {
                /* a <= b  is  !(b < a) */
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, rv_hi, lv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, rv, lv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                tmp2 = hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, tmp2, -1, 0, NULL);
            }
            if (n->op == TK_GE) {
                /* a >= b  is  !(a < b) */
                if (n->ty & TY_UNSIGNED) {
                    hi_lt = hi_emit(HI_SLTU, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                } else {
                    hi_lt = hi_emit(HI_SLT, TY_INT, lv_hi, rv_hi, 0, NULL);
                    hi_eq = hi_emit(HI_SEQ, TY_INT, lv_hi, rv_hi, 0, NULL);
                }
                lo_lt = hi_emit(HI_SLTU, TY_INT, lv, rv, 0, NULL);
                tmp2 = hi_emit(HI_AND, TY_INT, hi_eq, lo_lt, 0, NULL);
                tmp2 = hi_emit(HI_OR, TY_INT, hi_lt, tmp2, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, tmp2, -1, 0, NULL);
            }
            /* Mul/Div/Rem/Shifts — emit CALL to helper */
            if (n->op == TK_STAR) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__muldi3");
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_SLASH) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__udivdi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__divdi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_PERCENT) {
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__umoddi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__moddi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_LSHIFT) {
                /* Inline constant shifts, call helper for variable shifts */
                if (n->rhs->kind == ND_NUM && n->rhs->val >= 0 && n->rhs->val <= 63) {
                    int sn;
                    int sc;
                    sn = n->rhs->val;
                    if (sn == 0) {
                        hl_hi = lv_hi;
                        return lv;
                    }
                    if (sn < 32) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_lo = hi_emit(HI_SLL, TY_INT, lv, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_hi = hi_emit(HI_SLL, TY_INT, lv_hi, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 32 - sn, NULL);
                        tmp2 = hi_emit(HI_SRL, TY_INT, lv, sc, 0, NULL);
                        hl_hi = hi_emit(HI_OR, TY_INT, r_hi, tmp2, 0, NULL);
                        return r_lo;
                    }
                    if (sn == 32) {
                        hl_hi = lv;
                        return hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                    }
                    /* sn > 32 */
                    sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn - 32, NULL);
                    hl_hi = hi_emit(HI_SLL, TY_INT, lv, sc, 0, NULL);
                    return hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                }
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashldi3");
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            if (n->op == TK_RSHIFT) {
                /* Inline constant shifts, call helper for variable shifts */
                if (n->rhs->kind == ND_NUM && n->rhs->val >= 0 && n->rhs->val <= 63) {
                    int sn;
                    int sc;
                    int is_arith;
                    sn = n->rhs->val;
                    is_arith = !(n->ty & TY_UNSIGNED);
                    if (sn == 0) {
                        hl_hi = lv_hi;
                        return lv;
                    }
                    if (sn < 32) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        r_lo = hi_emit(HI_SRL, TY_INT, lv, sc, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 32 - sn, NULL);
                        tmp2 = hi_emit(HI_SLL, TY_INT, lv_hi, sc, 0, NULL);
                        r_lo = hi_emit(HI_OR, TY_INT, r_lo, tmp2, 0, NULL);
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn, NULL);
                        hl_hi = hi_emit(is_arith ? HI_SRA : HI_SRL, TY_INT,
                                        lv_hi, sc, 0, NULL);
                        return r_lo;
                    }
                    if (sn == 32) {
                        r_lo = lv_hi;
                        if (is_arith) {
                            sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
                            hl_hi = hi_emit(HI_SRA, TY_INT, lv_hi, sc, 0, NULL);
                        } else {
                            hl_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                        }
                        return r_lo;
                    }
                    /* sn > 32 */
                    sc = hi_emit(HI_ICONST, TY_INT, -1, -1, sn - 32, NULL);
                    r_lo = hi_emit(is_arith ? HI_SRA : HI_SRL, TY_INT,
                                   lv_hi, sc, 0, NULL);
                    if (is_arith) {
                        sc = hi_emit(HI_ICONST, TY_INT, -1, -1, 31, NULL);
                        hl_hi = hi_emit(HI_SRA, TY_INT, lv_hi, sc, 0, NULL);
                    } else {
                        hl_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                    }
                    return r_lo;
                }
                carg_base2 = h_ncarg;
                h_carg[h_ncarg] = lv;      h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi;   h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
                if (n->ty & TY_UNSIGNED) {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__lshrdi3");
                } else {
                    r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 3, "__ashrdi3");
                }
                h_cbase[r_lo] = carg_base2;
                hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
                return r_lo;
            }
            /* Logical && || handled elsewhere */
        }

        /* Pointer difference: (a - b) / elem_sz.  Scaling b like an
         * index multiplied one POINTER by the element size — doom's
         * `lump_p - lumpinfo` returned a huge negative "index" and
         * every W_GetNumForName bombed as not-found. */
        if (n->op == TK_MINUS && ty_is_ptr(n->lhs->ty) && ty_is_ptr(n->rhs->ty)) {
            int diff;
            elem_sz = ty_size(ty_deref(n->lhs->ty));
            lv = hl_expr(n->lhs);
            rv = hl_expr(n->rhs);
            diff = hi_emit(HI_SUB, TY_INT, lv, rv, 0, NULL);
            if (elem_sz > 1) {
                scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
                diff = hi_emit(HI_DIV, TY_INT, diff, scale, 0, NULL);
            }
            return diff;
        }

        /* Pointer arithmetic: scale the integer operand */
        if ((n->op == TK_PLUS || n->op == TK_MINUS) && ty_is_ptr(n->lhs->ty)) {
            elem_sz = ty_size(ty_deref(n->lhs->ty));
            lv = hl_expr(n->lhs);
            rv = hl_expr(n->rhs);
            if (elem_sz > 1) {
                scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
                rv = hi_emit(HI_MUL, TY_INT, rv, scale, 0, NULL);
            }
            if (n->op == TK_PLUS) {
                return hi_emit(HI_ADD, n->ty, lv, rv, 0, NULL);
            }
            return hi_emit(HI_SUB, n->ty, lv, rv, 0, NULL);
        }

        /* Floating-point binary operations (f64).
         *
         * Slow32 / x64 hosts use libcalls with int-pair representation
         * (lo, hi).  S12CC_NATIVE_F64 hosts (a64) emit native HI_F* with
         * TY_DOUBLE — single V-class value, no pair packing. */
        if (ty_is_double(n->lhs->ty) || ty_is_double(n->rhs->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Both operands as single TY_DOUBLE values; promote
             * non-double operands via HI_FCVT_*toD. */
            int lvn; int rvn;
            lvn = hl_expr(n->lhs);
            if (!ty_is_double(n->lhs->ty)) {
                if (ty_is_float(n->lhs->ty)) {
                    lvn = hi_emit(HI_FCVT_FtoD, TY_DOUBLE, lvn, -1, 0, NULL);
                } else {
                    /* int → double */
                    lvn = hi_emit(HI_FCVT_ItoF, TY_DOUBLE, lvn, -1, 0, NULL);
                }
            }
            rvn = hl_expr(n->rhs);
            if (!ty_is_double(n->rhs->ty)) {
                if (ty_is_float(n->rhs->ty)) {
                    rvn = hi_emit(HI_FCVT_FtoD, TY_DOUBLE, rvn, -1, 0, NULL);
                } else {
                    rvn = hi_emit(HI_FCVT_ItoF, TY_DOUBLE, rvn, -1, 0, NULL);
                }
            }
            if (n->op == TK_PLUS)  return hi_emit(HI_FADD, TY_DOUBLE, lvn, rvn, 0, NULL);
            if (n->op == TK_MINUS) return hi_emit(HI_FSUB, TY_DOUBLE, lvn, rvn, 0, NULL);
            if (n->op == TK_STAR)  return hi_emit(HI_FMUL, TY_DOUBLE, lvn, rvn, 0, NULL);
            if (n->op == TK_SLASH) return hi_emit(HI_FDIV, TY_DOUBLE, lvn, rvn, 0, NULL);
            if (n->op == TK_EQ)    return hi_emit(HI_FEQ, TY_INT, lvn, rvn, 0, NULL);
            if (n->op == TK_NE) {
                int v;
                v = hi_emit(HI_FEQ, TY_INT, lvn, rvn, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, v, -1, 0, NULL);
            }
            if (n->op == TK_LT) return hi_emit(HI_FLT, TY_INT, lvn, rvn, 0, NULL);
            if (n->op == TK_LE) return hi_emit(HI_FLE, TY_INT, lvn, rvn, 0, NULL);
            if (n->op == TK_GT) return hi_emit(HI_FLT, TY_INT, rvn, lvn, 0, NULL);
            if (n->op == TK_GE) return hi_emit(HI_FLE, TY_INT, rvn, lvn, 0, NULL);
            /* Fallthrough: shouldn't reach here for valid ops. */
#else
            int lv_hi;
            int rv_hi;
            int cb;
            int r;
            /* Evaluate and promote left operand to double */
            lv = hl_expr(n->lhs);
            if (ty_is_double(n->lhs->ty)) {
                lv_hi = hl_hi;
            } else {
                lv = hl_promote_to_f64(lv, n->lhs->ty);
                lv_hi = hl_hi;
            }
            /* Evaluate and promote right operand to double */
            rv = hl_expr(n->rhs);
            if (ty_is_double(n->rhs->ty)) {
                rv_hi = hl_hi;
            } else {
                rv = hl_promote_to_f64(rv, n->rhs->ty);
                rv_hi = hl_hi;
            }
            /* Comparisons: result is single int, no CALLHI */
            if (n->op == TK_EQ || n->op == TK_NE ||
                n->op == TK_LT || n->op == TK_LE ||
                n->op == TK_GT || n->op == TK_GE) {
                if (n->op == TK_GT || n->op == TK_GE) {
                    /* Swap operands: GT→LT, GE→LE */
                    cb = h_ncarg;
                    h_carg[h_ncarg] = rv; h_ncarg = h_ncarg + 1;
                    h_carg[h_ncarg] = rv_hi; h_ncarg = h_ncarg + 1;
                    h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
                    h_carg[h_ncarg] = lv_hi; h_ncarg = h_ncarg + 1;
                    if (n->op == TK_GT) {
                        r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_lt");
                    } else {
                        r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_le");
                    }
                    h_cbase[r] = cb;
                    return r;
                }
                cb = h_ncarg;
                h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = lv_hi; h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv; h_ncarg = h_ncarg + 1;
                h_carg[h_ncarg] = rv_hi; h_ncarg = h_ncarg + 1;
                if (n->op == TK_LT) {
                    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_lt");
                } else if (n->op == TK_LE) {
                    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_le");
                } else {
                    r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_eq");
                }
                h_cbase[r] = cb;
                if (n->op == TK_NE) {
                    return hi_emit(HI_NOT, TY_INT, r, -1, 0, NULL);
                }
                return r;
            }
            /* Arithmetic: result is double pair */
            cb = h_ncarg;
            h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = lv_hi; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = rv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = rv_hi; h_ncarg = h_ncarg + 1;
            if (n->op == TK_PLUS)  r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_add");
            else if (n->op == TK_MINUS) r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_sub");
            else if (n->op == TK_STAR)  r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_mul");
            else r = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_div");
            h_cbase[r] = cb;
            hl_hi = hi_emit(HI_CALLHI, TY_INT, r, -1, 0, NULL);
            return r;
#endif
        }

        /* Floating-point binary operations (f32) */
        if (ty_is_float(n->lhs->ty) || ty_is_float(n->rhs->ty)) {
            lv = hl_expr(n->lhs);
            rv = hl_expr(n->rhs);
            if (n->op == TK_PLUS)  return hi_emit(HI_FADD, TY_FLOAT, lv, rv, 0, NULL);
            if (n->op == TK_MINUS) return hi_emit(HI_FSUB, TY_FLOAT, lv, rv, 0, NULL);
            if (n->op == TK_STAR)  return hi_emit(HI_FMUL, TY_FLOAT, lv, rv, 0, NULL);
            if (n->op == TK_SLASH) return hi_emit(HI_FDIV, TY_FLOAT, lv, rv, 0, NULL);
            if (n->op == TK_EQ)    return hi_emit(HI_FEQ, TY_INT, lv, rv, 0, NULL);
            if (n->op == TK_NE) {
                val = hi_emit(HI_FEQ, TY_INT, lv, rv, 0, NULL);
                return hi_emit(HI_NOT, TY_INT, val, -1, 0, NULL);
            }
            if (n->op == TK_LT) return hi_emit(HI_FLT, TY_INT, lv, rv, 0, NULL);
            if (n->op == TK_LE) return hi_emit(HI_FLE, TY_INT, lv, rv, 0, NULL);
            if (n->op == TK_GT) return hi_emit(HI_FLT, TY_INT, rv, lv, 0, NULL);
            if (n->op == TK_GE) return hi_emit(HI_FLE, TY_INT, rv, lv, 0, NULL);
        }

        /* Regular binary op */
        lv = hl_expr(n->lhs);
        rv = hl_expr(n->rhs);
        kind = hl_binop_kind(n->op, n->ty);
#ifndef S12CC_X64_HOST
        /* SLOW-32 has only signed DIV/REM instructions; unsigned goes
         * through the libs32 libcalls.  Cross hosts divide natively. */
        if ((kind == HI_DIV || kind == HI_REM) && (n->ty & TY_UNSIGNED) &&
            !ty_is_llong(n->ty)) {
            return hl_udivmod32(lv, rv, kind == HI_REM);
        }
#endif
        return hi_emit(kind, n->ty, lv, rv, 0, NULL);
    }

    /* Compound assignment (+=, -=, etc.) */
    if (n->kind == ND_COMP_ASSIGN) {
        /* Bit-field LHS: extract old, apply op, RMW the unit. */
        if (n->lhs->kind == ND_MEMBER && n->lhs->bit_width > 0) {
            int base_addr;
            int unit_ty;
            int unsigned_bf;
            int unit_val;
            int rhs_val;
            int new_val;
            int kop;
            base_addr = hl_bf_unit_addr(n->lhs);
            unit_ty = hl_bf_unit_ty(n->lhs->ty);
            unsigned_bf = (n->lhs->ty & TY_UNSIGNED) != 0;
            unit_val = hi_emit(HI_LOAD, unit_ty, base_addr, -1, 0, NULL);
            old_val = hl_bf_extract(unit_val, n->lhs->bit_width,
                                    n->lhs->bit_off, unsigned_bf);
            rhs_val = hl_expr(n->rhs);
            kop = hl_binop_kind(n->op, n->lhs->ty);
            new_val = hi_emit(kop, TY_INT, old_val, rhs_val, 0, NULL);
            hl_bf_store(base_addr, unit_ty, n->lhs->bit_width,
                        n->lhs->bit_off, new_val);
            return new_val;
        }
        addr = hl_addr(n->lhs);
        old_val = hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
        rv = hl_expr(n->rhs);
        /* Scale for pointer arithmetic */
        if (ty_is_ptr(n->ty) && (n->op == TK_PLUS || n->op == TK_MINUS)) {
            elem_sz = ty_size(ty_deref(n->ty));
            if (elem_sz > 1) {
                scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
                rv = hi_emit(HI_MUL, TY_INT, rv, scale, 0, NULL);
            }
        }
        /* Float / double LHS: promote the rhs to the LHS's FP type and
         * emit HI_F*.  hl_binop_kind below would return integer HI_ADD
         * etc., which is wrong for FP destinations. */
#ifndef S12CC_NATIVE_F64
        if (ty_is_double(n->ty)) {
            /* Soft-pair f64: old_val above loaded only the LO word.
             * Load the pair, promote the rhs, libcall, store the pair.
             * (HI_FADD w/ TY_DOUBLE has no slow32 BURG pattern — the
             * old code here silently dropped the op: `d += step` was a
             * no-op and every sbasic FOR loop spun forever.) */
            int a4;
            int old_hi;
            int rv_hi;
            int cb;
            int r_lo;
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            old_hi = hi_emit(HI_LOAD, TY_INT, a4, -1, 0, NULL);
            if (ty_is_double(n->rhs->ty)) {
                rv_hi = hl_hi;
            } else {
                rv = hl_promote_to_f64(rv, n->rhs->ty);
                rv_hi = hl_hi;
            }
            cb = h_ncarg;
            h_carg[h_ncarg] = old_val; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = old_hi;  h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = rv;      h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = rv_hi;   h_ncarg = h_ncarg + 1;
            if (n->op == TK_PLUS)
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_add");
            else if (n->op == TK_MINUS)
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_sub");
            else if (n->op == TK_STAR)
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_mul");
            else
                r_lo = hi_emit(HI_CALL, TY_INT, -1, -1, 4, "__fp64_div");
            h_cbase[r_lo] = cb;
            hl_hi = hi_emit(HI_CALLHI, TY_INT, r_lo, -1, 0, NULL);
            hi_emit(HI_STORE, TY_INT, addr, r_lo, 0, NULL);
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, a4, hl_hi, 0, NULL);
            return r_lo;
        }
#endif
        if (ty_is_float(n->ty) || ty_is_double(n->ty)) {
            int rhs_ty;
            int fkind;
            rhs_ty = n->rhs->ty;
            if (ty_is_double(n->ty) && !ty_is_double(rhs_ty)) {
                if (ty_is_float(rhs_ty))
                    rv = hi_emit(HI_FCVT_FtoD, TY_DOUBLE, rv, -1, 0, NULL);
                else
                    rv = hi_emit(HI_FCVT_ItoF, TY_DOUBLE, rv, -1, 0, NULL);
            } else if (ty_is_float(n->ty) && !ty_is_fp(rhs_ty)) {
                rv = hi_emit(HI_FCVT_ItoF, TY_FLOAT, rv, -1, 0, NULL);
            }
            if (n->op == TK_PLUS)       fkind = HI_FADD;
            else if (n->op == TK_MINUS) fkind = HI_FSUB;
            else if (n->op == TK_STAR)  fkind = HI_FMUL;
            else                         fkind = HI_FDIV;  /* TK_SLASH */
            new_val = hi_emit(fkind, n->ty, old_val, rv, 0, NULL);
            hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
            return new_val;
        }
#ifndef S12CC_X64_HOST
        if (ty_is_llong(n->ty)) {
            /* Pair RMW: the scalar tail below is pair-blind (loads,
             * ops, and stores only the LO word). */
            int a4;
            int old_lo;
            int old_hi;
            int rv_hi2;
            old_lo = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            old_hi = hi_emit(HI_LOAD, TY_INT, a4, -1, 0, NULL);
            if (ty_is_llong(n->rhs->ty)) {
                rv_hi2 = hl_hi;
            } else {
                hl_widen64(rv, n->rhs->ty, &rv, &rv_hi2);
            }
            new_val = hl_ll_op(n->op, (n->ty & TY_UNSIGNED) != 0,
                               old_lo, old_hi, rv, rv_hi2);
            hi_emit(HI_STORE, TY_INT, addr, new_val, 0, NULL);
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, a4, hl_hi, 0, NULL);
            return new_val;
        }
#endif
        kind = hl_binop_kind(n->op, n->ty);
#ifndef S12CC_X64_HOST
        if ((kind == HI_DIV || kind == HI_REM) && (n->ty & TY_UNSIGNED) &&
            !ty_is_llong(n->ty)) {
            new_val = hl_udivmod32(old_val, rv, kind == HI_REM);
        } else {
            new_val = hi_emit(kind, n->ty, old_val, rv, 0, NULL);
        }
#else
        new_val = hi_emit(kind, n->ty, old_val, rv, 0, NULL);
#endif
        hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
        return new_val;
    }

    /* Postfix ++ / -- */
    if (n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        /* Bit-field LHS: extract old, +/-1, RMW unit, return old. */
        if (n->lhs->kind == ND_MEMBER && n->lhs->bit_width > 0) {
            int base_addr;
            int unit_ty;
            int unsigned_bf;
            int unit_val;
            int one;
            int new_val;
            base_addr = hl_bf_unit_addr(n->lhs);
            unit_ty = hl_bf_unit_ty(n->lhs->ty);
            unsigned_bf = (n->lhs->ty & TY_UNSIGNED) != 0;
            unit_val = hi_emit(HI_LOAD, unit_ty, base_addr, -1, 0, NULL);
            old_val = hl_bf_extract(unit_val, n->lhs->bit_width,
                                    n->lhs->bit_off, unsigned_bf);
            one = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
            if (n->kind == ND_POST_INC) {
                new_val = hi_emit(HI_ADD, TY_INT, old_val, one, 0, NULL);
            } else {
                new_val = hi_emit(HI_SUB, TY_INT, old_val, one, 0, NULL);
            }
            hl_bf_store(base_addr, unit_ty, n->lhs->bit_width,
                        n->lhs->bit_off, new_val);
            return old_val;
        }
        addr = hl_addr(n->lhs);
        old_val = hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
#ifndef S12CC_X64_HOST
        if (ty_is_llong(n->ty)) {
            /* Pair RMW: carry/borrow across both words; the scalar
             * path below only touches the LO word. */
            int a4;
            int old_hi;
            int one;
            int zero_hi;
            int nl;
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            old_hi = hi_emit(HI_LOAD, TY_INT, a4, -1, 0, NULL);
            one = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
            zero_hi = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            nl = hl_ll_op((n->kind == ND_POST_INC) ? TK_PLUS : TK_MINUS, 0,
                          old_val, old_hi, one, zero_hi);
            hi_emit(HI_STORE, TY_INT, addr, nl, 0, NULL);
            a4 = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, a4, hl_hi, 0, NULL);
            hl_hi = old_hi;
            return old_val;
        }
#endif
        /* Float / double: scale is a 1.0 of the right FP type, op is
         * HI_FADD / HI_FSUB. */
        if (ty_is_float(n->ty) || ty_is_double(n->ty)) {
            int one_int;
            int one_fp;
            int fkind;
            one_int = hi_emit(HI_ICONST, TY_INT, -1, -1, 1, NULL);
            if (ty_is_double(n->ty))
                one_fp = hi_emit(HI_FCVT_ItoF, TY_DOUBLE, one_int, -1, 0, NULL);
            else
                one_fp = hi_emit(HI_FCVT_ItoF, TY_FLOAT, one_int, -1, 0, NULL);
            fkind = (n->kind == ND_POST_INC) ? HI_FADD : HI_FSUB;
            new_val = hi_emit(fkind, n->ty, old_val, one_fp, 0, NULL);
            hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
            return old_val;
        }
        if (ty_is_ptr(n->ty)) {
            elem_sz = ty_size(ty_deref(n->ty));
        } else {
            elem_sz = 1;
        }
        scale = hi_emit(HI_ICONST, TY_INT, -1, -1, elem_sz, NULL);
        if (n->kind == ND_POST_INC) {
            new_val = hi_emit(HI_ADD, n->ty, old_val, scale, 0, NULL);
        } else {
            new_val = hi_emit(HI_SUB, n->ty, old_val, scale, 0, NULL);
        }
        hi_emit(HI_STORE, n->ty, addr, new_val, 0, NULL);
        return old_val;
    }

    /* Ternary c ? a : b */
    if (n->kind == ND_TERNARY) {
        /* Peephole: when both arms are the integer constants {0,1} and the
         * condition is a plain 32-bit integer, the result is just a
         * normalized boolean of the condition.  Building the control-flow
         * diamond + phi (below) for this turns a single SETcc/CSET into a
         * branch plus a redundant store/load/phi.  The SLOW-32 emulator's
         * comparison handlers (h_slt / h_seq / h_sgt / ... are all
         * `cond ? 1 : 0`) are the hot case.  Evaluate the condition once —
         * exactly as the diamond would — and fold:
         *     c ? 1 : 0  ==  (c != 0)   [== c itself when c is a compare]
         *     c ? 0 : 1  ==  (c == 0)
         * A comparison result has type TY_INT, so it satisfies the
         * "simple integer condition" guard; pointer / long long / fp
         * conditions fall through to the diamond unchanged.  This lives in
         * the shared lowering, so x64 / a64 / slow32 all benefit. */
        if (n->lhs->kind == ND_NUM && n->rhs->kind == ND_NUM &&
            n->lhs->val_hi == 0 && n->rhs->val_hi == 0 &&
            ((n->lhs->val == 1 && n->rhs->val == 0) ||
             (n->lhs->val == 0 && n->rhs->val == 1)) &&
            !ty_is_llong(n->cond->ty) && !ty_is_fp(n->cond->ty) &&
            !ty_is_ptr(n->cond->ty)) {
            int ck;
            int zero;
            cv = hl_cond_val(n->cond);
            ck = h_kind[cv];
            if (n->lhs->val == 1) {
                /* c ? 1 : 0 */
                if (ck >= HI_SEQ && ck <= HI_SGEU) return cv;
                zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
                return hi_emit(HI_SNE, TY_INT, cv, zero, 0, NULL);
            }
            /* c ? 0 : 1 — logical NOT of the condition */
            zero = hi_emit(HI_ICONST, TY_INT, -1, -1, 0, NULL);
            return hi_emit(HI_SEQ, TY_INT, cv, zero, 0, NULL);
        }
        tmp_off = hl_alloc_temp();
        tmp = hl_emit_temp_alloca(n->ty, tmp_off);
        cv = hl_cond_val(n->cond);
        then_blk = hir_new_block();
        else_blk = hir_new_block();
        join_blk = hir_new_block();
        hi_emit(HI_BRC, 0, cv, then_blk, else_blk, NULL);

#ifndef S12CC_X64_HOST
        if (ty_is_llong(n->ty) || ty_is_double(n->ty)) {
            /* Pair-typed result: both words must flow through the temp.
             * Storing only the lo word leaves the join reading whatever
             * hl_hi last held — an arm that never executed. */
            int t4;

            hl_switch_block(then_blk);
            val = hl_expr(n->lhs);
            if (!ty_is_llong(n->lhs->ty) && !ty_is_double(n->lhs->ty)) {
                if (ty_is_double(n->ty)) {
                    /* result is double: CONVERT the int/float arm (sema
                     * doesn't insert arm casts) */
                    val = hl_promote_to_f64(val, n->lhs->ty);
                } else {
                    hl_widen64(val, n->lhs->ty, &val, &hl_hi);
                }
            }
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            t4 = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, t4, hl_hi, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(else_blk);
            val = hl_expr(n->rhs);
            if (!ty_is_llong(n->rhs->ty) && !ty_is_double(n->rhs->ty)) {
                if (ty_is_double(n->ty)) {
                    val = hl_promote_to_f64(val, n->rhs->ty);
                } else {
                    hl_widen64(val, n->rhs->ty, &val, &hl_hi);
                }
            }
            hi_emit(HI_STORE, TY_INT, tmp, val, 0, NULL);
            t4 = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, 4, NULL);
            hi_emit(HI_STORE, TY_INT, t4, hl_hi, 0, NULL);
            hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

            hl_switch_block(join_blk);
            val = hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
            t4 = hi_emit(HI_ADDI, HL_ADDR_TY, tmp, -1, 4, NULL);
            hl_hi = hi_emit(HI_LOAD, TY_INT, t4, -1, 0, NULL);
            return val;
        }
#endif

        hl_switch_block(then_blk);
        val = hl_expr(n->lhs);
        hi_emit(HI_STORE, n->ty, tmp, val, 0, NULL);
        hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

        hl_switch_block(else_blk);
        val = hl_expr(n->rhs);
        hi_emit(HI_STORE, n->ty, tmp, val, 0, NULL);
        hi_emit(HI_BR, 0, -1, -1, join_blk, NULL);

        hl_switch_block(join_blk);
        return hi_emit(HI_LOAD, n->ty, tmp, -1, 0, NULL);
    }

    /* Type cast */
    if (n->kind == ND_CAST) {
        int src_is_native64;
        int dst_is_native64;
        lv = hl_expr(n->lhs);
        /* On 64-bit hosts (cc-x64 / cc-a64) pointers are already 64-bit
         * register values.  A `(long long)ptr` or `(uintptr_t)ptr` cast
         * must NOT sign- or zero-extend from 32 bits — it's a no-op.
         * Same in reverse: `(char *)long_long_value` must not truncate
         * to 32 bits. */
        src_is_native64 = ty_is_llong(n->lhs->ty) ||
                          (ty_is_ptr(n->lhs->ty) && ty_ptr_size == 8);
        dst_is_native64 = ty_is_llong(n->ty) ||
                          (ty_is_ptr(n->ty) && ty_ptr_size == 8);
#ifdef S12CC_X64_HOST
        if (dst_is_native64 && !src_is_native64 && !ty_is_double(n->lhs->ty)) {
            /* x64: explicit widening via SEXT32/ZEXT32 (optimizer won't fold) */
            if (n->lhs->ty & TY_UNSIGNED)
                return hi_emit(HI_ZEXT32, TY_LLONG, lv, -1, 0, NULL);
            else
                return hi_emit(HI_SEXT32, TY_LLONG, lv, -1, 0, NULL);
        }
#endif
        if (dst_is_native64 && !src_is_native64 && !ty_is_double(n->lhs->ty)) {
            /* Widen 32->64 */
            hl_widen64(lv, n->lhs->ty, &lv, &hl_hi);
            return lv;
        }
        if (!dst_is_native64 && !ty_is_double(n->ty) && src_is_native64) {
            /* Truncate 64->32: just use lo word */
            return lv;
        }
        if (dst_is_native64 && src_is_native64) {
            /* Both 64-bit (pointer ↔ long long, or pointer ↔ pointer):
             * the value is already a 64-bit register, no conversion. */
            return lv;
        }
        /* int → float */
        if (ty_is_float(n->ty) && !ty_is_fp(n->lhs->ty) && !ty_is_llong(n->lhs->ty)) {
            return hi_emit(HI_FCVT_ItoF, TY_FLOAT, lv, -1, 0, NULL);
        }
        /* float → int.  Pass through the destination type (n->ty) so the
         * codegen can pick fcvtzs vs fcvtzu by inspecting TY_UNSIGNED. */
        if (!ty_is_fp(n->ty) && !ty_is_llong(n->ty) && ty_is_float(n->lhs->ty)) {
            return hi_emit(HI_FCVT_FtoI, n->ty, lv, -1, 0, NULL);
        }
        /* int → double */
        if (ty_is_double(n->ty) && !ty_is_fp(n->lhs->ty) && !ty_is_llong(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: HI_FCVT_ItoF with TY_DOUBLE — codegen picks
             * scvtf sf=0 type=1 by inspecting src/dst types. */
            return hi_emit(HI_FCVT_ItoF, TY_DOUBLE, lv, -1, 0, NULL);
#else
            return hl_promote_to_f64(lv, n->lhs->ty);
#endif
        }
        /* float → double */
        if (ty_is_double(n->ty) && ty_is_float(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            return hi_emit(HI_FCVT_FtoD, TY_DOUBLE, lv, -1, 0, NULL);
#else
            return hl_promote_to_f64(lv, n->lhs->ty);
#endif
        }
        /* double → int */
        if (!ty_is_fp(n->ty) && !ty_is_llong(n->ty) && ty_is_double(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: HI_FCVT_FtoI; codegen picks fcvtzs/fcvtzu with
             * sf=0 (W-form) type=1 (D-form) by inspecting src/dst. */
            return hi_emit(HI_FCVT_FtoI, n->ty, lv, -1, 0, NULL);
#else
            int d_hi;
            int cb;
            d_hi = hl_hi;
            cb = h_ncarg;
            h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
            lv = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_cvt_DtoI");
            h_cbase[lv] = cb;
            return lv;
#endif
        }
        /* double → float */
        if (ty_is_float(n->ty) && ty_is_double(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            return hi_emit(HI_FCVT_DtoF, TY_FLOAT, lv, -1, 0, NULL);
#else
            int d_hi;
            int cb;
            d_hi = hl_hi;
            cb = h_ncarg;
            h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
            lv = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_cvt_DtoF");
            h_cbase[lv] = cb;
            return lv;
#endif
        }
        /* llong → double */
        if (ty_is_double(n->ty) && ty_is_llong(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: HI_FCVT_ItoF with TY_DOUBLE; codegen picks sf=1
             * by seeing TY_LLONG on src. */
            return hi_emit(HI_FCVT_ItoF, TY_DOUBLE, lv, -1, 0, NULL);
#else
            int d_hi;
            int cb;
            d_hi = hl_hi;
            cb = h_ncarg;
            h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
            lv = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_cvt_ltoD");
            h_cbase[lv] = cb;
            hl_hi = hi_emit(HI_CALLHI, TY_INT, lv, -1, 0, NULL);
            return lv;
#endif
        }
        /* double → llong */
        if (ty_is_llong(n->ty) && ty_is_double(n->lhs->ty)) {
#ifdef S12CC_NATIVE_F64
            /* Native: HI_FCVT_FtoI with TY_LLONG dst; codegen picks
             * sf=1 (X-form) type=1 (D-form). */
            return hi_emit(HI_FCVT_FtoI, n->ty, lv, -1, 0, NULL);
#else
            int d_hi;
            int cb;
            d_hi = hl_hi;
            cb = h_ncarg;
            h_carg[h_ncarg] = lv; h_ncarg = h_ncarg + 1;
            h_carg[h_ncarg] = d_hi; h_ncarg = h_ncarg + 1;
            lv = hi_emit(HI_CALL, TY_INT, -1, -1, 2, "__fp64_cvt_DtoL");
            h_cbase[lv] = cb;
            hl_hi = hi_emit(HI_CALLHI, TY_INT, lv, -1, 0, NULL);
            return lv;
#endif
        }
        /* Narrowing to char: truncate + sign/zero extend */
        if (!ty_is_ptr(n->ty) && (n->ty & TY_BASE_MASK) == TY_CHAR) {
            if (n->ty & TY_UNSIGNED) {
                tmp = hi_emit(HI_ICONST, TY_INT, -1, -1, 255, NULL);
                return hi_emit(HI_AND, TY_INT, lv, tmp, 0, NULL);
            } else {
                tmp = hi_emit(HI_ICONST, TY_INT, -1, -1, 24, NULL);
                lv = hi_emit(HI_SLL, TY_INT, lv, tmp, 0, NULL);
                return hi_emit(HI_SRA, TY_INT, lv, tmp, 0, NULL);
            }
        }
        /* Narrowing to short */
        if (!ty_is_ptr(n->ty) && (n->ty & TY_BASE_MASK) == TY_SHORT) {
            if (n->ty & TY_UNSIGNED) {
                tmp = hi_emit(HI_ICONST, TY_INT, -1, -1, 65535, NULL);
                return hi_emit(HI_AND, TY_INT, lv, tmp, 0, NULL);
            } else {
                tmp = hi_emit(HI_ICONST, TY_INT, -1, -1, 16, NULL);
                lv = hi_emit(HI_SLL, TY_INT, lv, tmp, 0, NULL);
                return hi_emit(HI_SRA, TY_INT, lv, tmp, 0, NULL);
            }
        }
        return lv;
    }

    /* Comma operator */
    if (n->kind == ND_COMMA) {
        hl_expr(n->lhs);
        return hl_expr(n->rhs);
    }

    /* GNU statement expression: lower the body's side effects, then
     * return the value of the trailing expression.  The block was
     * parsed in its own scope so locals declared inside don't leak;
     * their stack offsets are already baked into the captured lhs. */
    if (n->kind == ND_STMT_EXPR) {
        if (n->body) hl_stmt(n->body);
        return hl_expr(n->lhs);
    }

    /* Member access */
    if (n->kind == ND_MEMBER) {
        if (n->bit_width > 0) {
            int base_addr;
            int unit_ty;
            int unsigned_bf;
            base_addr = hl_bf_unit_addr(n);
            unit_ty = hl_bf_unit_ty(n->ty);
            unsigned_bf = (n->ty & TY_UNSIGNED) != 0;
            return hl_bf_load(base_addr, unit_ty, n->bit_width, n->bit_off,
                              unsigned_bf);
        }
        addr = hl_addr(n);
        if (ty_is_struct(n->ty) || n->is_array) return addr;
#ifdef S12CC_X64_HOST
        if (ty_is_llong(n->ty))
            return hi_emit(HI_LOAD, TY_LLONG, addr, -1, 0, NULL);
#endif
#ifdef S12CC_NATIVE_F64
        if (ty_is_double(n->ty))
            return hi_emit(HI_LOAD, TY_DOUBLE, addr, -1, 0, NULL);
#endif
        if (ty_is_double(n->ty) || ty_is_llong(n->ty)) {
            lv = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
            tmp = hi_emit(HI_ADDI, HL_ADDR_TY, addr, -1, 4, NULL);
            hl_hi = hi_emit(HI_LOAD, TY_INT, tmp, -1, 0, NULL);
            return lv;
        }
        return hi_emit(HI_LOAD, n->ty, addr, -1, 0, NULL);
    }

    /* va_start / va_arg */
#ifdef S12CC_X64_HOST
    /* x64: VA_START produces the initial tagged pointer; VA_ARG extracts
     * a value; VA_NEXT advances the pointer.  The va_list variable goes
     * through explicit STORE/LOAD so SSA can promote it. */
    if (n->kind == ND_VA_START) {
        addr = hl_addr(n->lhs);
        val = hi_emit(HI_VA_START, TY_PTR, -1, -1, hl_nparams, NULL);
        hi_emit(HI_STORE, TY_PTR, addr, val, 0, NULL);
        return val;
    }
    if (n->kind == ND_VA_ARG) {
        int cur_ap;
        int arg_val;
        int next_ap;
        addr = hl_addr(n->lhs);
        cur_ap = hi_emit(HI_LOAD, TY_PTR, addr, -1, 0, NULL);
        arg_val = hi_emit(HI_VA_ARG, n->ty, cur_ap, -1, 0, NULL);
        next_ap = hi_emit(HI_VA_NEXT, TY_PTR, cur_ap, -1, 0, NULL);
        hi_emit(HI_STORE, TY_PTR, addr, next_ap, 0, NULL);
        return arg_val;
    }
#else
    /* SLOW-32: va_start(ap, last) → ap = __getfp() */
    if (n->kind == ND_VA_START) {
        addr = hl_addr(n->lhs);
        val = hi_emit(HI_GETFP, TY_INT, -1, -1, 0, NULL);
        hi_emit(HI_STORE, TY_INT, addr, val, 0, NULL);
        return val;
    }

    /* SLOW-32: va_arg(ap, type) → val = *ap; ap += 4; return val */
    if (n->kind == ND_VA_ARG) {
        int ap_val;
        int new_ap;
        addr = hl_addr(n->lhs);
        ap_val = hi_emit(HI_LOAD, TY_INT, addr, -1, 0, NULL);
        if (ty_is_llong(n->ty) || ty_is_double(n->ty)) {
            /* 64-bit: load lo and hi words, advance ap by 8 */
            int lo;
            int ap4;
            int hi;
            lo = hi_emit(HI_LOAD, TY_INT, ap_val, -1, 0, NULL);
            ap4 = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 4, NULL);
            hi = hi_emit(HI_LOAD, TY_INT, ap4, -1, 0, NULL);
            new_ap = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 8, NULL);
            hi_emit(HI_STORE, TY_INT, addr, new_ap, 0, NULL);
            hl_hi = hi;
            return lo;
        }
        val = hi_emit(HI_LOAD, n->ty, ap_val, -1, 0, NULL);
        new_ap = hi_emit(HI_ADDI, TY_INT, ap_val, -1, 4, NULL);
        hi_emit(HI_STORE, TY_INT, addr, new_ap, 0, NULL);
        return val;
    }
#endif

    /* Direct function call */
    if (n->kind == ND_CALL) {
        int av[32];
        int av_hi[32];
        {
            Node *inl_fn;
            inl_fn = hl_inl_candidate(n);
            if (inl_fn) return hl_inline_call(n, inl_fn);
        }
        int is64[32];   /* 0 = word, 1 = llong pair, 2 = double pair.
                           (No extra arrays: hl_expr's frame is near
                           stage07's 12-bit offset ceiling.) */
        int callee_var;
        int phys_count;
        int ret_ty;

#ifdef S12CC_NATIVE_F64
        /* Recognise __builtin_sqrt / __builtin_sqrtf and emit HI_FSQRT
         * directly.  Lets libc_a64/math_stubs.c self-compile and drop
         * the host-gcc step. */
        if (n->name) {
            int is_sqrtd; int is_sqrtf;
            is_sqrtd = (strcmp(n->name, "__builtin_sqrt") == 0);
            is_sqrtf = (strcmp(n->name, "__builtin_sqrtf") == 0);
            if ((is_sqrtd || is_sqrtf) && n->args && !n->args->next) {
                int sv;
                sv = hl_expr(n->args);
                return hi_emit(HI_FSQRT,
                               is_sqrtd ? TY_DOUBLE : TY_FLOAT,
                               sv, -1, 0, NULL);
            }
        }
#endif

        /* Lower all arguments first (inner calls may use h_carg) */
        a = n->args;
        nargs = 0;
        while (a) {
            if (nargs >= 32) p_error("too many call args (limit 32)");
            av[nargs] = hl_expr(a);
            if (ty_is_llong(a->ty) || ty_is_double(a->ty)) {
#ifdef S12CC_X64_HOST
                av_hi[nargs] = -1;
                is64[nargs] = 0;
#else
                av_hi[nargs] = hl_hi;
                is64[nargs] = ty_is_double(a->ty) ? 2 : 1;
#endif
            } else {
                av_hi[nargs] = -1;
                is64[nargs] = 0;
#ifndef S12CC_X64_HOST
                if (ty_is_struct(a->ty)) {
                    /* clang byval: pass the address of a private copy in
                     * a size-reserving stack slot (hl_byval_arg). */
                    av[nargs] = hl_byval_arg(av[nargs], ty_size(a->ty));
                    is64[nargs] = HI_TAG_BYVAL + (ty_size(a->ty) + 3) / 4;
                }
#endif
            }
            nargs = nargs + 1;
            a = a->next;
        }
        /* Non-varargs doubles occupy aligned register pairs (clang's
         * f64 ABI); varargs calls pack sequentially so the callee's
         * va_arg walk stays gapless. */
        callee_var = (n->name != NULL) ? find_func_varargs(n->name) : 0;
        /* Now record args in h_carg, expanding 64-bit args to lo/hi pairs */
        carg_base = h_ncarg;
        phys_count = 0;
        i = 0;
        while (i < nargs) {
            if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
            h_carg[h_ncarg] = av[i];
            if (is64[i] >= HI_TAG_BYVAL) {
                h_carg_tag[h_ncarg] = is64[i];
            } else {
                h_carg_tag[h_ncarg] = (is64[i] == 2 && !callee_var) ? 1 : 0;
            }
            h_ncarg = h_ncarg + 1;
            phys_count = phys_count + 1;
            if (is64[i] == 1 || is64[i] == 2) {
                if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
                h_carg[h_ncarg] = av_hi[i];
                h_carg_tag[h_ncarg] = (is64[i] == 2 && !callee_var) ? 2 : 0;
                h_ncarg = h_ncarg + 1;
                phys_count = phys_count + 1;
            }
            i = i + 1;
        }
        ret_ty = find_func_type(n->name);
        if (ty_is_struct(ret_ty)) {
            /* Struct return: allocate temp, pass address as hidden first arg */
            int tmp_alloca;
            int tmp_sz;
            tmp_sz = ty_size(ret_ty);
            tmp_sz = ((tmp_sz + 3) / 4) * 4;
            hl_temp_stack = hl_temp_stack + tmp_sz;
            tmp_alloca = hl_emit_temp_alloca(HL_ADDR_TY, 0 - hl_temp_stack);
            /* Shift existing cargs right by 1 to make room for hidden
             * first arg.  Tags must move with their values or a double's
             * pair tags land on the wrong flat slots and caller/callee
             * disagree on its register pair. */
            i = h_ncarg;
            while (i > carg_base) {
                h_carg[i] = h_carg[i - 1];
                h_carg_tag[i] = h_carg_tag[i - 1];
                i = i - 1;
            }
            h_carg[carg_base] = tmp_alloca;
            h_carg_tag[carg_base] = 0;
            h_ncarg = h_ncarg + 1;
            phys_count = phys_count + 1;
            i = hi_emit(HI_CALL, HL_ADDR_TY, -1, -1, phys_count, n->name);
            h_cbase[i] = carg_base;
            return i;  /* returns the retptr (address of temp struct) */
        }
        /* Pass ret_ty as h_ty (not the historical TY_INT placeholder) so
         * codegen / regalloc can route the result to V0 for float returns
         * vs X0 for int/ptr.  Slow32 backends ignore h_ty here; a64
         * backend uses it via ra_class_of to pick the right ABI return
         * register. */
        i = hi_emit(HI_CALL, ret_ty, -1, -1, phys_count, n->name);
        h_cbase[i] = carg_base;
#ifdef S12CC_X64_HOST
        /* x64: 64-bit return is a single value, no CALLHI needed */
#else
        if (ty_is_llong(ret_ty) || ty_is_double(ret_ty)) {
            hl_hi = hi_emit(HI_CALLHI, TY_INT, i, -1, 0, NULL);
        }
#endif
        return i;
    }

    /* Function reference (bare function name as address) */
    if (n->kind == ND_FUNC_REF) {
        return hi_emit(HI_FADDR, TY_INT, -1, -1, 0, n->name);
    }

    /* Indirect call through expression */
    if (n->kind == ND_CALL_PTR) {
        int av[32];
        int av_hi2[32];
        int is64_2[32];  /* tri-state like is64 */
        int phys_count2;
        callee = hl_expr(n->lhs);
        a = n->args;
        nargs = 0;
        while (a) {
            if (nargs >= 32) p_error("too many call args (limit 32)");
            av[nargs] = hl_expr(a);
            if (ty_is_llong(a->ty) || ty_is_double(a->ty)) {
#ifdef S12CC_X64_HOST
                av_hi2[nargs] = -1;
                is64_2[nargs] = 0;
#else
                av_hi2[nargs] = hl_hi;
                is64_2[nargs] = ty_is_double(a->ty) ? 2 : 1;
#endif
            } else {
                av_hi2[nargs] = -1;
                is64_2[nargs] = 0;
#ifndef S12CC_X64_HOST
                if (ty_is_struct(a->ty)) {
                    /* clang byval, same as the direct-call path. */
                    av[nargs] = hl_byval_arg(av[nargs], ty_size(a->ty));
                    is64_2[nargs] = HI_TAG_BYVAL + (ty_size(a->ty) + 3) / 4;
                }
#endif
            }
            nargs = nargs + 1;
            a = a->next;
        }
        carg_base = h_ncarg;
        phys_count2 = 0;
        i = 0;
        while (i < nargs) {
            if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
            h_carg[h_ncarg] = av[i];
            if (is64_2[i] >= HI_TAG_BYVAL) {
                h_carg_tag[h_ncarg] = is64_2[i];
            } else {
                h_carg_tag[h_ncarg] = (is64_2[i] == 2) ? 1 : 0;
            }
            h_ncarg = h_ncarg + 1;
            phys_count2 = phys_count2 + 1;
            if (is64_2[i] == 1 || is64_2[i] == 2) {
                if (h_ncarg >= HIR_MAX_CARG) p_error("too many call args (hir)");
                h_carg[h_ncarg] = av_hi2[i];
                h_carg_tag[h_ncarg] = (is64_2[i] == 2) ? 2 : 0;
                h_ncarg = h_ncarg + 1;
                phys_count2 = phys_count2 + 1;
            }
            i = i + 1;
        }
        i = hi_emit(HI_CALLP, TY_INT, callee, -1, phys_count2, NULL);
        h_cbase[i] = carg_base;
        return i;
    }

    p_error("unknown expression node (hir)");
    return -1;
}

/* --- Statement lowering --- */

static void hl_stmt(Node *n) {
    int cv;
    int lv;
    int cmp;
    int head_blk;
    int body_blk;
    int step_blk;
    int exit_blk;
    int then_blk;
    int else_blk;
    int end_blk;
    int brk_blk;
    int def_blk;
    int case_blk;
    int next_blk;
    int sw_d;
    int sw_b;
    int sw_n;
    int sw_i;
    int cd;
    int ci;
    int lbl_blk;
    int zero_inst;
    int lt_kind;
    int use_bs;
    int use_jt;
    Node *s;
    Node *cs;

    if (!n) return;

    /* Return */
    if (n->kind == ND_RETURN) {
        /* Inside an inlined body a return is an assignment to the
         * result slot plus a branch to the continuation. */
        if (hl_inl_depth > 0) {
            if (n->lhs && hl_inl_res >= 0) {
                lv = hl_expr(n->lhs);
                if (ty_is_llong(hl_inl_res_ty) || ty_is_double(hl_inl_res_ty)) {
#ifdef S12CC_X64_HOST
                    hi_emit(HI_STORE, hl_inl_res_ty, hl_inl_res, lv, 0, NULL);
#else
                    {
                        int a4;
                        int vhi;
                        vhi = hl_hi;
                        hi_emit(HI_STORE, TY_INT, hl_inl_res, lv, 0, NULL);
                        a4 = hi_emit(HI_ADDI, TY_INT, hl_inl_res, -1, 4, NULL);
                        hi_emit(HI_STORE, TY_INT, a4, vhi, 0, NULL);
                    }
#endif
                } else {
                    hi_emit(HI_STORE, hl_inl_res_ty, hl_inl_res, lv, 0, NULL);
                }
            } else if (n->lhs) {
                (void)hl_expr(n->lhs);   /* void context: side effects only */
            }
            hi_emit(HI_BR, 0, -1, -1, hl_inl_ret_blk, NULL);
            return;
        }
        if (n->lhs && hl_struct_ret && ty_is_struct(n->lhs->ty)) {
            /* Struct return: copy struct to hidden __retptr, then return ptr */
            {
                int src_addr;
                int dst_addr;
                int copy_i;
                int copy_sz;
                int tmp;
                int src_off;
                int dst_off;
                src_addr = hl_addr(n->lhs);
                dst_addr = hi_emit(HI_LOAD, HL_ADDR_TY, hl_retptr_alloca, -1, 0, NULL);
                copy_sz = hl_ret_size;
                copy_i = 0;
                while (copy_i + 4 <= copy_sz) {
                    src_off = hi_emit(HI_ADDI, HL_ADDR_TY, src_addr, -1, copy_i, NULL);
                    tmp = hi_emit(HI_LOAD, TY_INT, src_off, -1, 0, NULL);
                    dst_off = hi_emit(HI_ADDI, HL_ADDR_TY, dst_addr, -1, copy_i, NULL);
                    hi_emit(HI_STORE, TY_INT, dst_off, tmp, 0, NULL);
                    copy_i = copy_i + 4;
                }
                while (copy_i < copy_sz) {
                    src_off = hi_emit(HI_ADDI, HL_ADDR_TY, src_addr, -1, copy_i, NULL);
                    tmp = hi_emit(HI_LOAD, TY_CHAR, src_off, -1, 0, NULL);
                    dst_off = hi_emit(HI_ADDI, HL_ADDR_TY, dst_addr, -1, copy_i, NULL);
                    hi_emit(HI_STORE, TY_CHAR, dst_off, tmp, 0, NULL);
                    copy_i = copy_i + 1;
                }
                hi_emit(HI_RET, 0, dst_addr, -1, 0, NULL);
            }
        } else if (n->lhs) {
            lv = hl_expr(n->lhs);
#ifdef S12CC_X64_HOST
            hi_emit(HI_RET, 0, lv, -1, 0, NULL);
#else
            if (ty_is_llong(n->lhs->ty) || ty_is_double(n->lhs->ty)) {
                hi_emit(HI_RET, 0, lv, hl_hi, 0, NULL);
            } else {
                hi_emit(HI_RET, 0, lv, -1, 0, NULL);
            }
#endif
        } else {
            hi_emit(HI_RET, 0, -1, -1, 0, NULL);
        }
        return;
    }

    /* If / If-else */
    if (n->kind == ND_IF) {
        cv = hl_cond_val(n->cond);
        if (n->els) {
            then_blk = hir_new_block();
            else_blk = hir_new_block();
            end_blk = hir_new_block();
            hi_emit(HI_BRC, 0, cv, then_blk, else_blk, NULL);

            hl_switch_block(then_blk);
            hl_stmt(n->body);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(else_blk);
            hl_stmt(n->els);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(end_blk);
        } else {
            then_blk = hir_new_block();
            end_blk = hir_new_block();
            hi_emit(HI_BRC, 0, cv, then_blk, end_blk, NULL);

            hl_switch_block(then_blk);
            hl_stmt(n->body);
            if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, end_blk, NULL);

            hl_switch_block(end_blk);
        }
        return;
    }

    /* While */
    if (n->kind == ND_WHILE) {
        head_blk = hir_new_block();
        body_blk = hir_new_block();
        exit_blk = hir_new_block();

        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_switch_block(head_blk);
        cv = hl_cond_val(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = head_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* Do-while */
    if (n->kind == ND_DO_WHILE) {
        body_blk = hir_new_block();
        step_blk = hir_new_block();
        exit_blk = hir_new_block();

        hi_emit(HI_BR, 0, -1, -1, body_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = step_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, step_blk, NULL);

        hl_switch_block(step_blk);
        cv = hl_cond_val(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* For */
    if (n->kind == ND_FOR) {
        head_blk = hir_new_block();
        body_blk = hir_new_block();
        step_blk = hir_new_block();
        exit_blk = hir_new_block();

        if (n->init) hl_expr(n->init);
        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_switch_block(head_blk);
        cv = hl_cond_val(n->cond);
        hi_emit(HI_BRC, 0, cv, body_blk, exit_blk, NULL);

        hl_break_blk[hl_loop_depth] = exit_blk;
        hl_cont_blk[hl_loop_depth] = step_blk;
        hl_loop_depth = hl_loop_depth + 1;

        hl_switch_block(body_blk);
        hl_stmt(n->body);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, step_blk, NULL);

        hl_switch_block(step_blk);
        if (n->step) hl_expr(n->step);
        hi_emit(HI_BR, 0, -1, -1, head_blk, NULL);

        hl_loop_depth = hl_loop_depth - 1;
        hl_switch_block(exit_blk);
        return;
    }

    /* Switch */
    if (n->kind == ND_SWITCH) {
        sw_d = hl_sw_depth;
        sw_b = 0;
        if (sw_d > 0) {
            sw_b = hl_sw_base[sw_d - 1] + hl_sw_count[sw_d - 1];
        }
        hl_sw_base[sw_d] = sw_b;
        hl_sw_count[sw_d] = 0;
        hl_sw_def[sw_d] = -1;

        /* Pre-scan body for case/default, allocate blocks */
        if (n->body && n->body->kind == ND_BLOCK) {
            cs = n->body->body;
            while (cs) {
                if (cs->kind == ND_CASE) {
                    sw_n = hl_sw_count[sw_d];
                    hl_sw_val[sw_b + sw_n] = cs->val;
                    hl_sw_blk[sw_b + sw_n] = hir_new_block();
                    hl_sw_count[sw_d] = sw_n + 1;
                } else if (cs->kind == ND_DEFAULT) {
                    hl_sw_def[sw_d] = hir_new_block();
                }
                cs = cs->next;
            }
        }

        brk_blk = hir_new_block();
        if (hl_sw_def[sw_d] >= 0) {
            def_blk = hl_sw_def[sw_d];
        } else {
            def_blk = brk_blk;
        }

        /* Push break target. A switch only provides `break`, not
         * `continue` — `continue` must skip past the switch and reach
         * the enclosing loop. Inherit the parent's continue target
         * (or -1 if there is no enclosing loop) so that `continue`
         * inside a `case` body doesn't read stale hl_cont_blk[] data.
         */
        hl_break_blk[hl_loop_depth] = brk_blk;
        if (hl_loop_depth > 0) {
            hl_cont_blk[hl_loop_depth] = hl_cont_blk[hl_loop_depth - 1];
        } else {
            hl_cont_blk[hl_loop_depth] = -1;
        }
        hl_loop_depth = hl_loop_depth + 1;

        /* Evaluate switch expression */
        lv = hl_expr(n->cond);

        sw_n = hl_sw_count[sw_d];
        if (n->cond && (n->cond->ty & TY_UNSIGNED)) lt_kind = HI_SLTU;
        else lt_kind = HI_SLT;

        /* Issue #32 dispatch selection:
         *   1. dense, non-fall-through switch  -> O(1) jump table
         *   2. >= 6 cases                       -> balanced binary tree
         *   3. otherwise                        -> linear comparison chain
         * The jump table needs single-predecessor case blocks, so it is
         * only attempted when no case falls through (see
         * hl_switch_has_fallthrough).
         *
         * HI_JMPTAB codegen exists for the native SLOW-32 target (hir_codegen.h)
         * and the x64 cross backend (hir_codegen_x64.h).  The a64 cross backend
         * (hir_codegen_a64.h) has no HI_JMPTAB handler yet, so emitting a JMPTAB
         * there would silently produce no dispatch and miscompile dense
         * switches; gate it off for a64 (it falls back to the binary-search /
         * comparison-chain paths below, which are correct). */
        use_jt = 0;
#ifndef S12CC_TARGET_A64
        if (sw_n >= 5 && !hl_switch_has_fallthrough(n->body)) {
            use_jt = hl_sw_emit_jumptable(lv, def_blk, sw_b, sw_n);
        }
#endif
        if (!use_jt) {
            use_bs = (sw_n >= 6);
            if (use_bs) {
                hl_sw_sort_cases(sw_b, sw_n, (lt_kind == HI_SLTU));
                hl_sw_emit_bsearch(lv, def_blk, lt_kind, sw_b, sw_b + sw_n - 1);
            } else {
                hl_sw_emit_chain(lv, def_blk, sw_b, sw_n);
            }
        }

        /* Generate body with case cursor */
        hl_sw_cur[sw_d] = 0;
        hl_sw_depth = sw_d + 1;
        hl_stmt(n->body);
        hl_sw_depth = sw_d;

        /* Break label */
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, brk_blk, NULL);
        hl_switch_block(brk_blk);
        hl_loop_depth = hl_loop_depth - 1;
        return;
    }

    /* Case label */
    if (n->kind == ND_CASE) {
        cd = hl_sw_depth - 1;
        ci = hl_sw_cur[cd];
        case_blk = hl_sw_blk[hl_sw_base[cd] + ci];
        hl_sw_cur[cd] = ci + 1;
        /* Fall-through from previous block */
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, case_blk, NULL);
        hl_switch_block(case_blk);
        return;
    }

    /* Default label */
    if (n->kind == ND_DEFAULT) {
        cd = hl_sw_depth - 1;
        def_blk = hl_sw_def[cd];
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, def_blk, NULL);
        hl_switch_block(def_blk);
        return;
    }

    /* Break */
    if (n->kind == ND_BREAK) {
        if (hl_loop_depth < 1) p_error("break outside loop/switch");
        hi_emit(HI_BR, 0, -1, -1, hl_break_blk[hl_loop_depth - 1], NULL);
        return;
    }

    /* Continue */
    if (n->kind == ND_CONTINUE) {
        if (hl_loop_depth < 1 || hl_cont_blk[hl_loop_depth - 1] < 0)
            p_error("continue outside loop");
        hi_emit(HI_BR, 0, -1, -1, hl_cont_blk[hl_loop_depth - 1], NULL);
        return;
    }

    /* Goto */
    if (n->kind == ND_GOTO) {
        lbl_blk = hl_label_block(n->val);
        hi_emit(HI_BR, 0, -1, -1, lbl_blk, NULL);
        return;
    }

    /* Label */
    if (n->kind == ND_LABEL) {
        lbl_blk = hl_label_block(n->val);
        if (!hl_terminated()) hi_emit(HI_BR, 0, -1, -1, lbl_blk, NULL);
        hl_switch_block(lbl_blk);
        if (n->body) hl_stmt(n->body);
        return;
    }

    /* Block */
    if (n->kind == ND_BLOCK) {
        s = n->body;
        while (s) {
            if (hl_terminated() && s->kind != ND_LABEL &&
                s->kind != ND_CASE && s->kind != ND_DEFAULT) {
                s = s->next;
                continue;
            }
            hl_stmt(s);
            s = s->next;
        }
        return;
    }

    /* Expression statement */
    if (n->kind == ND_EXPR_STMT) {
        if (n->lhs) hl_expr(n->lhs);
        return;
    }

    p_error("unknown statement node (hir)");
}

/* hl_nparams declared above (near forward declarations) for SSA + VA_START */

/* --- Lower one function --- */

static void hl_func(Node *fn) {
    int entry;
    hl_cur_fn_dbg = fn->name;
    int pi;
    int param_inst;
    int param_alloca;

    hir_reset();
    hl_nalloca = 0;
    hl_nparams = fn->nparams;  /* will be updated by param expansion for llong */
    hl_hi = -1;
    hl_temp_stack = fn->locals_size;
    hl_loop_depth = 0;
    hl_sw_depth = 0;
    hl_ngoto = 0;
    hl_struct_ret = ty_is_struct(fn->ty) ? 1 : 0;
    hl_retptr_alloca = -1;
    hl_ret_size = hl_struct_ret ? ty_size(fn->ty) : 0;

    entry = hir_new_block();
    hl_switch_block(entry);

    /* Emit PARAM+STORE for each parameter (enables SSA promotion) */
    {
        int phys_idx;
        int param_hi;
        int a4;
        Node *pp;
        phys_idx = 0;

        {
            int pt_i;
            pt_i = 0;
            while (pt_i < 64) { hl_param_tags[pt_i] = 0; pt_i = pt_i + 1; }
        }

        /* Pass 1: emit EVERY HI_PARAM before any copy/store code.  A
         * PARAM's incoming register is only pinned from its def point
         * onward; if the copy loop for an earlier param runs first, the
         * allocator may hand a later param's incoming register to one
         * of the copy's address temps (this clobbered a second struct
         * param's pointer in r5 before it was ever read). */
        if (hl_struct_ret) {
            /* Hidden first param for struct return.  On 64-bit hosts the
             * retptr is a pointer (8 bytes); using TY_INT silently
             * truncates to 32 bits. */
            hl_pp_inst[0] = hi_emit(HI_PARAM, HL_ADDR_TY, -1, -1, 0, NULL);
            phys_idx = 1;
        }
        pp = fn->args;
        while (pp) {
            if (ty_is_struct(pp->ty)) {
#ifndef S12CC_X64_HOST
                /* clang byval: the pointer to the caller's copy arrives
                 * in a stack slot reserving the struct's rounded size,
                 * never in a register.  A varargs fn can't mix that slot
                 * with the register save area's contiguous va walk. */
                if (fn->is_varargs)
                    p_error("struct parameter in varargs function (slow32)");
                if (phys_idx < 64)
                    hl_param_tags[phys_idx] =
                        HI_TAG_BYVAL + (ty_size(pp->ty) + 3) / 4;
#endif
                hl_pp_inst[phys_idx] = hi_emit(HI_PARAM, HL_ADDR_TY, -1, -1, phys_idx, NULL);
                phys_idx = phys_idx + 1;
            } else if (ty_is_llong(pp->ty) || ty_is_double(pp->ty)) {
#ifdef S12CC_X64_HOST
                hl_pp_inst[phys_idx] = hi_emit(HI_PARAM, pp->ty, -1, -1, phys_idx, NULL);
                phys_idx = phys_idx + 1;
#else
                /* Doubles in NON-VARARGS functions sit in an aligned
                 * register pair (tags 1/2 -> hi_abi_assign). */
                if (phys_idx < 62 && ty_is_double(pp->ty) && !fn->is_varargs) {
                    hl_param_tags[phys_idx] = 1;
                    hl_param_tags[phys_idx + 1] = 2;
                }
                hl_pp_inst[phys_idx] = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx, NULL);
                hl_pp_inst[phys_idx + 1] = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx + 1, NULL);
                phys_idx = phys_idx + 2;
#endif
            } else {
#ifdef S12CC_X64_HOST
                hl_pp_inst[phys_idx] = hi_emit(HI_PARAM, pp->ty, -1, -1, phys_idx, NULL);
#else
                hl_pp_inst[phys_idx] = hi_emit(HI_PARAM, TY_INT, -1, -1, phys_idx, NULL);
#endif
                phys_idx = phys_idx + 1;
            }
            pp = pp->next;
        }
        hl_nparams = phys_idx;

        /* Pass 2: store/copy each param into its local slot. */
        phys_idx = 0;
        if (hl_struct_ret) {
            hl_retptr_alloca = hl_get_alloca(fn->offset, HL_ADDR_TY);
            hi_emit(HI_STORE, HL_ADDR_TY, hl_retptr_alloca, hl_pp_inst[0], 0, NULL);
            phys_idx = 1;
        }
        pp = fn->args;
        while (pp) {
            if (ty_is_struct(pp->ty)) {
                /* Struct parameter (pass-by-value semantics): caller
                 * passes a pointer to the source struct in one arg
                 * register; we copy its bytes into the local slot at
                 * pp->offset so subsequent references to pp work via
                 * the usual fp + pp->offset + member_offset path, and
                 * callee mutations stay local.  Mirrors the inline
                 * byte-copy loop used on the struct-return path. */
                int param_ptr;
                int local_addr;
                int copy_i;
                int copy_sz;
                int src_off;
                int dst_off;
                int tmp;

                param_ptr = hl_pp_inst[phys_idx];
                local_addr = hl_get_alloca(pp->offset, pp->ty);
                copy_sz = ty_size(pp->ty);
                copy_i = 0;
                while (copy_i + 4 <= copy_sz) {
                    src_off = hi_emit(HI_ADDI, HL_ADDR_TY, param_ptr, -1, copy_i, NULL);
                    tmp = hi_emit(HI_LOAD, TY_INT, src_off, -1, 0, NULL);
                    dst_off = hi_emit(HI_ADDI, HL_ADDR_TY, local_addr, -1, copy_i, NULL);
                    hi_emit(HI_STORE, TY_INT, dst_off, tmp, 0, NULL);
                    copy_i = copy_i + 4;
                }
                while (copy_i < copy_sz) {
                    src_off = hi_emit(HI_ADDI, HL_ADDR_TY, param_ptr, -1, copy_i, NULL);
                    tmp = hi_emit(HI_LOAD, TY_CHAR, src_off, -1, 0, NULL);
                    dst_off = hi_emit(HI_ADDI, HL_ADDR_TY, local_addr, -1, copy_i, NULL);
                    hi_emit(HI_STORE, TY_CHAR, dst_off, tmp, 0, NULL);
                    copy_i = copy_i + 1;
                }
                phys_idx = phys_idx + 1;
            } else if (ty_is_llong(pp->ty) || ty_is_double(pp->ty)) {
#ifdef S12CC_X64_HOST
                param_inst = hl_pp_inst[phys_idx];
                param_alloca = hl_get_alloca(pp->offset, pp->ty);
                hi_emit(HI_STORE, pp->ty, param_alloca, param_inst, 0, NULL);
                phys_idx = phys_idx + 1;
#else
                /* Both llong and double arrive as a lo/hi word pair. */
                param_inst = hl_pp_inst[phys_idx];
                param_hi = hl_pp_inst[phys_idx + 1];
                param_alloca = hl_get_alloca(pp->offset, TY_INT);
                hi_emit(HI_STORE, TY_INT, param_alloca, param_inst, 0, NULL);
                a4 = hi_emit(HI_ADDI, TY_INT, param_alloca, -1, 4, NULL);
                hi_emit(HI_STORE, TY_INT, a4, param_hi, 0, NULL);
                phys_idx = phys_idx + 2;
#endif
            } else {
#ifdef S12CC_X64_HOST
                /* x64: preserve pointer types (8 bytes) vs int (4 bytes) */
                param_inst = hl_pp_inst[phys_idx];
                param_alloca = hl_get_alloca(pp->offset, pp->ty);
                hi_emit(HI_STORE, pp->ty, param_alloca, param_inst, 0, NULL);
#else
                param_inst = hl_pp_inst[phys_idx];
                param_alloca = hl_get_alloca(pp->offset, TY_INT);
                hi_emit(HI_STORE, TY_INT, param_alloca, param_inst, 0, NULL);
#endif
                phys_idx = phys_idx + 1;
            }
            pp = pp->next;
        }
        hl_nparams = phys_idx;
        /* Resolve the incoming-parameter ABI locations (register or
         * stack) for the allocator's preferences and the codegen
         * entry sequence. */
        hl_param_nflat = phys_idx;
        {
            int pm_i;
            int pm_ord;
            hi_abi_assign(hl_param_tags, hl_param_nflat, hl_param_map);
            /* Byte offsets from the incoming sp: byval slots reserve
             * the struct's rounded size, everything else one word. */
            pm_ord = 0;
            pm_i = 0;
            while (pm_i < hl_param_nflat) {
                if (hl_param_map[pm_i] < 0) {
                    hl_param_stkord[pm_i] = pm_ord;
                    if (hl_param_tags[pm_i] >= HI_TAG_BYVAL) {
                        pm_ord = pm_ord + (hl_param_tags[pm_i] - HI_TAG_BYVAL) * 4;
                    } else {
                        pm_ord = pm_ord + 4;
                    }
                } else {
                    hl_param_stkord[pm_i] = -1;
                }
                pm_i = pm_i + 1;
            }
        }
    }

    hl_stmt(fn->body);

    /* Ensure function ends with a return */
    if (!hl_terminated()) {
        hi_emit(HI_RET, 0, -1, -1, 0, NULL);
    }
}
