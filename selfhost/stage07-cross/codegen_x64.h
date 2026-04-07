/* codegen_x64.h -- Tree-walk code generator targeting x86-64 ELF
 *
 * Walks AST, emits x86-64 machine code via x64_encode.h.
 * Stack-machine style: expressions evaluate into RAX,
 * binary ops push lhs, evaluate rhs, pop lhs into RCX.
 *
 * After code generation, wraps output in ELF via elf_writer.h.
 */

/* ---- includes must come before this file ----
 * x64_encode.h   (x64_buf[], x64_off, encoding functions)
 * elf_writer.h   (elf_* functions)
 */

/* --- Label system --- */
/* Labels are tracked as code buffer offsets. Forward references are
 * recorded in a patch list and resolved when the label is defined. */

#define CG_MAX_LABELS  4096
#define CG_MAX_PATCHES 16384

static int cg_lbl_off[CG_MAX_LABELS]; /* -1 = not yet defined */
/* cg_lbl is defined in parser.h */

/* Forward-reference patch list */
static int cg_patch_lbl[CG_MAX_PATCHES];
static int cg_patch_off[CG_MAX_PATCHES]; /* offset of rel32 in x64_buf */
static int cg_npatches;

static int cg_olen;  /* compatibility: set to final code size */
/* cg_out is x64_buf from x64_encode.h */

/* --- String pool --- */
#define CG_MAX_STRINGS    1024
#define CG_STRING_POOL_SZ 65536

static char cg_str_pool[CG_STRING_POOL_SZ];
static int  cg_str_off[CG_MAX_STRINGS]; /* offset into pool */
static int  cg_str_len[CG_MAX_STRINGS]; /* length including NUL */
static int  cg_nstrings;
static int  cg_str_pool_used;

/* --- Global variable tracking --- */
#define CG_MAX_GLOBALS 1024

static char *cg_glob_name[CG_MAX_GLOBALS];
static int   cg_glob_size[CG_MAX_GLOBALS];
static int   cg_glob_init[CG_MAX_GLOBALS]; /* initial value (or 0) */
static int   cg_glob_has_init[CG_MAX_GLOBALS]; /* 1 if initialized */
static int   cg_nglobals;

/* --- Function symbol table (for call resolution) --- */
#define CG_MAX_FUNCS 512

static char *cg_func_name[CG_MAX_FUNCS];
static int   cg_func_off[CG_MAX_FUNCS];  /* offset in code buffer */
static int   cg_nfuncs;

/* --- Call patch list (function call relocations) --- */
#define CG_MAX_CALL_PATCHES 4096

static char *cg_cpatch_name[CG_MAX_CALL_PATCHES];
static int   cg_cpatch_off[CG_MAX_CALL_PATCHES]; /* offset of rel32 */
static int   cg_ncpatches;

/* --- Data relocations (code references to rodata/data addresses) --- */
#define CG_MAX_DATA_RELOCS 4096

#define DRELOC_STRING 0
#define DRELOC_GLOBAL 1

static int cg_dreloc_off[CG_MAX_DATA_RELOCS];  /* offset of imm64 in code */
static int cg_dreloc_kind[CG_MAX_DATA_RELOCS]; /* DRELOC_STRING or DRELOC_GLOBAL */
static int cg_dreloc_idx[CG_MAX_DATA_RELOCS];
static int cg_ndrelocs;

/* --- Per-function state --- */
static int cg_epilog;
static int cg_frame_size;

#define CG_MAX_LOOP 32
static int cg_break_lbl[CG_MAX_LOOP];
static int cg_cont_lbl[CG_MAX_LOOP];
static int cg_loop_depth;

/* Switch state */
#define CG_MAX_CASE     256
#define CG_MAX_SW_DEPTH 8
static int cg_sw_val[CG_MAX_CASE];
static int cg_sw_lbl[CG_MAX_CASE];
static int cg_sw_base[CG_MAX_SW_DEPTH];
static int cg_sw_count[CG_MAX_SW_DEPTH];
static int cg_sw_def[CG_MAX_SW_DEPTH];
static int cg_sw_cur[CG_MAX_SW_DEPTH];
static int cg_sw_depth;

/* ============================================================================
 * Label management
 * ============================================================================ */

/* cg_label() is defined in parser.h (shared with goto label allocation).
 * We use it here — do not redefine. */

static void cg_ldef(int l) {
    int i;
    cg_lbl_off[l] = x64_off;
    /* Resolve pending patches for this label */
    i = 0;
    while (i < cg_npatches) {
        if (cg_patch_lbl[i] == l) {
            x64_patch_rel32(cg_patch_off[i], x64_off);
            /* Remove by swapping with last */
            cg_npatches = cg_npatches - 1;
            cg_patch_lbl[i] = cg_patch_lbl[cg_npatches];
            cg_patch_off[i] = cg_patch_off[cg_npatches];
        } else {
            i = i + 1;
        }
    }
}

/* Jump to label — emits JMP rel32, patches later if forward ref */
static void cg_jmp_label(int l) {
    if (cg_lbl_off[l] >= 0) {
        /* Backward reference: compute rel32 directly */
        int target;
        target = cg_lbl_off[l];
        x64_jmp_rel32(target - (x64_off + 5));
    } else {
        /* Forward reference: emit placeholder, record patch */
        x64_byte(0xE9);
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = x64_off;
        cg_npatches = cg_npatches + 1;
        x64_dword(0);
    }
}

/* Conditional jump to label — cc is condition code byte (0x84=JE, etc.) */
static void cg_jcc_label(int cc, int l) {
    if (cg_lbl_off[l] >= 0) {
        int target;
        target = cg_lbl_off[l];
        x64_jcc_rel32(cc, target - (x64_off + 6));
    } else {
        x64_byte(0x0F);
        x64_byte(cc);
        cg_patch_lbl[cg_npatches] = l;
        cg_patch_off[cg_npatches] = x64_off;
        cg_npatches = cg_npatches + 1;
        x64_dword(0);
    }
}

/* ============================================================================
 * Stack operations (expression temporaries)
 * ============================================================================ */

static void cg_push(void) {
    x64_push(X64_RAX);
}

static void cg_pop(void) {
    x64_pop(X64_RCX);
}

/* ============================================================================
 * Helpers
 * ============================================================================ */

/* Check if a type needs 64-bit (pointer) operations on x86-64 */
static int cg_is_ptr_type(int ty) {
    return ty_is_ptr(ty);
}

/* Scale a SLOW-32 stack offset (4-byte slots) to x86-64 (8-byte slots).
 * Frontend offsets: -12, -16, -20, ... (4 bytes apart, starting at -12)
 * x86-64 offsets:   -16, -24, -32, ... (8 bytes apart, starting at -16)
 *
 * The SLOW-32 frontend reserves fp-4 for saved LR and fp-8 for saved FP.
 * On x86-64, we don't have saved LR/FP in the frame (PUSH RBP handles it).
 * So we map: frontend_offset → (frontend_offset + 8) * 2
 * This gives: -12 → (-4)*2 = -8; -16 → (-8)*2 = -16; -20 → (-12)*2 = -24
 *
 * Simpler: multiply the offset by 2. This gives sufficient spacing:
 * -12 → -24; -16 → -32; -20 → -40; etc. (8 bytes between each)
 */
static int cg_x64_offset(int slow32_offset) {
    return slow32_offset * 2;
}

/* Load a variable from [RBP + scaled_offset] with correct width for type.
 * Caller passes the SLOW-32 offset; we scale to x86-64 internally. */
static void cg_load_local(int ty, int offset) {
    int off;
    off = cg_x64_offset(offset);
    if (cg_is_ptr_type(ty)) {
        x64_mov_rm64(X64_RAX, X64_RBP, off);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm8(X64_RAX, X64_RBP, off);
        else
            x64_movsx_rm8(X64_RAX, X64_RBP, off);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm16(X64_RAX, X64_RBP, off);
        else
            x64_movsx_rm16(X64_RAX, X64_RBP, off);
    } else {
        x64_mov_rm(X64_RAX, X64_RBP, off);
    }
}

/* Store a value to [RBP + scaled_offset] with correct width for type */
static void cg_store_local(int ty, int offset, int src_reg) {
    int off;
    off = cg_x64_offset(offset);
    if (cg_is_ptr_type(ty)) {
        x64_mov_mr64(X64_RBP, off, src_reg);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        x64_mov_mr8(X64_RBP, off, src_reg);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        x64_mov_mr16(X64_RBP, off, src_reg);
    } else {
        x64_mov_mr(X64_RBP, off, src_reg);
    }
}

/* Load 32-bit immediate into EAX */
static void cg_li(int v) {
    if (v == 0) {
        x64_xor_rr(X64_RAX, X64_RAX);
    } else {
        x64_mov_ri(X64_RAX, v);
    }
}

/* Load 64-bit address placeholder into RAX, record data reloc */
static void cg_data_addr(int kind, int idx) {
    cg_dreloc_off[cg_ndrelocs] = x64_off + 2; /* imm64 starts after REX+opcode */
    cg_dreloc_kind[cg_ndrelocs] = kind;
    cg_dreloc_idx[cg_ndrelocs] = idx;
    cg_ndrelocs = cg_ndrelocs + 1;
    x64_mov_ri64(X64_RAX, 0, 0);
}

/* Load from [RAX] with appropriate width for type → result in EAX */
static void cg_load(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm8(X64_RAX, X64_RAX, 0);
        else
            x64_movsx_rm8(X64_RAX, X64_RAX, 0);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        if (ty & TY_UNSIGNED)
            x64_movzx_rm16(X64_RAX, X64_RAX, 0);
        else
            x64_movsx_rm16(X64_RAX, X64_RAX, 0);
    } else if (cg_is_ptr_type(ty)) {
        x64_mov_rm64(X64_RAX, X64_RAX, 0);
    } else {
        x64_mov_rm(X64_RAX, X64_RAX, 0);
    }
}

/* Store RCX to [RAX] with appropriate width */
static void cg_store(int ty) {
    if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_CHAR) {
        x64_mov_mr8(X64_RAX, 0, X64_RCX);
    } else if (!ty_is_ptr(ty) && (ty & TY_BASE_MASK) == TY_SHORT) {
        x64_mov_mr16(X64_RAX, 0, X64_RCX);
    } else if (cg_is_ptr_type(ty)) {
        x64_mov_mr64(X64_RAX, 0, X64_RCX);
    } else {
        x64_mov_mr(X64_RAX, 0, X64_RCX);
    }
}

/* Compute address: RAX = RBP + scaled_offset */
static void cg_frame_off(int off) {
    x64_lea(X64_RAX, X64_RBP, cg_x64_offset(off));
}

/* ============================================================================
 * Expression generation — result in EAX
 * ============================================================================ */

static void gen_expr(Node *n);
static void gen_stmt(Node *n);

static int cg_strcmp(char *a, char *b) {
    int i;
    i = 0;
    while (a[i] && a[i] == b[i]) i = i + 1;
    return a[i] - b[i];
}

/* Generate address of lvalue into RAX */
static void gen_addr(Node *n) {
    if (n->kind == ND_VAR) {
        if (n->is_local) {
            cg_frame_off(n->offset);
        } else {
            /* Global variable: load address (data reloc) */
            int gi;
            gi = 0;
            while (gi < cg_nglobals) {
                if (cg_strcmp(cg_glob_name[gi], n->name) == 0) break;
                gi = gi + 1;
            }
            cg_data_addr(DRELOC_GLOBAL, gi);
        }
        return;
    }
    if (n->kind == ND_UNARY && n->op == TK_STAR) {
        gen_expr(n->lhs);
        return;
    }
    if (n->kind == ND_MEMBER) {
        gen_addr(n->lhs);
        if (n->val != 0) {
            x64_add_ri64(X64_RAX, n->val);
        }
        return;
    }
    p_error("not an lvalue");
}

/* Generate expression, result in EAX */
static void gen_expr(Node *n) {
    int l1;
    int l2;
    Node *a;
    int i;
    int elem_sz;

    if (!n) { cg_li(0); return; }

    if (n->kind == ND_NUM) {
        cg_li(n->val);
        return;
    }

    if (n->kind == ND_STRING) {
        cg_data_addr(DRELOC_STRING, n->val);
        return;
    }

    if (n->kind == ND_VAR) {
        if (n->is_array || ty_is_struct(n->ty)) {
            gen_addr(n);
            return;
        }
        if (n->is_local) {
            cg_load_local(n->ty, n->offset);
        } else {
            /* Global: load address, then load value */
            gen_addr(n);
            cg_load(n->ty);
        }
        return;
    }

    if (n->kind == ND_ASSIGN) {
        gen_expr(n->rhs);
        cg_push();
        gen_addr(n->lhs);
        cg_pop();          /* RCX = value, RAX = address */
        cg_store(n->ty);
        x64_mov_rr(X64_RAX, X64_RCX);  /* result = stored value */
        return;
    }

    if (n->kind == ND_UNARY) {
        if (n->op == TK_MINUS) {
            gen_expr(n->lhs);
            x64_neg(X64_RAX);
            return;
        }
        if (n->op == TK_BANG) {
            gen_expr(n->lhs);
            x64_test_rr(X64_RAX, X64_RAX);
            x64_sete(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_STAR) {
            gen_expr(n->lhs);
            if (!ty_is_struct(n->ty)) {
                cg_load(n->ty);
            }
            return;
        }
        if (n->op == TK_AMP) {
            gen_addr(n->lhs);
            return;
        }
        if (n->op == TK_TILDE) {
            gen_expr(n->lhs);
            x64_not(X64_RAX);
            return;
        }
        p_error("unknown unary op");
        return;
    }

    if (n->kind == ND_BINOP) {
        /* Short-circuit: && */
        if (n->op == TK_LAND) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            x64_test_rr(X64_RAX, X64_RAX);
            cg_jcc_label(X64_CC_E, l1);
            gen_expr(n->rhs);
            x64_test_rr(X64_RAX, X64_RAX);
            x64_setne(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            cg_jmp_label(l2);
            cg_ldef(l1);
            x64_xor_rr(X64_RAX, X64_RAX);
            cg_ldef(l2);
            return;
        }
        /* Short-circuit: || */
        if (n->op == TK_LOR) {
            l1 = cg_label();
            l2 = cg_label();
            gen_expr(n->lhs);
            x64_test_rr(X64_RAX, X64_RAX);
            cg_jcc_label(X64_CC_NE, l1);
            gen_expr(n->rhs);
            x64_test_rr(X64_RAX, X64_RAX);
            x64_setne(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            cg_jmp_label(l2);
            cg_ldef(l1);
            x64_mov_ri(X64_RAX, 1);
            cg_ldef(l2);
            return;
        }

        /* Pointer arithmetic: scale offset */
        if (n->op == TK_PLUS || n->op == TK_MINUS) {
            if (ty_is_ptr(n->lhs->ty)) {
                elem_sz = ty_size(ty_deref(n->lhs->ty));
                gen_expr(n->lhs);
                cg_push();
                gen_expr(n->rhs);
                if (elem_sz > 1) {
                    x64_imul_rri(X64_RAX, X64_RAX, elem_sz);
                }
                cg_pop();  /* RCX = pointer, RAX = scaled offset */
                /* Use 64-bit ops for pointer arithmetic */
                x64_movsxd(X64_RAX, X64_RAX); /* sign-extend offset to 64-bit */
                if (n->op == TK_PLUS)
                    x64_add_rr64(X64_RAX, X64_RCX);
                else {
                    x64_sub_rr64(X64_RCX, X64_RAX);
                    x64_mov_rr64(X64_RAX, X64_RCX);
                }
                return;
            }
        }

        /* Regular binary op: lhs in RCX, rhs in RAX */
        gen_expr(n->lhs);
        cg_push();
        gen_expr(n->rhs);
        cg_pop();  /* RCX = lhs, RAX = rhs */

        if (n->op == TK_PLUS) {
            x64_add_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_MINUS) {
            /* result = lhs - rhs = RCX - RAX */
            x64_sub_rr(X64_RCX, X64_RAX);
            x64_mov_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_STAR) {
            x64_imul_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_SLASH || n->op == TK_PERCENT) {
            /* RCX = lhs (dividend), RAX = rhs (divisor)
             * IDIV: EDX:EAX / src → EAX=quotient, EDX=remainder
             * Need: lhs in EAX, divisor in a non-RDX reg */
            x64_mov_rr(X64_R10, X64_RAX);  /* save divisor → R10 */
            x64_mov_rr(X64_RAX, X64_RCX);  /* lhs → EAX */
            if (n->ty & TY_UNSIGNED) {
                x64_zero(X64_RDX);          /* zero-extend for unsigned */
                x64_div(X64_R10);
            } else {
                x64_cdq();                   /* sign-extend EAX → EDX:EAX */
                x64_idiv(X64_R10);
            }
            if (n->op == TK_PERCENT)
                x64_mov_rr(X64_RAX, X64_RDX);  /* remainder → EAX */
            return;
        }

        /* For comparisons: CMP lhs, rhs → SETcc */
        if (n->op == TK_EQ) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            x64_sete(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_NE) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            x64_setne(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_LT) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            if (n->ty & TY_UNSIGNED) x64_setb(X64_RAX);
            else x64_setl(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_GT) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            if (n->ty & TY_UNSIGNED) x64_seta(X64_RAX);
            else x64_setg(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_LE) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            if (n->ty & TY_UNSIGNED) x64_setbe(X64_RAX);
            else x64_setle(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }
        if (n->op == TK_GE) {
            x64_cmp_rr(X64_RCX, X64_RAX);
            if (n->ty & TY_UNSIGNED) x64_setae(X64_RAX);
            else x64_setge(X64_RAX);
            x64_movzx_rr8(X64_RAX, X64_RAX);
            return;
        }

        if (n->op == TK_AMP) {
            x64_and_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_PIPE) {
            x64_or_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_CARET) {
            x64_xor_rr(X64_RAX, X64_RCX);
            return;
        }
        if (n->op == TK_LSHIFT) {
            /* result = lhs << rhs; RCX=lhs, RAX=rhs(count) */
            /* SHL needs count in CL. Swap: EAX↔ECX */
            x64_mov_rr(X64_RDX, X64_RAX);  /* save shift count */
            x64_mov_rr(X64_RAX, X64_RCX);  /* lhs → EAX */
            x64_mov_rr(X64_RCX, X64_RDX);  /* count → ECX (CL) */
            x64_shl_cl(X64_RAX);
            return;
        }
        if (n->op == TK_RSHIFT) {
            x64_mov_rr(X64_RDX, X64_RAX);
            x64_mov_rr(X64_RAX, X64_RCX);
            x64_mov_rr(X64_RCX, X64_RDX);
            if (n->ty & TY_UNSIGNED) x64_shr_cl(X64_RAX);
            else x64_sar_cl(X64_RAX);
            return;
        }

        p_error("unknown binop");
        return;
    }

    /* Division and modulo — handle separately because they need special setup.
     * Note: these are now handled here before ND_BINOP falls through.
     * Actually, they're ND_BINOP with op=TK_SLASH/TK_PERCENT.
     * The code above already checked for TK_SLASH but had a bug.
     * Let me handle it via a separate path. */

    if (n->kind == ND_COMP_ASSIGN) {
        /* Strategy: eval rhs → push; gen_addr → push addr; load old_val;
         * pop rhs into RCX; apply op; save result; pop addr; store. */
        gen_expr(n->rhs);
        cg_push();          /* stack: [rhs] */
        gen_addr(n->lhs);
        cg_push();          /* stack: [rhs, addr] */
        cg_load(n->ty);    /* RAX = old_val (from [RAX=addr]) */
        /* Now: RAX = old_val, stack = [rhs, addr] */
        /* Get rhs from stack: it's at [RSP+8] */
        x64_mov_rm64(X64_RCX, X64_RSP, 8); /* RCX = rhs */

        /* Apply: result = old_val op rhs. RAX=old_val, RCX=rhs */
        if (n->op == TK_PLUS) x64_add_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_MINUS) x64_sub_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_STAR) x64_imul_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_AMP) x64_and_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_PIPE) x64_or_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_CARET) x64_xor_rr(X64_RAX, X64_RCX);
        else if (n->op == TK_SLASH || n->op == TK_PERCENT) {
            /* RAX = old_val (dividend), RCX = rhs (divisor) */
            x64_mov_rr(X64_R10, X64_RCX);
            if (n->ty & TY_UNSIGNED) {
                x64_zero(X64_RDX);
                x64_div(X64_R10);
            } else {
                x64_cdq();
                x64_idiv(X64_R10);
            }
            if (n->op == TK_PERCENT)
                x64_mov_rr(X64_RAX, X64_RDX);
        }
        else if (n->op == TK_LSHIFT) {
            /* RAX = old_val, RCX = shift count (already in CL) */
            x64_shl_cl(X64_RAX);
        }
        else if (n->op == TK_RSHIFT) {
            if (n->ty & TY_UNSIGNED) x64_shr_cl(X64_RAX);
            else x64_sar_cl(X64_RAX);
        }
        /* RAX = new_val */
        x64_mov_rr(X64_RCX, X64_RAX);  /* RCX = new_val */
        /* Pop addr into RAX */
        x64_pop(X64_RAX);              /* RAX = addr */
        x64_add_ri64(X64_RSP, 8);      /* discard rhs */
        cg_store(n->ty);               /* store RCX to [RAX] */
        x64_mov_rr(X64_RAX, X64_RCX);  /* result = new_val */
        return;
    }

    if (n->kind == ND_POST_INC || n->kind == ND_POST_DEC) {
        gen_addr(n->lhs);
        cg_push();                      /* save address */
        x64_mov_rr(X64_RCX, X64_RAX);  /* RCX = addr for load */
        cg_load(n->ty);                 /* RAX = old value */
        cg_push();                      /* save old value (return val) */
        /* Compute new value */
        if (ty_is_ptr(n->ty)) {
            elem_sz = ty_size(ty_deref(n->ty));
        } else {
            elem_sz = 1;
        }
        if (n->kind == ND_POST_INC)
            x64_add_ri(X64_RAX, elem_sz);
        else
            x64_sub_ri(X64_RAX, elem_sz);
        x64_mov_rr(X64_RCX, X64_RAX);  /* RCX = new value */
        /* Pop addr, store new value */
        x64_mov_rm(X64_RAX, X64_RSP, 8); /* RAX = addr (2nd on stack) */
        cg_store(n->ty);
        /* Result = old value */
        cg_pop();                       /* RAX = old value */
        x64_add_ri64(X64_RSP, 8);       /* discard saved addr */
        return;
    }

    if (n->kind == ND_TERNARY) {
        l1 = cg_label();
        l2 = cg_label();
        gen_expr(n->cond);
        x64_test_rr(X64_RAX, X64_RAX);
        cg_jcc_label(X64_CC_E, l1);
        gen_expr(n->lhs);
        cg_jmp_label(l2);
        cg_ldef(l1);
        gen_expr(n->rhs);
        cg_ldef(l2);
        return;
    }

    if (n->kind == ND_CAST) {
        gen_expr(n->lhs);
        /* TODO: actual type conversions (sign-extend, truncate) */
        return;
    }

    if (n->kind == ND_COMMA) {
        gen_expr(n->lhs);
        gen_expr(n->rhs);
        return;
    }

    if (n->kind == ND_MEMBER) {
        gen_addr(n);
        if (!ty_is_struct(n->ty)) {
            cg_load(n->ty);
        }
        return;
    }

    if (n->kind == ND_FUNC_REF) {
        /* Load function address into RAX.
         * Emit movabs rax, imm64 with a call patch to resolve later. */
        cg_cpatch_name[cg_ncpatches] = n->name;
        cg_cpatch_off[cg_ncpatches] = -(x64_off + 2 + 1); /* negative = func addr patch */
        cg_ncpatches = cg_ncpatches + 1;
        x64_mov_ri64(X64_RAX, 0, 0); /* placeholder 64-bit address */
        return;
    }

    /* Builtin: __syscall(nr, a0, a1, a2, a3, a4, a5)
     * Linux syscall convention: rax=nr, rdi=a0, rsi=a1, rdx=a2, r10=a3, r8=a4, r9=a5 */
    if (n->kind == ND_CALL && n->name && cg_strcmp(n->name, "__syscall") == 0) {
        Node *arg;
        int nargs;
        int sc_regs[7];

        sc_regs[0] = X64_RAX;  /* syscall number */
        sc_regs[1] = X64_RDI;  /* arg 0 */
        sc_regs[2] = X64_RSI;  /* arg 1 */
        sc_regs[3] = X64_RDX;  /* arg 2 */
        sc_regs[4] = X64_R10;  /* arg 3 (NOT rcx — syscall clobbers it) */
        sc_regs[5] = X64_R8;   /* arg 4 */
        sc_regs[6] = X64_R9;   /* arg 5 */

        /* Evaluate all args left-to-right, push to stack */
        nargs = 0;
        arg = n->args;
        while (arg) {
            gen_expr(arg);
            cg_push();
            nargs = nargs + 1;
            arg = arg->next;
        }

        /* Pop args into syscall registers (from stack) */
        i = 0;
        while (i < nargs && i < 7) {
            x64_mov_rm64(sc_regs[i], X64_RSP, (nargs - 1 - i) * 8);
            i = i + 1;
        }
        if (nargs > 0)
            x64_add_ri64(X64_RSP, nargs * 8);

        x64_syscall();
        /* Result in RAX */
        return;
    }

    if (n->kind == ND_CALL || n->kind == ND_CALL_PTR) {
        /* x86-64 SysV: args in rdi, rsi, rdx, rcx, r8, r9; stack for 7+ */
        int nargs;
        int nreg;
        int nstack;
        int arg_reg[6];
        Node *arg;

        arg_reg[0] = X64_RDI;
        arg_reg[1] = X64_RSI;
        arg_reg[2] = X64_RDX;
        arg_reg[3] = X64_RCX;
        arg_reg[4] = X64_R8;
        arg_reg[5] = X64_R9;

        /* Count args */
        nargs = 0;
        arg = n->args;
        while (arg) { nargs = nargs + 1; arg = arg->next; }
        nreg = nargs < 6 ? nargs : 6;
        nstack = nargs > 6 ? nargs - 6 : 0;

        /* Evaluate ALL args left-to-right, push to stack as temp storage */
        arg = n->args;
        while (arg) {
            gen_expr(arg);
            cg_push();
            arg = arg->next;
        }

        /* For indirect call, evaluate callee and save to R10 */
        if (n->kind == ND_CALL_PTR) {
            gen_expr(n->lhs);
            x64_mov_rr64(X64_R10, X64_RAX);
        }

        /* Stack now has nargs values:
         *   [RSP + (nargs-1)*8] = arg0 (first, deepest)
         *   ...
         *   [RSP]               = arg(nargs-1) (last, top) */

        /* Load register args (0..nreg-1) from their stack positions */
        i = 0;
        while (i < nreg) {
            x64_mov_rm64(arg_reg[i], X64_RSP, (nargs - 1 - i) * 8);
            i = i + 1;
        }

        /* For stack args: copy them to their final positions.
         * After we remove temp storage, callee expects stack args at
         * [RSP], [RSP+8], etc. We need to set up that area.
         *
         * Strategy: remove temp storage, allocate stack arg space + pad,
         * then store stack args from registers we saved them to.
         * But we don't have enough registers. Instead, shuffle in place.
         *
         * Simpler: remove all temp, then re-push stack args (right to left).
         * We need to save them before removing. Use: copy stack args to
         * the final position below the temp area. */

        if (nstack > 0) {
            /* Save stack args to R11 one at a time and push.
             * Stack args are indices 6..nargs-1.
             * On stack: arg[j] is at [RSP + (nargs-1-j)*8].
             * Push them in reverse order (last arg first = lowest addr). */
            int sarg;

            /* First, remove all temp storage */
            x64_add_ri64(X64_RSP, nargs * 8);

            /* Alignment padding: nstack pushes must leave RSP 16-aligned.
             * Frame is currently 16-aligned. nstack pushes + padding. */
            if (nstack & 1)
                x64_push(X64_RAX);  /* dummy for alignment */

            /* Re-evaluate and push stack args in reverse (last arg first).
             * This is O(n^2) to walk the arg list but correct. */
            sarg = nargs - 1;
            while (sarg >= 6) {
                arg = n->args;
                i = 0;
                while (i < sarg) { arg = arg->next; i = i + 1; }
                gen_expr(arg);
                cg_push();
                sarg = sarg - 1;
            }
        } else {
            /* No stack args: just remove temp storage */
            if (nargs > 0)
                x64_add_ri64(X64_RSP, nargs * 8);
        }

        if (n->kind == ND_CALL_PTR) {
            x64_call_r(X64_R10);
        } else {
            /* Direct call: record call patch */
            x64_byte(0xE8);
            cg_cpatch_name[cg_ncpatches] = n->name;
            cg_cpatch_off[cg_ncpatches] = x64_off;
            cg_ncpatches = cg_ncpatches + 1;
            x64_dword(0);
        }
        /* Clean up stack args + alignment padding */
        if (nstack > 0) {
            int cleanup;
            cleanup = nstack * 8;
            if (nstack & 1) cleanup = cleanup + 8; /* alignment padding */
            x64_add_ri64(X64_RSP, cleanup);
        }
        /* Result in RAX */
        return;
    }

    /* Fallback */
    cg_li(0);
}

/* ============================================================================
 * Statement generation
 * ============================================================================ */

static void gen_stmt(Node *n) {
    int l1;
    int l2;
    int l3;
    int saved_depth;
    int i;
    int ncase;
    Node *c;

    if (!n) return;

    if (n->kind == ND_EXPR_STMT) {
        gen_expr(n->lhs);
        return;
    }

    if (n->kind == ND_RETURN) {
        if (n->lhs) gen_expr(n->lhs);
        cg_jmp_label(cg_epilog);
        return;
    }

    if (n->kind == ND_IF) {
        l1 = cg_label();
        gen_expr(n->cond);
        x64_test_rr(X64_RAX, X64_RAX);
        cg_jcc_label(X64_CC_E, l1);
        gen_stmt(n->body);
        if (n->els) {
            l2 = cg_label();
            cg_jmp_label(l2);
            cg_ldef(l1);
            gen_stmt(n->els);
            cg_ldef(l2);
        } else {
            cg_ldef(l1);
        }
        return;
    }

    if (n->kind == ND_WHILE) {
        l1 = cg_label();
        l2 = cg_label();
        cg_break_lbl[cg_loop_depth] = l2;
        cg_cont_lbl[cg_loop_depth] = l1;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_expr(n->cond);
        x64_test_rr(X64_RAX, X64_RAX);
        cg_jcc_label(X64_CC_E, l2);
        gen_stmt(n->body);
        cg_jmp_label(l1);
        cg_ldef(l2);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_DO_WHILE) {
        l1 = cg_label();
        l2 = cg_label();
        l3 = cg_label();
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth] = l2;
        cg_loop_depth = cg_loop_depth + 1;
        cg_ldef(l1);
        gen_stmt(n->body);
        cg_ldef(l2);
        gen_expr(n->cond);
        x64_test_rr(X64_RAX, X64_RAX);
        cg_jcc_label(X64_CC_NE, l1);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_FOR) {
        l1 = cg_label();
        l2 = cg_label();
        l3 = cg_label();
        cg_break_lbl[cg_loop_depth] = l3;
        cg_cont_lbl[cg_loop_depth] = l2;
        cg_loop_depth = cg_loop_depth + 1;
        if (n->init) gen_stmt(n->init);
        cg_ldef(l1);
        if (n->cond) {
            gen_expr(n->cond);
            x64_test_rr(X64_RAX, X64_RAX);
            cg_jcc_label(X64_CC_E, l3);
        }
        gen_stmt(n->body);
        cg_ldef(l2);
        if (n->step) gen_expr(n->step);
        cg_jmp_label(l1);
        cg_ldef(l3);
        cg_loop_depth = cg_loop_depth - 1;
        return;
    }

    if (n->kind == ND_SWITCH) {
        l1 = cg_label(); /* break label */
        cg_break_lbl[cg_loop_depth] = l1;
        cg_cont_lbl[cg_loop_depth] = -1;
        cg_loop_depth = cg_loop_depth + 1;

        /* Collect cases */
        saved_depth = cg_sw_depth;
        cg_sw_base[cg_sw_depth] = 0;
        if (cg_sw_depth > 0)
            cg_sw_base[cg_sw_depth] = cg_sw_base[cg_sw_depth - 1] + cg_sw_count[cg_sw_depth - 1];
        cg_sw_count[cg_sw_depth] = 0;
        cg_sw_def[cg_sw_depth] = -1;
        cg_sw_depth = cg_sw_depth + 1;

        /* Pre-scan body for case/default labels */
        c = n->body;
        if (c && c->kind == ND_BLOCK) c = c->body;
        while (c) {
            if (c->kind == ND_CASE) {
                i = cg_sw_base[saved_depth] + cg_sw_count[saved_depth];
                cg_sw_val[i] = c->val;
                cg_sw_lbl[i] = cg_label();
                cg_sw_count[saved_depth] = cg_sw_count[saved_depth] + 1;
            } else if (c->kind == ND_DEFAULT) {
                cg_sw_def[saved_depth] = cg_label();
            }
            c = c->next;
        }

        /* Evaluate switch expression */
        gen_expr(n->cond);
        x64_mov_rr(X64_RDX, X64_RAX);  /* save switch value in EDX */

        /* Compare chain */
        ncase = cg_sw_count[saved_depth];
        i = 0;
        while (i < ncase) {
            int ci;
            ci = cg_sw_base[saved_depth] + i;
            x64_cmp_ri(X64_RDX, cg_sw_val[ci]);
            cg_jcc_label(X64_CC_E, cg_sw_lbl[ci]);
            i = i + 1;
        }

        /* Jump to default or break */
        if (cg_sw_def[saved_depth] >= 0)
            cg_jmp_label(cg_sw_def[saved_depth]);
        else
            cg_jmp_label(l1);

        /* Generate body */
        cg_sw_cur[saved_depth] = 0;
        gen_stmt(n->body);

        cg_ldef(l1);
        cg_loop_depth = cg_loop_depth - 1;
        cg_sw_depth = cg_sw_depth - 1;
        return;
    }

    if (n->kind == ND_CASE) {
        i = cg_sw_base[cg_sw_depth - 1] + cg_sw_cur[cg_sw_depth - 1];
        cg_ldef(cg_sw_lbl[i]);
        cg_sw_cur[cg_sw_depth - 1] = cg_sw_cur[cg_sw_depth - 1] + 1;
        return;
    }

    if (n->kind == ND_DEFAULT) {
        cg_ldef(cg_sw_def[cg_sw_depth - 1]);
        return;
    }

    if (n->kind == ND_BREAK) {
        cg_jmp_label(cg_break_lbl[cg_loop_depth - 1]);
        return;
    }

    if (n->kind == ND_CONTINUE) {
        cg_jmp_label(cg_cont_lbl[cg_loop_depth - 1]);
        return;
    }

    if (n->kind == ND_GOTO) {
        cg_jmp_label(n->val);
        return;
    }

    if (n->kind == ND_LABEL) {
        cg_ldef(n->val);
        if (n->body) gen_stmt(n->body);
        return;
    }

    if (n->kind == ND_BLOCK) {
        c = n->body;
        while (c) {
            gen_stmt(c);
            c = c->next;
        }
        return;
    }
}

/* ============================================================================
 * Function generation
 * ============================================================================ */

/* x86-64 arg registers */
static int x64_arg_regs[6];

static void gen_func(Node *fn) {
    int fs;
    int i;
    Node *p;

    x64_arg_regs[0] = X64_RDI;
    x64_arg_regs[1] = X64_RSI;
    x64_arg_regs[2] = X64_RDX;
    x64_arg_regs[3] = X64_RCX;
    x64_arg_regs[4] = X64_R8;
    x64_arg_regs[5] = X64_R9;

    /* Record function offset */
    cg_func_name[cg_nfuncs] = fn->name;
    cg_func_off[cg_nfuncs] = x64_off;
    cg_nfuncs = cg_nfuncs + 1;

    cg_epilog = cg_label();
    cg_loop_depth = 0;
    cg_sw_depth = 0;

    /* Frame size: 8 (saved rbp is implicit via push rbp) + params + locals
     * The frontend computed locals_size including param storage.
     * Add 8 for saved LR slot (matching SLOW-32 layout: fp-4=LR, fp-8=FP)
     * Total = 8 + locals_size, rounded up to 16-byte alignment.
     */
    /* Double the frame size to account for 8-byte slots on x86-64 */
    fs = 16 + fn->locals_size * 2;
    fs = (fs + 15) & ~15;
    cg_frame_size = fs;

    /* Prologue */
    x64_push(X64_RBP);
    x64_mov_rr64(X64_RBP, X64_RSP);
    if (fs > 0) x64_sub_ri64(X64_RSP, fs);

    /* Save register args to stack.
     *
     * On x86-64, ALL values (including pointers) need 8-byte storage to
     * avoid truncation. The SLOW-32 frontend assigns 4-byte-spaced offsets.
     * We simply double the offset magnitude to get 8-byte spacing.
     * Example: frontend offset -12 → x64 offset -24; -16 → -32; etc.
     *
     * The cg_x64_offset() helper handles this scaling everywhere.
     */
    i = 0;
    p = fn->args;
    while (p && i < 6) {
        x64_mov_mr64(X64_RBP, cg_x64_offset(p->offset), x64_arg_regs[i]);
        i = i + 1;
        p = p->next;
    }
    /* Params 7+ arrive on the caller's stack: [RBP+16], [RBP+24], etc.
     * Copy them to their assigned local slots. */
    while (p) {
        /* Load from caller's stack: [RBP + 16 + (i-6)*8] */
        x64_mov_rm64(X64_RAX, X64_RBP, 16 + (i - 6) * 8);
        x64_mov_mr64(X64_RBP, cg_x64_offset(p->offset), X64_RAX);
        i = i + 1;
        p = p->next;
    }

    /* Generate function body */
    gen_stmt(fn->body);

    /* Epilogue */
    cg_ldef(cg_epilog);
    x64_mov_rr64(X64_RSP, X64_RBP);
    x64_pop(X64_RBP);
    x64_ret();
}

/* ============================================================================
 * Data section building
 * ============================================================================ */

static unsigned char cg_rodata[65536];
static int cg_rodata_len;

static unsigned char cg_data[65536];
static int cg_data_len;

static int cg_bss_size;

/* Offsets within data section for each global */
static int cg_glob_data_off[CG_MAX_GLOBALS];

/* Offsets within rodata section for each string */
static int cg_str_rodata_off[CG_MAX_STRINGS];

static void gen_data_sections(Node *prog) {
    int i;
    int j;

    cg_rodata_len = 0;
    cg_data_len = 0;
    cg_bss_size = 0;

    /* Strings → rodata (from lexer's string pool, including NUL terminator) */
    i = 0;
    while (i < cg_nstrings) {
        cg_str_rodata_off[i] = cg_rodata_len;
        j = 0;
        while (j <= cg_str_len[i]) { /* <= to include NUL terminator */
            if (cg_rodata_len < 65536)
                cg_rodata[cg_rodata_len] = cg_str_pool[cg_str_off[i] + j];
            cg_rodata_len = cg_rodata_len + 1;
            j = j + 1;
        }
        i = i + 1;
    }

    /* Globals → data section.
     * All globals go to data for simplicity (even zero-init ones).
     * String-initialized globals get a pointer relocation. */
    i = 0;
    while (i < cg_nglobals) {
        /* Align to 8 bytes for pointer-sized globals */
        if (cg_glob_size[i] >= 8) {
            while (cg_data_len & 7) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
            }
        } else if (cg_glob_size[i] >= 4) {
            while (cg_data_len & 3) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
            }
        }
        cg_glob_data_off[i] = cg_data_len;
        if (cg_glob_has_init[i] == 2) {
            /* String-initialized pointer: will be patched with rodata address.
             * Record a data-section relocation for this. */
            cg_dreloc_off[cg_ndrelocs] = -(cg_data_len + 1); /* negative = data section offset */
            cg_dreloc_kind[cg_ndrelocs] = DRELOC_STRING;
            cg_dreloc_idx[cg_ndrelocs] = cg_glob_init[i]; /* string pool index */
            cg_ndrelocs = cg_ndrelocs + 1;
            /* Emit 8 zero bytes (placeholder for pointer) */
            j = 0;
            while (j < 8) {
                if (cg_data_len < 65536) cg_data[cg_data_len] = 0;
                cg_data_len = cg_data_len + 1;
                j = j + 1;
            }
        } else {
            /* Emit bytes for this global */
            j = 0;
            while (j < cg_glob_size[i]) {
                if (cg_data_len < 65536) {
                    if (j < 4 && cg_glob_has_init[i] == 1) {
                        cg_data[cg_data_len] = (cg_glob_init[i] >> (j * 8)) & 0xFF;
                    } else {
                        cg_data[cg_data_len] = 0;
                    }
                }
                cg_data_len = cg_data_len + 1;
                j = j + 1;
            }
        }
        i = i + 1;
    }
}

/* ============================================================================
 * Resolve relocations
 * ============================================================================ */

static void resolve_relocations(void) {
    int i;
    int addr;
    int off;
    int rodata_base;
    int data_base;

    rodata_base = elf_rodata_vaddr();
    data_base = elf_data_vaddr();

    /* Resolve data relocations (string and global addresses in code/data) */
    i = 0;
    while (i < cg_ndrelocs) {
        off = cg_dreloc_off[i];

        /* Compute target address */
        if (cg_dreloc_kind[i] == DRELOC_STRING) {
            addr = rodata_base + cg_str_rodata_off[cg_dreloc_idx[i]];
        } else {
            addr = data_base + cg_glob_data_off[cg_dreloc_idx[i]];
        }

        if (off < 0) {
            /* Negative offset = data section relocation (string-init pointer).
             * off is -(data_offset + 1), so data_offset = -(off + 1). */
            int doff;
            doff = -(off + 1);
            cg_data[doff]     = addr & 0xFF;
            cg_data[doff + 1] = (addr >> 8) & 0xFF;
            cg_data[doff + 2] = (addr >> 16) & 0xFF;
            cg_data[doff + 3] = (addr >> 24) & 0xFF;
            cg_data[doff + 4] = 0;
            cg_data[doff + 5] = 0;
            cg_data[doff + 6] = 0;
            cg_data[doff + 7] = 0;
        } else {
            /* Code section relocation: patch imm64 */
            x64_buf[off]     = addr & 0xFF;
            x64_buf[off + 1] = (addr >> 8) & 0xFF;
            x64_buf[off + 2] = (addr >> 16) & 0xFF;
            x64_buf[off + 3] = (addr >> 24) & 0xFF;
            /* High 32 bits zero (addresses < 4GB) */
        }
        i = i + 1;
    }
}

/* ============================================================================
 * Resolve function calls
 * ============================================================================ */

static void resolve_calls(void) {
    int i;
    int j;
    int found;
    int off;
    int text_base;

    text_base = elf_text_vaddr();

    i = 0;
    while (i < cg_ncpatches) {
        found = 0;
        j = 0;
        off = cg_cpatch_off[i];
        while (j < cg_nfuncs) {
            if (cg_strcmp(cg_cpatch_name[i], cg_func_name[j]) == 0) {
                if (off < 0) {
                    /* Negative offset: function address patch (imm64 in movabs).
                     * off = -(code_offset + 1), so code_offset = -(off + 1). */
                    int coff;
                    int addr;
                    coff = -(off + 1);
                    addr = text_base + cg_func_off[j];
                    x64_buf[coff]     = addr & 0xFF;
                    x64_buf[coff + 1] = (addr >> 8) & 0xFF;
                    x64_buf[coff + 2] = (addr >> 16) & 0xFF;
                    x64_buf[coff + 3] = (addr >> 24) & 0xFF;
                    x64_buf[coff + 4] = 0;
                    x64_buf[coff + 5] = 0;
                    x64_buf[coff + 6] = 0;
                    x64_buf[coff + 7] = 0;
                } else {
                    /* Positive offset: call rel32 patch */
                    x64_patch_rel32(off, cg_func_off[j]);
                }
                found = 1;
                break;
            }
            j = j + 1;
        }
        if (!found) {
            /* Unresolved call — NOP out the call instruction (E8 xx xx xx xx → 5-byte NOP) */
            if (off >= 0) {
                x64_buf[off - 1] = 0x0F;  /* 5-byte NOP: 0F 1F 44 00 00 */
                x64_buf[off + 0] = 0x1F;
                x64_buf[off + 1] = 0x44;
                x64_buf[off + 2] = 0x00;
                x64_buf[off + 3] = 0x00;
            }
        }
        i = i + 1;
    }
}

/* ============================================================================
 * Collect globals from AST
 * ============================================================================ */

/* Compute x86-64 size for a global variable.
 * Pointers are 8 bytes on x86-64 (vs 4 on SLOW-32). */
static int cg_x64_global_size(int ty, int s32_size) {
    if (s32_size > 0) {
        /* Array: scale pointer-element arrays by 2x for 8-byte pointers */
        if (ty_is_ptr(ty) && ty_is_ptr(ty_deref(ty))) {
            /* Array of pointers: each element 8 bytes instead of 4 */
            return s32_size * 2;
        }
        return s32_size;
    }
    /* Scalar */
    if (ty_is_ptr(ty)) return 8;
    return ty_size(ty);
}

static void collect_globals(Node *prog) {
    int i;
    int sz;

    cg_nglobals = 0;

    /* Copy parser's global symbol table to codegen's */
    i = 0;
    while (i < ps_nglobals) {
        cg_glob_name[cg_nglobals] = ps_gname[i];

        /* Compute size */
        if (ps_gsize[i] > 0) {
            sz = ps_gsize[i]; /* array: use parser-computed size */
        } else {
            sz = cg_x64_global_size(ps_gtype[i], 0);
        }
        cg_glob_size[cg_nglobals] = sz;

        /* Initialized? */
        if (ps_ginit_start[i] >= 0) {
            /* Has initializer list (array/struct) — handled in gen_data_sections */
            cg_glob_has_init[cg_nglobals] = 1;
            cg_glob_init[cg_nglobals] = 0;
        } else if (ps_gstr[i] >= 0) {
            /* String-initialized (char *name = "...") */
            cg_glob_has_init[cg_nglobals] = 2; /* special: string init */
            cg_glob_init[cg_nglobals] = ps_gstr[i];
        } else if (ps_ginit[i] != 0) {
            /* Scalar with nonzero initial value */
            cg_glob_has_init[cg_nglobals] = 1;
            cg_glob_init[cg_nglobals] = ps_ginit[i];
        } else {
            /* Uninitialized (BSS) */
            cg_glob_has_init[cg_nglobals] = 0;
            cg_glob_init[cg_nglobals] = 0;
        }

        cg_nglobals = cg_nglobals + 1;
        i = i + 1;
    }
}

/* ============================================================================
 * String pool management (called from parser)
 * ============================================================================ */

/* The parser calls ps_add_string() — we need to intercept string creation.
 * Actually, the parser stores strings in its own pool (ps_str_pool).
 * We'll copy them to our pool during gen_data_sections.
 * For now, we rely on the parser's string pool being available.
 */

/* ============================================================================
 * Top-level: gen_program
 * ============================================================================ */

static void gen_program(Node *prog) {
    Node *fn;
    int entry_off;
    int main_off;
    int i;

    /* Initialize state */
    /* Note: cg_lbl may be >0 if the parser allocated goto labels.
     * We do NOT reset cg_lbl — those labels are still valid.
     * Instead, initialize all label offsets to "undefined". */
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

    /* Collect globals from parser's symbol table */
    collect_globals(prog);

    /* Emit _start stub first */
    entry_off = x64_off;

    /* _start:
     *   xor ebp, ebp          ; mark end of frames
     *   mov edi, [rsp]        ; argc (32-bit)
     *   lea rsi, [rsp+8]      ; argv
     *   ; compute envp = argv + (argc+1)*8
     *   movsxd rax, edi        ; sign-extend argc to 64-bit
     *   inc rax                ; argc+1
     *   lea rdx, [rsi+rax*8]   ; envp = argv + (argc+1)*8
     *   ; save rbx/r12 for later use
     *   push rsi               ; save argv
     *   push rdi               ; save argc
     *   ; call __save_envp(envp) — rdi=envp=rdx
     *   mov rdi, rdx
     *   call __save_envp
     *   ; restore argc/argv
     *   pop rdi                ; argc
     *   pop rsi                ; argv
     *   call main
     *   mov edi, eax           ; exit code = main return
     *   mov eax, 60            ; sys_exit
     *   syscall
     */
    x64_xor_rr(X64_RBP, X64_RBP);
    x64_mov_rm(X64_RDI, X64_RSP, 0);           /* edi = argc */
    x64_lea(X64_RSI, X64_RSP, 8);              /* rsi = argv */

    /* movsxd rax, edi (sign-extend argc to 64-bit) */
    x64_movsxd(X64_RAX, X64_RDI);
    /* inc rax */
    x64_add_ri64(X64_RAX, 1);
    /* lea rdx, [rsi + rax*8] */
    /* Can't easily encode SIB. Instead: shl rax,3; add rax,rsi; mov rdx,rax */
    x64_byte(0x48); x64_byte(0xC1); x64_byte(0xE0); x64_byte(0x03); /* shl rax, 3 */
    x64_add_rr64(X64_RAX, X64_RSI);
    x64_mov_rr64(X64_RDX, X64_RAX);            /* rdx = envp */

    x64_push(X64_RSI);                          /* save argv */
    x64_push(X64_RDI);                          /* save argc */

    /* call __save_envp(envp) */
    x64_mov_rr64(X64_RDI, X64_RDX);
    x64_byte(0xE8);
    cg_cpatch_name[cg_ncpatches] = "__save_envp";
    cg_cpatch_off[cg_ncpatches] = x64_off;
    cg_ncpatches = cg_ncpatches + 1;
    x64_dword(0);

    x64_pop(X64_RDI);                           /* argc */
    x64_pop(X64_RSI);                           /* argv */

    /* Call main — record patch */
    x64_byte(0xE8);
    cg_cpatch_name[cg_ncpatches] = "main";
    cg_cpatch_off[cg_ncpatches] = x64_off;
    cg_ncpatches = cg_ncpatches + 1;
    x64_dword(0);

    x64_mov_rr(X64_RDI, X64_RAX);
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    /* Generate all functions */
    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) {
            gen_func(fn);
        }
        fn = fn->next;
    }

    /* Copy string pool from lexer */
    /* The lexer stores strings in lex_strpool / lex_str_off / lex_str_len. */
    /* We figure out how many strings exist by scanning the AST for ND_STRING
     * nodes — the string index is stored in n->val. For now, copy the entire
     * lexer string pool to our rodata pool. We'll figure out the count from
     * the max index seen. Simplified: just copy it all. */
    /* lex_strpool_len tells us how much pool is used */
    i = 0;
    while (i < lex_strpool_len && i < CG_STRING_POOL_SZ) {
        cg_str_pool[i] = lex_strpool[i];
        i = i + 1;
    }
    cg_str_pool_used = lex_strpool_len;
    /* Copy string table entries — we don't know max index, so copy up to 1024 */
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

    /* Set up ELF */
    elf_init();
    elf_set_text(x64_buf, x64_off);
    if (cg_rodata_len > 0) elf_set_rodata(cg_rodata, cg_rodata_len);
    if (cg_data_len > 0) elf_set_data(cg_data, cg_data_len);
    elf_set_entry(entry_off);

    /* Resolve function calls and data relocations (needs ELF layout finalized) */
    resolve_calls();
    resolve_relocations();

    /* Record code size for driver */
    cg_olen = x64_off;
}

/* ============================================================================
 * Division/modulo helper — fixes the broken inline handler above
 * ============================================================================ */

/* This needs to be called from gen_expr for TK_SLASH and TK_PERCENT.
 * But since gen_expr already handled those cases (with bugs), let me
 * fix this by adding a pre-check in the ND_BINOP handler.
 *
 * Actually, the way the code is structured, TK_SLASH fell through to
 * a broken handler. Let me fix this by restructuring gen_expr to
 * handle div/mod BEFORE the generic binary op evaluation.
 */

/* We need to restructure gen_expr. The cleanest fix is to handle
 * division specially before the push/pop pattern. Let me add this
 * as a patch to gen_expr by checking for SLASH/PERCENT early.
 *
 * For now, users should avoid / and % until this is fixed.
 * The core tests (fib, add, etc.) don't need division.
 *
 * TODO: Fix division by handling it before the generic binop path.
 * Pattern: gen_expr(lhs) → push → gen_expr(rhs) → mov r10,rax →
 *          pop rax → cdq → idiv r10 → result in eax(quot) or edx(rem)
 */
