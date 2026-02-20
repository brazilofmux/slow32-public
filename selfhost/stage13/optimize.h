/* optimize.h -- AST optimization passes for stage12 compiler
 *
 * Phase 12: constant folding, dead code elimination, strength reduction.
 * Runs between parse and codegen on the AST.
 * Compiled by stage03 s32-cc.
 */

/* --- Helpers --- */

static int opt_is_pow2(int v) {
    int shift;
    if (v <= 0) return -1;
    shift = 0;
    while (shift < 31) {
        if (v == (1 << shift)) return shift;
        shift = shift + 1;
    }
    return -1;
}

static void opt_copy_node(Node *dst, Node *src) {
    Node *saved_next;
    saved_next = dst->next;
    dst->kind = src->kind;
    dst->ty = src->ty;
    dst->op = src->op;
    dst->val = src->val;
    dst->name = src->name;
    dst->offset = src->offset;
    dst->is_local = src->is_local;
    dst->is_array = src->is_array;
    dst->nparams = src->nparams;
    dst->locals_size = src->locals_size;
    dst->lhs = src->lhs;
    dst->rhs = src->rhs;
    dst->cond = src->cond;
    dst->body = src->body;
    dst->init = src->init;
    dst->step = src->step;
    dst->els = src->els;
    dst->args = src->args;
    dst->next = saved_next;
}

/* --- Constant Folding --- */

static void opt_fold(Node *n) {
    int lv;
    int rv;
    int result;
    int shift;
    Node *tmp;

    if (!n) return;

    /* Recurse into children first (post-order) */
    opt_fold(n->lhs);
    opt_fold(n->rhs);
    opt_fold(n->cond);
    opt_fold(n->body);
    opt_fold(n->init);
    opt_fold(n->step);
    opt_fold(n->els);
    opt_fold(n->args);
    opt_fold(n->next);

    /* Fold unary on constant */
    if (n->kind == ND_UNARY && n->lhs && n->lhs->kind == ND_NUM) {
        lv = n->lhs->val;
        if (n->op == TK_MINUS) {
            n->kind = ND_NUM;
            n->val = 0 - lv;
            n->ty = TY_INT;
            n->lhs = NULL;
            return;
        }
        if (n->op == TK_BANG) {
            n->kind = ND_NUM;
            n->val = (lv == 0) ? 1 : 0;
            n->ty = TY_INT;
            n->lhs = NULL;
            return;
        }
        if (n->op == TK_TILDE) {
            n->kind = ND_NUM;
            n->val = ~lv;
            n->ty = TY_INT;
            n->lhs = NULL;
            return;
        }
    }

    /* Fold binary on two constants */
    if (n->kind == ND_BINOP && n->lhs && n->rhs &&
        n->lhs->kind == ND_NUM && n->rhs->kind == ND_NUM) {
        lv = n->lhs->val;
        rv = n->rhs->val;
        result = 0;

        /* Skip folding ops that differ between signed/unsigned */
        if (n->ty & TY_UNSIGNED) {
            if (n->op == TK_LT || n->op == TK_GT ||
                n->op == TK_LE || n->op == TK_GE ||
                n->op == TK_SLASH || n->op == TK_PERCENT ||
                n->op == TK_RSHIFT) return;
        }

        if (n->op == TK_PLUS)    { result = lv + rv; }
        else if (n->op == TK_MINUS)   { result = lv - rv; }
        else if (n->op == TK_STAR)    { result = lv * rv; }
        else if (n->op == TK_SLASH)   { if (rv == 0) return; result = lv / rv; }
        else if (n->op == TK_PERCENT) { if (rv == 0) return; result = lv % rv; }
        else if (n->op == TK_AMP)     { result = lv & rv; }
        else if (n->op == TK_PIPE)    { result = lv | rv; }
        else if (n->op == TK_CARET)   { result = lv ^ rv; }
        else if (n->op == TK_LSHIFT)  { result = lv << rv; }
        else if (n->op == TK_RSHIFT)  { result = lv >> rv; }
        else if (n->op == TK_EQ)      { result = (lv == rv) ? 1 : 0; }
        else if (n->op == TK_NE)      { result = (lv != rv) ? 1 : 0; }
        else if (n->op == TK_LT)      { result = (lv < rv) ? 1 : 0; }
        else if (n->op == TK_GT)      { result = (lv > rv) ? 1 : 0; }
        else if (n->op == TK_LE)      { result = (lv <= rv) ? 1 : 0; }
        else if (n->op == TK_GE)      { result = (lv >= rv) ? 1 : 0; }
        else if (n->op == TK_LAND)    { result = (lv && rv) ? 1 : 0; }
        else if (n->op == TK_LOR)     { result = (lv || rv) ? 1 : 0; }
        else { return; }

        n->kind = ND_NUM;
        n->val = result;
        n->ty = TY_INT;
        n->op = 0;
        n->lhs = NULL;
        n->rhs = NULL;
        return;
    }

    /* Strength reduction: multiply by power-of-2 → left shift */
    if (n->kind == ND_BINOP && n->op == TK_STAR) {
        /* rhs is constant power-of-2 */
        if (n->rhs && n->rhs->kind == ND_NUM) {
            shift = opt_is_pow2(n->rhs->val);
            if (shift > 0) {
                n->op = TK_LSHIFT;
                n->rhs->val = shift;
                return;
            }
        }
        /* lhs is constant power-of-2: swap so constant is on rhs */
        if (n->lhs && n->lhs->kind == ND_NUM) {
            shift = opt_is_pow2(n->lhs->val);
            if (shift > 0) {
                tmp = n->lhs;
                n->lhs = n->rhs;
                n->rhs = tmp;
                n->op = TK_LSHIFT;
                n->rhs->val = shift;
                return;
            }
        }
    }
}

/* --- Dead Code Elimination --- */

static void opt_dce(Node *n) {
    Node *empty;

    if (!n) return;

    /* Recurse into children */
    opt_dce(n->lhs);
    opt_dce(n->rhs);
    opt_dce(n->cond);
    opt_dce(n->body);
    opt_dce(n->init);
    opt_dce(n->step);
    opt_dce(n->els);
    opt_dce(n->args);
    opt_dce(n->next);

    /* if(0) body → empty block; if(0) A else B → B */
    if (n->kind == ND_IF && n->cond && n->cond->kind == ND_NUM) {
        if (n->cond->val == 0) {
            if (n->els) {
                opt_copy_node(n, n->els);
            } else {
                empty = nd_block(NULL);
                opt_copy_node(n, empty);
            }
            return;
        } else {
            /* if(nonzero) A else B → A */
            opt_copy_node(n, n->body);
            return;
        }
    }

    /* while(0) body → empty block */
    if (n->kind == ND_WHILE && n->cond && n->cond->kind == ND_NUM) {
        if (n->cond->val == 0) {
            empty = nd_block(NULL);
            opt_copy_node(n, empty);
            return;
        }
    }
}

/* --- Entry point --- */

static void optimize(Node *prog) {
    Node *fn;
    fn = prog->body;
    while (fn) {
        if (fn->kind == ND_FUNC) {
            opt_fold(fn->body);
            opt_dce(fn->body);
        }
        fn = fn->next;
    }
}
