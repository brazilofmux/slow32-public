/* Direct mimic of the hl_stmt failure mode reported in Issue #31:

     "n (the Node * parameter) is no longer spilled at fp-12. Without
      the spill, n is reused-then-clobbered for the
      n->lhs && hl_struct_ret && ty_is_struct(...) short-circuit chain,
      and the `else if (n->lhs)` path reads *(0+52) instead of *(n+52)."

   Pattern: a Node * parameter used through a 3-term short-circuit
   that includes a function call (which clobbers caller-saved regs),
   with the same parameter re-dereferenced after the chain. */

struct Node {
    int kind;
    struct Node *lhs;
    struct Node *rhs;
    int extra;
};

int g_count;

int helper(struct Node *n) {
    g_count = g_count + 1;
    if (n) {
        return n->kind;
    }
    return 0;
}

int classify(struct Node *n) {
    if (n->lhs && helper(n) && n->rhs->kind > 0) {
        return n->extra;
    } else if (n->lhs) {
        return n->lhs->kind;
    }
    return -1;
}

int main(void) {
    struct Node a;
    struct Node b;
    struct Node c;
    a.kind = 5; a.lhs = 0; a.rhs = 0; a.extra = 100;
    b.kind = 7; b.lhs = 0; b.rhs = 0; b.extra = 200;
    c.kind = 9; c.lhs = &a; c.rhs = &b; c.extra = 300;
    g_count = 0;
    return classify(&c) - 300;
}
