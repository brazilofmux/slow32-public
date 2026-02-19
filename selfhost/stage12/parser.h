/* parser.h -- Recursive-descent parser for stage12 compiler
 *
 * Phase 2: types, pointers, strings, globals, local arrays.
 * Builds a typed AST from the Ragel lexer token stream.
 * Compiled by stage11 s32-cc.
 */

/* --- Parser state --- */
#define P_MAX_LOCALS  128
#define P_MAX_GLOBALS 512

static char *ps_lname[P_MAX_LOCALS];  /* local var names */
static int   ps_loff[P_MAX_LOCALS];   /* local var offsets from fp */
static int   ps_ltype[P_MAX_LOCALS];  /* local var types */
static int   ps_larr[P_MAX_LOCALS];   /* 1 if array (addr, no load) */
static int   ps_nlocals;
static int   ps_stack;                /* current stack allocation */
static int   ps_nparams;              /* params in current func */

static char *ps_gname[P_MAX_GLOBALS]; /* global var names */
static int   ps_gtype[P_MAX_GLOBALS]; /* global var types */
static int   ps_gsize[P_MAX_GLOBALS]; /* size in bytes (0=scalar, >0=array) */
static int   ps_nglobals;

/* Forward declarations */
static struct Node *parse_expr(void);
static struct Node *parse_stmt(void);
static struct Node *parse_assign(void);
static struct Node *parse_postfix(void);

/* --- Utilities --- */

static void next(void) {
    lex_next();
}

static void p_error(char *msg) {
    fputs("s12cc:", stderr);
    fput_uint(stderr, lex_line);
    fputs(": error: ", stderr);
    fputs(msg, stderr);
    fputc(10, stderr);
    exit(1);
}

static void expect(int tok) {
    if (lex_tok != tok) {
        fputs("s12cc:", stderr);
        fput_uint(stderr, lex_line);
        fputs(": expected token ", stderr);
        fput_uint(stderr, tok);
        fputs(" got ", stderr);
        fput_uint(stderr, lex_tok);
        fputc(10, stderr);
        exit(1);
    }
    next();
}

static int is_type(void) {
    if (lex_tok == TK_INT) return 1;
    if (lex_tok == TK_VOID) return 1;
    if (lex_tok == TK_CHAR) return 1;
    return 0;
}

/* Parse a type: int, char, void, with optional pointer stars */
static int parse_type(void) {
    int ty;
    if (lex_tok == TK_INT)  { ty = TY_INT;  next(); }
    else if (lex_tok == TK_CHAR) { ty = TY_CHAR; next(); }
    else if (lex_tok == TK_VOID) { ty = TY_VOID; next(); }
    else { p_error("expected type"); return TY_INT; }
    while (lex_tok == TK_STAR) { ty = ty + TY_PTR; next(); }
    return ty;
}

/* --- Variable lookup --- */

static int find_local(char *name) {
    int i;
    i = ps_nlocals - 1;
    while (i >= 0) {
        if (strcmp(name, ps_lname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

static int find_global(char *name) {
    int i;
    i = ps_nglobals - 1;
    while (i >= 0) {
        if (strcmp(name, ps_gname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

static int add_local(char *name, int ty) {
    int idx;
    int sz;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    sz = 4;  /* all locals occupy word-aligned slots */
    ps_stack = ps_stack + sz;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_loff[idx] = 0 - ps_stack;
    ps_ltype[idx] = ty;
    ps_larr[idx] = 0;
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

/* Add a local array. Returns offset of start of array. */
static int add_local_array(char *name, int elem_ty, int count) {
    int idx;
    int total;
    int elem_sz;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    elem_sz = ty_size(elem_ty);
    total = elem_sz * count;
    /* Round up to multiple of 4 */
    total = ((total + 3) / 4) * 4;
    ps_stack = ps_stack + total;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_loff[idx] = 0 - ps_stack;
    ps_ltype[idx] = elem_ty + TY_PTR;  /* array decays to pointer */
    ps_larr[idx] = 1;
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

static int add_global(char *name, int ty, int size_bytes) {
    int idx;
    if (ps_nglobals >= P_MAX_GLOBALS) {
        p_error("too many globals");
        return 0;
    }
    idx = ps_nglobals;
    ps_gname[idx] = strdup(name);
    ps_gtype[idx] = ty;
    ps_gsize[idx] = size_bytes;
    ps_nglobals = ps_nglobals + 1;
    return idx;
}

/* --- Expression parser (operator precedence climbing) --- */

static struct Node *parse_primary(void) {
    struct Node *n;
    int v;
    int ty;
    char nm[256];
    struct Node *head;
    struct Node *tail;
    struct Node *arg;
    int nargs;
    int li;
    int gi;

    /* Number literal */
    if (lex_tok == TK_NUM) {
        v = lex_val;
        next();
        return nd_num(v);
    }

    /* Character literal */
    if (lex_tok == TK_CHARLIT) {
        v = lex_val;
        next();
        return nd_num(v);
    }

    /* String literal */
    if (lex_tok == TK_STRING) {
        v = lex_val;  /* string pool index */
        next();
        return nd_string(v);
    }

    /* Identifier: variable or function call */
    if (lex_tok == TK_IDENT) {
        memcpy(nm, lex_str, lex_slen + 1);
        next();

        /* Function call */
        if (lex_tok == TK_LPAREN) {
            next();
            head = NULL;
            tail = NULL;
            nargs = 0;
            if (lex_tok != TK_RPAREN) {
                arg = parse_assign();
                nargs = 1;
                head = arg;
                tail = arg;
                while (lex_tok == TK_COMMA) {
                    next();
                    arg = parse_assign();
                    tail->next = arg;
                    tail = arg;
                    nargs = nargs + 1;
                }
            }
            expect(TK_RPAREN);
            return nd_call(nm, head, nargs);
        }

        /* Local variable */
        li = find_local(nm);
        if (li >= 0) {
            n = nd_var(nm, ps_loff[li], ps_ltype[li]);
            n->is_local = 1;
            n->is_array = ps_larr[li];
            return n;
        }

        /* Global variable */
        gi = find_global(nm);
        if (gi >= 0) {
            n = nd_var(nm, 0, ps_gtype[gi]);
            n->is_local = 0;
            n->is_array = (ps_gsize[gi] > 0) ? 1 : 0;
            return n;
        }

        fputs("s12cc:", stderr);
        fput_uint(stderr, lex_line);
        fputs(": undefined: ", stderr);
        fputs(nm, stderr);
        fputc(10, stderr);
        exit(1);
        return nd_num(0);
    }

    /* Parenthesized expression or type cast */
    if (lex_tok == TK_LPAREN) {
        next();
        if (is_type()) {
            ty = parse_type();
            expect(TK_RPAREN);
            n = parse_unary();
            return nd_cast(n, ty);
        }
        n = parse_expr();
        expect(TK_RPAREN);
        return n;
    }

    /* sizeof(type) — basic support */
    if (lex_tok == TK_SIZEOF) {
        next();
        expect(TK_LPAREN);
        if (is_type()) {
            v = ty_size(parse_type());
        } else {
            n = parse_expr();
            v = ty_size(n->ty);
        }
        expect(TK_RPAREN);
        return nd_num(v);
    }

    p_error("unexpected token in expression");
    return nd_num(0);
}

/* Postfix: handle array subscript p[i], postfix ++/-- */
static struct Node *parse_postfix(void) {
    struct Node *n;
    struct Node *idx;
    struct Node *pi;

    n = parse_primary();
    while (lex_tok == TK_LBRACK || lex_tok == TK_INC || lex_tok == TK_DEC) {
        if (lex_tok == TK_LBRACK) {
            next();
            idx = parse_expr();
            expect(TK_RBRACK);
            /* n[idx] → *(n + idx)  — codegen handles pointer arithmetic scaling */
            n = nd_binop(TK_PLUS, n, idx);
            n = nd_unary(TK_STAR, n);
        } else if (lex_tok == TK_INC) {
            next();
            pi = nd_new(ND_POST_INC);
            pi->lhs = n;
            pi->ty = n->ty;
            n = pi;
        } else {
            next();
            pi = nd_new(ND_POST_DEC);
            pi->lhs = n;
            pi->ty = n->ty;
            n = pi;
        }
    }
    return n;
}

static struct Node *parse_unary(void) {
    struct Node *n;

    if (lex_tok == TK_MINUS) {
        next();
        n = parse_unary();
        return nd_unary(TK_MINUS, n);
    }
    if (lex_tok == TK_BANG) {
        next();
        n = parse_unary();
        return nd_unary(TK_BANG, n);
    }
    /* Bitwise NOT */
    if (lex_tok == TK_TILDE) {
        next();
        n = parse_unary();
        return nd_unary(TK_TILDE, n);
    }
    /* Prefix ++ (desugar to compound assign) */
    if (lex_tok == TK_INC) {
        next();
        n = parse_unary();
        return nd_comp_assign(TK_PLUS, n, nd_num(1));
    }
    /* Prefix -- (desugar to compound assign) */
    if (lex_tok == TK_DEC) {
        next();
        n = parse_unary();
        return nd_comp_assign(TK_MINUS, n, nd_num(1));
    }
    /* Dereference */
    if (lex_tok == TK_STAR) {
        next();
        n = parse_unary();
        return nd_unary(TK_STAR, n);
    }
    /* Address-of */
    if (lex_tok == TK_AMP) {
        next();
        n = parse_unary();
        return nd_unary(TK_AMP, n);
    }
    return parse_postfix();
}

static struct Node *parse_multiplicative(void) {
    struct Node *n;
    int op;

    n = parse_unary();
    while (lex_tok == TK_STAR || lex_tok == TK_SLASH || lex_tok == TK_PERCENT) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_unary());
    }
    return n;
}

static struct Node *parse_additive(void) {
    struct Node *n;
    int op;

    n = parse_multiplicative();
    while (lex_tok == TK_PLUS || lex_tok == TK_MINUS) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_multiplicative());
    }
    return n;
}

static struct Node *parse_shift(void) {
    struct Node *n;
    int op;

    n = parse_additive();
    while (lex_tok == TK_LSHIFT || lex_tok == TK_RSHIFT) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_additive());
    }
    return n;
}

static struct Node *parse_relational(void) {
    struct Node *n;
    int op;

    n = parse_shift();
    while (lex_tok == TK_LT || lex_tok == TK_GT || lex_tok == TK_LE || lex_tok == TK_GE) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_shift());
    }
    return n;
}

static struct Node *parse_equality(void) {
    struct Node *n;
    int op;

    n = parse_relational();
    while (lex_tok == TK_EQ || lex_tok == TK_NE) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_relational());
    }
    return n;
}

static struct Node *parse_band(void) {
    struct Node *n;

    n = parse_equality();
    while (lex_tok == TK_AMP) {
        next();
        n = nd_binop(TK_AMP, n, parse_equality());
    }
    return n;
}

static struct Node *parse_bxor(void) {
    struct Node *n;

    n = parse_band();
    while (lex_tok == TK_CARET) {
        next();
        n = nd_binop(TK_CARET, n, parse_band());
    }
    return n;
}

static struct Node *parse_bor(void) {
    struct Node *n;

    n = parse_bxor();
    while (lex_tok == TK_PIPE) {
        next();
        n = nd_binop(TK_PIPE, n, parse_bxor());
    }
    return n;
}

static struct Node *parse_land(void) {
    struct Node *n;

    n = parse_bor();
    while (lex_tok == TK_LAND) {
        next();
        n = nd_binop(TK_LAND, n, parse_bor());
    }
    return n;
}

static struct Node *parse_lor(void) {
    struct Node *n;

    n = parse_land();
    while (lex_tok == TK_LOR) {
        next();
        n = nd_binop(TK_LOR, n, parse_land());
    }
    return n;
}

static struct Node *parse_conditional(void) {
    struct Node *n;
    struct Node *then_e;
    struct Node *else_e;

    n = parse_lor();
    if (lex_tok == TK_QMARK) {
        next();
        then_e = parse_expr();
        expect(TK_COLON);
        else_e = parse_conditional();
        n = nd_ternary(n, then_e, else_e);
    }
    return n;
}

static struct Node *parse_assign(void) {
    struct Node *n;
    int op;

    n = parse_conditional();
    if (lex_tok == TK_ASSIGN) {
        next();
        return nd_assign(n, parse_assign());
    }
    /* Compound assignment operators */
    op = 0;
    if (lex_tok == TK_PLUSEQ)    { op = TK_PLUS;    }
    if (lex_tok == TK_MINUSEQ)   { op = TK_MINUS;   }
    if (lex_tok == TK_STAREQ)    { op = TK_STAR;    }
    if (lex_tok == TK_SLASHEQ)   { op = TK_SLASH;   }
    if (lex_tok == TK_PERCENTEQ) { op = TK_PERCENT; }
    if (lex_tok == TK_AMPEQ)     { op = TK_AMP;     }
    if (lex_tok == TK_PIPEEQ)    { op = TK_PIPE;    }
    if (lex_tok == TK_CARETEQ)   { op = TK_CARET;   }
    if (lex_tok == TK_LSHIFTEQ)  { op = TK_LSHIFT;  }
    if (lex_tok == TK_RSHIFTEQ)  { op = TK_RSHIFT;  }
    if (op) {
        next();
        return nd_comp_assign(op, n, parse_assign());
    }
    return n;
}

static struct Node *parse_expr(void) {
    struct Node *n;

    n = parse_assign();
    while (lex_tok == TK_COMMA) {
        next();
        n = nd_comma(n, parse_assign());
    }
    return n;
}

/* --- Statement parser --- */

static struct Node *parse_block(void);

static struct Node *parse_stmt(void) {
    struct Node *n;
    struct Node *c;
    struct Node *t;
    struct Node *e;
    int ty;
    int off;
    int count;
    char nm[256];

    /* return statement */
    if (lex_tok == TK_RETURN) {
        next();
        if (lex_tok == TK_SEMI) {
            next();
            return nd_return(NULL);
        }
        n = parse_expr();
        expect(TK_SEMI);
        return nd_return(n);
    }

    /* if statement */
    if (lex_tok == TK_IF) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        e = NULL;
        if (lex_tok == TK_ELSE) {
            next();
            e = parse_stmt();
        }
        return nd_if(c, t, e);
    }

    /* do/while statement */
    if (lex_tok == TK_DO) {
        next();
        t = parse_stmt();
        if (lex_tok != TK_WHILE) {
            p_error("expected 'while' after do body");
        }
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        expect(TK_SEMI);
        return nd_do_while(c, t);
    }

    /* while statement */
    if (lex_tok == TK_WHILE) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        return nd_while(c, t);
    }

    /* for statement (first-class node) */
    if (lex_tok == TK_FOR) {
        next();
        expect(TK_LPAREN);
        /* init */
        if (lex_tok == TK_SEMI) {
            next();
            n = NULL;
        } else {
            n = parse_expr();
            expect(TK_SEMI);
        }
        /* cond */
        if (lex_tok == TK_SEMI) {
            c = nd_num(1);
            next();
        } else {
            c = parse_expr();
            expect(TK_SEMI);
        }
        /* step */
        if (lex_tok == TK_RPAREN) {
            e = NULL;
            next();
        } else {
            e = parse_expr();
            expect(TK_RPAREN);
        }
        /* body */
        t = parse_stmt();
        return nd_for(n, c, e, t);
    }

    /* break statement */
    if (lex_tok == TK_BREAK) {
        next();
        expect(TK_SEMI);
        return nd_new(ND_BREAK);
    }

    /* continue statement */
    if (lex_tok == TK_CONTINUE) {
        next();
        expect(TK_SEMI);
        return nd_new(ND_CONTINUE);
    }

    /* block */
    if (lex_tok == TK_LBRACE) {
        return parse_block();
    }

    /* local variable declaration */
    if (is_type()) {
        ty = parse_type();
        if (lex_tok != TK_IDENT) {
            p_error("expected identifier in declaration");
            return nd_num(0);
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();

        /* Array declaration: type name[N]; */
        if (lex_tok == TK_LBRACK) {
            next();
            if (lex_tok != TK_NUM) {
                p_error("expected array size");
                return nd_num(0);
            }
            count = lex_val;
            next();
            expect(TK_RBRACK);
            off = add_local_array(nm, ty, count);
            expect(TK_SEMI);
            return nd_block(NULL);
        }

        /* Scalar: type name; or type name = expr; */
        off = add_local(nm, ty);
        if (lex_tok == TK_ASSIGN) {
            next();
            n = nd_assign(nd_var(nm, off, ty), parse_expr());
            n->lhs->is_local = 1;
            expect(TK_SEMI);
            return nd_expr_stmt(n);
        }
        expect(TK_SEMI);
        return nd_block(NULL);
    }

    /* expression statement */
    n = parse_expr();
    expect(TK_SEMI);
    return nd_expr_stmt(n);
}

static struct Node *parse_block(void) {
    struct Node *head;
    struct Node *tail;
    struct Node *s;

    expect(TK_LBRACE);
    head = NULL;
    tail = NULL;
    while (lex_tok != TK_RBRACE && lex_tok != TK_EOF) {
        s = parse_stmt();
        if (head == NULL) {
            head = s;
            tail = s;
        } else {
            tail->next = s;
            tail = s;
        }
    }
    expect(TK_RBRACE);
    return nd_block(head);
}

/* --- Top-level parser --- */

/* Parse type + name + optional pointer stars + params.
 * Handles: function definitions, function prototypes, global variables. */
static void parse_type_and_stars(int *out_ty) {
    *out_ty = parse_type();
}

static struct Node *parse_top_decl(void) {
    struct Node *fn;
    struct Node *phead;
    struct Node *ptail;
    struct Node *p;
    char nm[256];
    int ty;
    int pty;
    int off;
    int i;
    int count;

    /* Parse return type / variable type */
    ty = parse_type();

    /* Name */
    if (lex_tok != TK_IDENT) {
        p_error("expected name in declaration");
        return NULL;
    }
    memcpy(nm, lex_str, lex_slen + 1);
    next();

    /* Global variable: type name; or type name[N]; */
    if (lex_tok == TK_SEMI) {
        next();
        add_global(nm, ty, 0);
        return NULL;
    }
    if (lex_tok == TK_LBRACK) {
        next();
        if (lex_tok != TK_NUM) {
            p_error("expected array size");
            return NULL;
        }
        count = lex_val;
        next();
        expect(TK_RBRACK);
        expect(TK_SEMI);
        add_global(nm, ty + TY_PTR, ty_size(ty) * count);
        return NULL;
    }

    /* Function: type name(params) { body } or type name(params); */
    expect(TK_LPAREN);

    /* Reset locals for this function */
    i = 0;
    while (i < ps_nlocals) {
        free(ps_lname[i]);
        i = i + 1;
    }
    ps_nlocals = 0;
    ps_stack = 8;  /* reserve 8 bytes: saved r31 + saved r30 */
    ps_nparams = 0;

    /* Parameters */
    phead = NULL;
    ptail = NULL;
    if (lex_tok != TK_RPAREN) {
        /* first param — or (void) meaning no params */
        if (!is_type()) {
            p_error("expected type in params");
            return NULL;
        }
        if (lex_tok == TK_VOID) {
            pty = parse_type();
            if (lex_tok == TK_RPAREN) {
                /* (void) → no params */
                goto params_done;
            }
        } else {
            pty = parse_type();
        }
        if (lex_tok != TK_IDENT) {
            p_error("expected param name");
            return NULL;
        }
        off = add_local(lex_str, pty);
        p = nd_var(lex_str, off, pty);
        p->is_local = 1;
        ps_nparams = 1;
        phead = p;
        ptail = p;
        next();

        while (lex_tok == TK_COMMA) {
            next();
            if (!is_type()) {
                p_error("expected type in params");
                return NULL;
            }
            pty = parse_type();
            if (lex_tok != TK_IDENT) {
                p_error("expected param name");
                return NULL;
            }
            off = add_local(lex_str, pty);
            p = nd_var(lex_str, off, pty);
            p->is_local = 1;
            ptail->next = p;
            ptail = p;
            ps_nparams = ps_nparams + 1;
            next();
        }
    }
params_done:
    expect(TK_RPAREN);

    /* Prototype: type name(params); */
    if (lex_tok == TK_SEMI) {
        next();
        /* Just skip — we don't need prototypes in a single-pass compiler */
        return NULL;
    }

    /* Function body */
    fn = nd_new(ND_FUNC);
    fn->name = strdup(nm);
    fn->args = phead;
    fn->nparams = ps_nparams;
    fn->body = parse_block();
    fn->locals_size = ps_stack;

    return fn;
}

static struct Node *parse_program(void) {
    struct Node *prog;
    struct Node *fhead;
    struct Node *ftail;
    struct Node *f;

    ps_nglobals = 0;
    next();  /* prime the first token */

    fhead = NULL;
    ftail = NULL;
    while (lex_tok != TK_EOF) {
        f = parse_top_decl();
        if (f) {
            if (fhead == NULL) {
                fhead = f;
                ftail = f;
            } else {
                ftail->next = f;
                ftail = f;
            }
        }
    }

    prog = nd_new(ND_PROGRAM);
    prog->body = fhead;
    return prog;
}
