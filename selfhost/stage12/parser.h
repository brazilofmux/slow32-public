/* parser.h -- Recursive-descent parser for stage12 compiler
 *
 * Phase 1: int/void/char types, arithmetic, if/while, function calls.
 * Builds an AST from the Ragel lexer token stream.
 * Compiled by stage11 s32-cc.
 */

/* --- Parser state --- */
#define P_MAX_LOCALS 128
#define P_MAX_FUNCS  256

static char *ps_lname[P_MAX_LOCALS];  /* local var names */
static int   ps_loff[P_MAX_LOCALS];   /* local var offsets from fp */
static int   ps_nlocals;
static int   ps_stack;                /* current stack allocation */
static int   ps_nparams;              /* params in current func */

/* Forward declarations */
static struct Node *parse_expr(void);
static struct Node *parse_stmt(void);
static struct Node *parse_assign(void);

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

/* Look up local variable by name. Returns index or -1. */
static int find_local(char *name) {
    int i;
    i = ps_nlocals - 1;
    while (i >= 0) {
        if (strcmp(name, ps_lname[i]) == 0) return i;
        i = i - 1;
    }
    return -1;
}

/* Add a local variable. Returns its offset from fp. */
static int add_local(char *name) {
    int idx;
    if (ps_nlocals >= P_MAX_LOCALS) {
        p_error("too many locals");
        return 0;
    }
    ps_stack = ps_stack + 4;
    idx = ps_nlocals;
    ps_lname[idx] = strdup(name);
    ps_loff[idx] = 0 - ps_stack;
    ps_nlocals = ps_nlocals + 1;
    return ps_loff[idx];
}

/* --- Expression parser (operator precedence climbing) --- */

static struct Node *parse_primary(void) {
    struct Node *n;
    int v;
    char nm[256];
    struct Node *head;
    struct Node *tail;
    struct Node *arg;
    int nargs;
    int li;

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

        /* Variable reference */
        li = find_local(nm);
        if (li < 0) {
            p_error(nm);
            return nd_num(0);
        }
        return nd_var(nm, ps_loff[li]);
    }

    /* Parenthesized expression */
    if (lex_tok == TK_LPAREN) {
        next();
        n = parse_expr();
        expect(TK_RPAREN);
        return n;
    }

    p_error("unexpected token in expression");
    return nd_num(0);
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
    return parse_primary();
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

static struct Node *parse_relational(void) {
    struct Node *n;
    int op;

    n = parse_additive();
    while (lex_tok == TK_LT || lex_tok == TK_GT || lex_tok == TK_LE || lex_tok == TK_GE) {
        op = lex_tok;
        next();
        n = nd_binop(op, n, parse_additive());
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

static struct Node *parse_land(void) {
    struct Node *n;

    n = parse_equality();
    while (lex_tok == TK_LAND) {
        next();
        n = nd_binop(TK_LAND, n, parse_equality());
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

static struct Node *parse_assign(void) {
    struct Node *n;

    n = parse_lor();
    if (lex_tok == TK_ASSIGN) {
        next();
        n = nd_assign(n, parse_assign());
    }
    return n;
}

static struct Node *parse_expr(void) {
    return parse_assign();
}

/* --- Statement parser --- */

static struct Node *parse_block(void);

static struct Node *parse_stmt(void) {
    struct Node *n;
    struct Node *c;
    struct Node *t;
    struct Node *e;
    int bt;
    int off;
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

    /* while statement */
    if (lex_tok == TK_WHILE) {
        next();
        expect(TK_LPAREN);
        c = parse_expr();
        expect(TK_RPAREN);
        t = parse_stmt();
        return nd_while(c, t);
    }

    /* block */
    if (lex_tok == TK_LBRACE) {
        return parse_block();
    }

    /* local variable declaration: int x; or int x = expr; */
    if (is_type()) {
        bt = lex_tok;
        next();
        if (lex_tok != TK_IDENT) {
            p_error("expected identifier in declaration");
            return nd_num(0);
        }
        memcpy(nm, lex_str, lex_slen + 1);
        next();
        off = add_local(nm);
        if (lex_tok == TK_ASSIGN) {
            /* int x = expr; → declare + assign */
            next();
            n = nd_assign(nd_var(nm, off), parse_expr());
            expect(TK_SEMI);
            return nd_expr_stmt(n);
        }
        expect(TK_SEMI);
        /* bare declaration, no initializer → no-op block */
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

static struct Node *parse_func(void) {
    struct Node *fn;
    struct Node *phead;
    struct Node *ptail;
    struct Node *p;
    char nm[256];
    int off;
    int i;

    /* Reset locals for this function */
    i = 0;
    while (i < ps_nlocals) {
        free(ps_lname[i]);
        i = i + 1;
    }
    ps_nlocals = 0;
    ps_stack = 8;  /* reserve 8 bytes: saved r31 + saved r30 */
    ps_nparams = 0;

    /* Parse return type (skip it for now — Phase 1 is untyped) */
    if (!is_type()) {
        p_error("expected type");
        return NULL;
    }
    next();

    /* Function name */
    if (lex_tok != TK_IDENT) {
        p_error("expected function name");
        return NULL;
    }
    memcpy(nm, lex_str, lex_slen + 1);
    next();

    /* Parameters */
    expect(TK_LPAREN);
    phead = NULL;
    ptail = NULL;
    if (lex_tok != TK_RPAREN) {
        /* first param — or (void) meaning no params */
        if (!is_type()) {
            p_error("expected type in params");
            return NULL;
        }
        if (lex_tok == TK_VOID) {
            next();
            if (lex_tok == TK_RPAREN) {
                /* (void) → no params, fall through to expect(TK_RPAREN) */
                goto params_done;
            }
            /* void followed by ident: void *name or similar — treat as param */
        } else {
            next();
        }
        if (lex_tok != TK_IDENT) {
            p_error("expected param name");
            return NULL;
        }
        off = add_local(lex_str);
        p = nd_var(lex_str, off);
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
            next();
            if (lex_tok != TK_IDENT) {
                p_error("expected param name");
                return NULL;
            }
            off = add_local(lex_str);
            p = nd_var(lex_str, off);
            ptail->next = p;
            ptail = p;
            ps_nparams = ps_nparams + 1;
            next();
        }
    }
params_done:
    expect(TK_RPAREN);

    /* Body */
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

    next();  /* prime the first token */

    fhead = NULL;
    ftail = NULL;
    while (lex_tok != TK_EOF) {
        f = parse_func();
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
