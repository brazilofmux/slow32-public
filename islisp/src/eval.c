/* eval.c -- forms and evaluation (§12), control structure (§14).
 *
 * A direct evaluator over the read forms with a tail-call loop: the
 * last form of a body, the chosen branch of if/cond/case, and the body
 * of a called closure are continued in the same C activation, so a
 * tail-recursive Lisp function does not grow the C stack.  Macros are
 * expanded at each evaluation; caching is a measurement away.
 *
 * Non-local exits are a chain of setjmp frames with dynamic extent.
 * block and tagbody bind their names lexically: the environment maps
 * the name to the frame's tag object, so return-from and go find the
 * right frame even when a same-named block is active deeper in.
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>

enum {
    SP_QUOTE = 1, SP_IF, SP_PROGN, SP_SETQ, SP_LET, SP_LETSTAR, SP_LAMBDA, SP_FUNCTION,
    SP_COND, SP_CASE, SP_CASE_USING, SP_AND, SP_OR, SP_WHILE, SP_FOR, SP_BLOCK,
    SP_RETURN_FROM, SP_CATCH, SP_THROW, SP_TAGBODY, SP_GO, SP_UNWIND_PROTECT,
    SP_DYNAMIC, SP_DYNAMIC_LET, SP_SET_DYNAMIC, SP_SETF, SP_LABELS, SP_FLET,
    SP_THE, SP_ASSURE, SP_CONVERT, SP_CLASS, SP_WITH_HANDLER, SP_IGNORE_ERRORS,
    SP_WITH_STANDARD_INPUT, SP_WITH_STANDARD_OUTPUT, SP_WITH_ERROR_OUTPUT,
    SP_WITH_OPEN_INPUT_FILE, SP_WITH_OPEN_OUTPUT_FILE, SP_WITH_OPEN_IO_FILE,
    SP_QUASIQUOTE, SP_DEFUN, SP_DEFMACRO, SP_DEFGLOBAL, SP_DEFCONSTANT, SP_DEFDYNAMIC,
    SP_DEFCLASS, SP_DEFGENERIC, SP_DEFMETHOD, SP_CALL_NEXT_METHOD, SP_NEXT_METHOD_P
};

val global_env;
val cur_handler;          /* list of handlers, innermost first */
long eval_depth;
nlx_t *nlx_top;

val *dyn_syms, *dyn_olds;
long dyn_sp;
static long dyn_cap = 65536;

static val sym_ignore_mark;

/* hooks into other units */
val sf_convert(val obj, val clsname, val env);
val sf_defclass(val form, val env);
val sf_defgeneric(val form, val env);
val sf_defmethod(val form, val env);
val sf_with_open_file(int kind, val form, val env);
val setf_writer_for(val accessor);
val class_create(val cls, int nargs, val *initargs);

void defspecial(const char *name, int id) { SYM(intern_c(name))->special = id; }

/* ---------- environments ---------- */

static val find_binding(val alist, val sym) {
    for (; IS_CONS(alist); alist = CDR(alist))
        if (CAR(CAR(alist)) == sym) return CAR(alist);
    return 0;
}

static val env_var_binding(val env, val sym) {
    val b;
    for (; env != NIL; env = ENV(env)->parent)
        if ((b = find_binding(ENV(env)->vars, sym)) != 0) return b;
    return 0;
}

static val env_fun_binding(val env, val sym) {
    val b;
    for (; env != NIL; env = ENV(env)->parent)
        if ((b = find_binding(ENV(env)->funs, sym)) != 0) return b;
    return 0;
}

static val env_tag_binding(val env, val sym) {
    val b;
    for (; env != NIL; env = ENV(env)->parent)
        if ((b = find_binding(ENV(env)->tags, sym)) != 0) return b;
    return 0;
}

static void env_bind_var(val env, val sym, val value) {
    val cell = mk_cons(sym, value);
    ENV(env)->vars = mk_cons(cell, ENV(env)->vars);
}

static void env_bind_fun(val env, val sym, val fn) {
    val cell = mk_cons(sym, fn);
    ENV(env)->funs = mk_cons(cell, ENV(env)->funs);
}

static void env_bind_tag(val env, val sym, val tag) {
    val cell = mk_cons(sym, tag);
    ENV(env)->tags = mk_cons(cell, ENV(env)->tags);
}

val symbol_function(val sym) { return SYM(sym)->fn; }

static val lookup_var(val sym, val env) {
    val b = env_var_binding(env, sym);
    if (b) return CDR(b);
    if (SYM(sym)->value != UNBOUND) return SYM(sym)->value;
    err_unbound_variable(sym);
    return NIL;
}

static val lookup_fun(val sym, val env) {
    val b = env_fun_binding(env, sym);
    if (b) return CDR(b);
    if (SYM(sym)->fn != UNBOUND) return SYM(sym)->fn;
    err_undefined_function(sym);
    return NIL;
}

/* ---------- dynamic variables ---------- */

void dyn_bind(val sym, val value) {
    if (dyn_sp >= dyn_cap) fatal("dynamic binding stack exhausted");
    dyn_syms[dyn_sp] = sym;
    dyn_olds[dyn_sp] = SYM(sym)->dyn;
    dyn_sp++;
    SYM(sym)->dyn = value;
}

void dyn_unwind_to(long sp) {
    while (dyn_sp > sp) {
        dyn_sp--;
        SYM(dyn_syms[dyn_sp])->dyn = dyn_olds[dyn_sp];
    }
}

/* ---------- non-local exits ---------- */

void nlx_push(nlx_t *f, int kind, val tag) {
    f->kind = kind;
    f->tag = tag;
    f->value = NIL;
    f->prev = nlx_top;
    f->dyn_sp = dyn_sp;
    f->gc_sp = gc_sp;
    f->eval_depth = eval_depth;
    f->handler = cur_handler;
    f->active = 1;
    f->pending = 0;
    f->pending_value = NIL;
    f->cleanup = NIL;
    f->env = NIL;
    nlx_top = f;
}

void nlx_pop(nlx_t *f) {
    nlx_top = f->prev;
    f->active = 0;
}

/* Transfer control to target with value, running the unwind-protect
 * cleanups and restoring the dynamic state of every frame crossed. */
void nlx_throw(nlx_t *target, val value) {
    nlx_t *f;
    for (f = nlx_top; f && f != target; f = f->prev) {
        if (f->kind == NLX_UNWIND) {
            /* Land here first; its code runs the cleanup, then resumes the
             * transfer.  Every frame between here and the target is being
             * left: a throw or go to one of them from the cleanup is a
             * control error (§14.7.2), so they are disestablished now. */
            nlx_t *g;
            for (g = f->prev; g && g != target; g = g->prev) g->active = 0;
            f->pending = target;
            f->pending_value = value;
            nlx_top = f;
            dyn_unwind_to(f->dyn_sp);
            gc_sp = f->gc_sp;
            eval_depth = f->eval_depth;
            cur_handler = f->handler;
            longjmp(f->jb, 1);
        }
    }
    if (!f) fatal("non-local exit to a frame that is no longer established");
    nlx_top = target;
    target->value = value;
    dyn_unwind_to(target->dyn_sp);
    gc_sp = target->gc_sp;
    eval_depth = target->eval_depth;
    cur_handler = target->handler;
    longjmp(target->jb, 1);
}

static nlx_t *find_frame_by_tag(int kind, val tag) {
    nlx_t *f;
    for (f = nlx_top; f; f = f->prev)
        if (f->kind == kind && f->tag == tag && f->active) return f;
    return 0;
}

/* ---------- application ---------- */

static val bind_params(val fn, int argc, val *argv) {
    closure_t *c = CLO(fn);
    val env;
    val p;
    int i;
    long sp = gc_sp;
    if (argc < c->nparams || (c->rest == NIL && argc > c->nparams)) err_arity(fn, argc);
    env = mk_env(c->env);
    gc_push(env);
    for (p = c->params, i = 0; IS_CONS(p); p = CDR(p), i++) env_bind_var(env, CAR(p), argv[i]);
    if (c->rest != NIL) {
        val rest = NIL;
        long slot = gc_push(NIL);
        for (i = argc - 1; i >= c->nparams; i--) { rest = mk_cons(argv[i], rest); gc_roots[slot] = rest; }
        env_bind_var(env, c->rest, rest);
    }
    gc_sp = sp;
    return env;
}

val apply(val fn, int argc, val *argv) {
    if (!IS_PTR(fn)) err_domain(fn, "<function>");
    switch (TAG(fn)) {
    case T_BUILTIN: {
        builtin_t *b = BI(fn);
        if (argc < b->minargs || (b->maxargs >= 0 && argc > b->maxargs)) err_arity(fn, argc);
        return b->fn(argc, argv);
    }
    case T_CLOSURE: {
        val env = bind_params(fn, argc, argv);
        val r;
        long sp = gc_sp;
        gc_push(env);
        r = eval_body(CLO(fn)->body, env);
        gc_sp = sp;
        return r;
    }
    case T_GENERIC:
        return generic_call(fn, argc, argv);
    default:
        err_domain(fn, "<function>");
        return NIL;
    }
}

val funcall1(val fn, val a) {
    long sp = gc_sp;
    long base = gc_push(a);
    val r = apply(fn, 1, &gc_roots[base]);
    gc_sp = sp;
    return r;
}

val funcall2(val fn, val a, val b) {
    long sp = gc_sp;
    long base = gc_push(a);
    gc_push(b);
    {
        val r = apply(fn, 2, &gc_roots[base]);
        gc_sp = sp;
        return r;
    }
}

/* Evaluate a list of argument forms onto the root stack; returns the base index. */
static long eval_args(val args, val env, int *argc) {
    long base = gc_sp;
    int n = 0;
    long slot = gc_push(args);          /* keep the remaining forms alive */
    long envslot = gc_push(env);
    (void)envslot;
    for (; IS_CONS(args); args = CDR(args)) {
        val v;
        gc_roots[slot] = args;
        v = eval(CAR(args), env);
        gc_push(v);
        n++;
    }
    if (args != NIL) isl_error("<program-error>", NULL);
    /* compact: move the values down over the two bookkeeping slots */
    memmove(&gc_roots[base], &gc_roots[base + 2], (size_t)n * sizeof(val));
    gc_sp = base + n;
    *argc = n;
    return base;
}

/* ---------- lambda lists ---------- */

static void parse_lambda_list(val ll, val *params, val *rest) {
    val p = NIL;
    long sp = gc_sp;
    long slot = gc_push(NIL);
    val l;
    *rest = NIL;
    if (list_length(ll) < 0) err_domain(ll, "<list>");
    for (l = ll; IS_CONS(l); l = CDR(l)) {
        val x = CAR(l);
        val q;
        if (x == sym_rest || x == sym_amp_rest) {
            l = CDR(l);
            if (!IS_CONS(l) || CDR(l) != NIL) isl_error("<program-error>", NULL);
            if (!IS_SYM(CAR(l))) err_domain(CAR(l), "<symbol>");
            if (SYM(CAR(l))->constant == 2 || strchr(STR(SYM(CAR(l))->name)->data, ':') || strchr(STR(SYM(CAR(l))->name)->data, '&'))
                isl_error("<program-error>", NULL);
            *rest = CAR(l);
            for (q = p; IS_CONS(q); q = CDR(q)) if (CAR(q) == *rest) isl_error("<program-error>", NULL);
            break;
        }
        if (!IS_SYM(x)) err_domain(x, "<symbol>");
        /* §3: no lexical binding of a named constant of the standard; §8: names
         * with a colon or an ampersand are reserved.  A user's defconstant may
         * be rebound locally (§12.8). */
        if (SYM(x)->constant == 2 || strchr(STR(SYM(x)->name)->data, ':') || strchr(STR(SYM(x)->name)->data, '&'))
            isl_error("<program-error>", NULL);
        for (q = p; IS_CONS(q); q = CDR(q)) if (CAR(q) == x) isl_error("<program-error>", NULL);   /* no identifier twice */
        p = mk_cons(x, p);
        gc_roots[slot] = p;
    }
    *params = list_nreverse(p);
    gc_sp = sp;
}

static val make_lambda(val ll, val body, val env, val name) {
    val params, rest, fn;
    long sp = gc_sp;
    gc_push(ll); gc_push(body); gc_push(env); gc_push(name);
    parse_lambda_list(ll, &params, &rest);
    gc_push(params); gc_push(rest);
    fn = mk_closure(params, rest, body, env, name);
    gc_sp = sp;
    return fn;
}

/* ---------- quasiquote ---------- */

static val qq_expand(val x, val env, int depth);

static val qq_list(val x, val env, int depth) {
    /* x is a list possibly containing (unquote-splicing e) elements */
    val head = NIL, tail = NIL;
    long sp = gc_sp;
    long hslot = gc_push(NIL);
    long xslot = gc_push(x);
    while (IS_CONS(x)) {
        val item = CAR(x);
        if (IS_CONS(item) && CAR(item) == sym_splice && depth == 1) {
            val spliced = eval(CAR(CDR(item)), env);
            long sslot = gc_push(spliced);
            for (; IS_CONS(spliced); spliced = CDR(spliced)) {
                val cell = mk_cons(CAR(spliced), NIL);
                if (head == NIL) { head = cell; gc_roots[hslot] = head; } else CDR(tail) = cell;
                tail = cell;
                gc_roots[sslot] = spliced;
            }
            gc_sp = sslot;
        } else if (item == sym_unquote && depth == 1) {
            /* dotted unquote: `(a . ,b) reads as (a unquote b) */
            val v = eval(CAR(CDR(x)), env);
            if (head == NIL) head = v; else CDR(tail) = v;
            gc_sp = sp;
            return head;
        } else {
            val v = qq_expand(item, env, depth);
            val cell = mk_cons(v, NIL);
            if (head == NIL) { head = cell; gc_roots[hslot] = head; } else CDR(tail) = cell;
            tail = cell;
        }
        x = CDR(x);
        gc_roots[xslot] = x;
    }
    if (x != NIL) {
        val v = qq_expand(x, env, depth);
        if (head == NIL) head = v; else CDR(tail) = v;
    }
    gc_sp = sp;
    return head;
}

static val qq_expand(val x, val env, int depth) {
    if (!IS_CONS(x)) {
        if (IS_VEC(x)) {
            /* `#(a ,b): the elements are expanded like a list's */
            long n = VEC(x)->n, i;
            val v;
            long sp = gc_sp;
            gc_push(x);
            v = mk_vector(n, NIL);
            gc_push(v);
            for (i = 0; i < n; i++) VEC(v)->elt[i] = qq_expand(VEC(x)->elt[i], env, depth);
            gc_sp = sp;
            return v;
        }
        if (IS_ARR(x)) {
            long n = VEC(ARR(x)->data)->n, i;
            val d;
            array_t *a;
            long sp = gc_sp;
            gc_push(x);
            d = mk_vector(n, NIL);
            gc_push(d);
            for (i = 0; i < n; i++) VEC(d)->elt[i] = qq_expand(VEC(ARR(x)->data)->elt[i], env, depth);
            a = (array_t *)heap_alloc(T_ARRAY, sizeof(array_t));
            a->rank = ARR(x)->rank; a->dims = ARR(x)->dims; a->data = d;
            gc_sp = sp;
            return (val)a;
        }
        return x;
    }
    if (CAR(x) == sym_unquote) {
        if (depth == 1) return eval(CAR(CDR(x)), env);
        return list2(sym_unquote, qq_expand(CAR(CDR(x)), env, depth - 1));
    }
    if (CAR(x) == sym_quasiquote) {
        val inner;
        long sp = gc_sp;
        inner = qq_expand(CAR(CDR(x)), env, depth + 1);
        gc_push(inner);
        inner = list2(sym_quasiquote, inner);
        gc_sp = sp;
        return inner;
    }
    return qq_list(x, env, depth);
}

/* ---------- setf ---------- */

static val eval_setf(val place, val valform, val env) {
    val v;
    long sp = gc_sp;
    gc_push(place); gc_push(env);
    if (IS_SYM(place)) {
        val b = env_var_binding(env, place);
        v = eval(valform, env);
        if (b) { CDR(b) = v; gc_sp = sp; return v; }
        if (SYM(place)->constant) err_immutable(place);
        if (SYM(place)->value == UNBOUND) err_unbound_variable(place);
        SYM(place)->value = v;
        gc_sp = sp;
        return v;
    }
    if (!IS_CONS(place) || !IS_SYM(CAR(place))) err_domain(place, "<symbol>");
    {
        val op = CAR(place);
        const char *name;
        val fn = env_fun_binding(env, op) ? CDR(env_fun_binding(env, op)) : SYM(op)->fn;
        if (IS_OBJ(fn, T_MACRO)) {
            /* a macro form as a place: expand it and set the expansion */
            long base = gc_sp;
            int argc = 0;
            val a, expansion;
            for (a = CDR(place); IS_CONS(a); a = CDR(a)) { gc_push(CAR(a)); argc++; }
            expansion = apply(MAC(fn)->expander, argc, &gc_roots[base]);
            gc_sp = sp;
            return eval_setf(expansion, valform, env);
        }
        name = STR(SYM(op)->name)->data;
        if (strcmp(name, "DYNAMIC") == 0) {
            val sym;
            if (list_length(place) != 2) isl_error("<program-error>", NULL);
            sym = CAR(CDR(place));
            if (!IS_SYM(sym)) err_domain(sym, "<symbol>");
            if (strchr(STR(SYM(sym)->name)->data, ':') || strchr(STR(SYM(sym)->name)->data, '&')) isl_error("<program-error>", NULL);
            v = eval(valform, env);
            if (SYM(sym)->dyn == UNBOUND) err_undefined_dynamic(sym);
            SYM(sym)->dyn = v;
            gc_sp = sp;
            return v;
        }
        {
            /* (setf (f args...) v)  ==  (set-f args... v) for the standard
             * accessors, or the writer of an accessor generic function. */
            val writer = setf_writer_for(op);
            int argc;
            long base;
            if (writer == NIL || writer == UNBOUND) isl_error("<program-error>", NULL);
            gc_push(writer);
            base = eval_args(CDR(place), env, &argc);
            v = eval(valform, env);
            gc_push(v);
            if (strcmp(name, "AREF") == 0 || strcmp(name, "GAREF") == 0 || strcmp(name, "ELT") == 0 ||
                strcmp(name, "PROPERTY") == 0 || strcmp(name, "CAR") == 0 || strcmp(name, "CDR") == 0) {
                /* set-xxx takes the value FIRST: (set-car obj cons), (set-aref obj array i...) */
                val tmp = gc_roots[base + argc];
                memmove(&gc_roots[base + 1], &gc_roots[base], (size_t)argc * sizeof(val));
                gc_roots[base] = tmp;
            } else {
                /* writer generic: (writer new-value instance) */
                val tmp = gc_roots[base + argc];
                memmove(&gc_roots[base + 1], &gc_roots[base], (size_t)argc * sizeof(val));
                gc_roots[base] = tmp;
            }
            /* (setf (f x) y) is the call of f's writer with (y x): its value is the result */
            v = apply(writer, argc + 1, &gc_roots[base]);
            gc_sp = sp;
            return v;
        }
    }
}

/* ---------- the evaluator ---------- */

static val eval_case_match(val key, val keys, val test, int using) {
    if (!using) {
        for (; IS_CONS(keys); keys = CDR(keys)) if (eql(key, CAR(keys))) return T;
        return NIL;
    }
    for (; IS_CONS(keys); keys = CDR(keys)) if (funcall2(test, key, CAR(keys)) != NIL) return T;
    return NIL;
}

val eval_body(val body, val env) {
    val r = NIL;
    long sp = gc_sp;
    long slot = gc_push(body);
    gc_push(env);
    if (list_length(body) < 0) isl_error("<program-error>", NULL);
    for (; IS_CONS(body); body = CDR(body)) {
        gc_roots[slot] = body;
        r = eval(CAR(body), env);
    }
    gc_sp = sp;
    return r;
}

/* Evaluate all forms but the last, returning the last form (for the tail loop). */
static val eval_body_but_last(val body, val env) {
    long sp = gc_sp;
    long slot = gc_push(body);
    gc_push(env);
    if (list_length(body) < 0) isl_error("<program-error>", NULL);
    if (!IS_CONS(body)) { gc_sp = sp; return NIL; }
    while (IS_CONS(CDR(body))) {
        gc_roots[slot] = body;
        eval(CAR(body), env);
        body = CDR(body);
    }
    gc_sp = sp;
    return CAR(body);
}

/* The nth argument of a special form, or a program error when the form
 * is too short or dotted (the suite feeds (block), (catch), (go . 1)). */
static val nth_arg(val f, int n) {
    int i;
    for (i = 0; i <= n; i++) {
        f = IS_CONS(f) ? CDR(f) : NIL;
        if (!IS_CONS(f)) isl_error("<program-error>", NULL);
    }
    return CAR(f);
}
static val rest_args(val f, int n) {
    int i;
    for (i = 0; i < n; i++) f = IS_CONS(f) ? CDR(f) : NIL;
    if (f != NIL && !IS_CONS(f)) isl_error("<program-error>", NULL);
    return f;
}
#define ARG1(f) nth_arg(f, 0)
#define ARG2(f) nth_arg(f, 1)
#define ARG3(f) nth_arg(f, 2)
#define REST1(f) rest_args(f, 1)
#define REST2(f) rest_args(f, 2)
#define REST3(f) rest_args(f, 3)

static void check_argcount(val form, int min, int max) {
    long n = list_length(form) - 1;
    if (n < min || (max >= 0 && n > max)) isl_error("<program-error>", NULL);
}

static val defining_form(val form, val env, int id);
static val eval_special(int id, val form, val env, int *tail, val *tform, val *tenv);

val eval(val form, val env) {
    long sp = gc_sp;
    val r;
    if (++eval_depth > 100000) fatal("evaluation too deep");
    gc_push(form); gc_push(env);
tail:
    if (IS_FIX(form) || IS_CHR(form)) { r = form; goto done; }
    if (!IS_PTR(form)) { r = form; goto done; }
    switch (TAG(form)) {
    case T_SYMBOL:
        if (SYM(form)->constant && (form == NIL || form == T)) { r = form; goto done; }
        if (STR(SYM(form)->name)->len > 0 && STR(SYM(form)->name)->data[0] == ':') { r = form; goto done; }  /* keywords */
        r = lookup_var(form, env);
        goto done;
    case T_CONS: {
        val op = CAR(form);
        if (IS_SYM(op)) {
            int id = SYM(op)->special;
            val fn;
            if (id) {
                int t = 0;
                val tform = NIL, tenv = NIL;
                if (id >= SP_DEFUN && id <= SP_DEFMETHOD) { r = defining_form(form, env, id); goto done; }
                r = eval_special(id, form, env, &t, &tform, &tenv);
                if (t) { form = tform; env = tenv; gc_roots[sp] = form; gc_roots[sp + 1] = env; goto tail; }
                goto done;
            }
            fn = lookup_fun(op, env);
            if (IS_OBJ(fn, T_MACRO)) {
                int argc;
                long base;
                val expansion;
                gc_push(fn);
                /* macro arguments are passed unevaluated */
                base = gc_sp;
                {
                    val a = CDR(form);
                    argc = 0;
                    for (; IS_CONS(a); a = CDR(a)) { gc_push(CAR(a)); argc++; }
                    if (a != NIL) isl_error("<program-error>", NULL);
                }
                expansion = apply(MAC(fn)->expander, argc, &gc_roots[base]);
                form = expansion;
                gc_roots[sp] = form;
                gc_sp = sp + 2;
                goto tail;
            }
            {
                int argc;
                long base;
                gc_push(fn);
                if (IS_OBJ(fn, T_CLOSURE)) {
                    long n = list_length(CDR(form));
                    if (n < 0) isl_error("<program-error>", NULL);
                    if (n < CLO(fn)->nparams || (CLO(fn)->rest == NIL && n > CLO(fn)->nparams)) err_arity(fn, (int)n);
                }
                base = eval_args(CDR(form), env, &argc);
                if (IS_OBJ(fn, T_CLOSURE)) {
                    /* tail call: rebind and continue in this activation */
                    env = bind_params(fn, argc, &gc_roots[base]);
                    gc_roots[sp + 1] = env;
                    form = eval_body_but_last(CLO(fn)->body, env);
                    gc_roots[sp] = form;
                    gc_sp = sp + 2;
                    goto tail;
                }
                r = apply(fn, argc, &gc_roots[base]);
                goto done;
            }
        }
        if (IS_CONS(op) && CAR(op) == sym_lambda) {
            val fn = make_lambda(ARG1(op), REST2(op), env, NIL);
            int argc;
            long base;
            long n = list_length(CDR(form));
            gc_push(fn);
            if (n < 0) isl_error("<program-error>", NULL);
            if (n < CLO(fn)->nparams || (CLO(fn)->rest == NIL && n > CLO(fn)->nparams)) err_arity(fn, (int)n);
            base = eval_args(CDR(form), env, &argc);
            env = bind_params(fn, argc, &gc_roots[base]);
            gc_roots[sp + 1] = env;
            form = eval_body_but_last(CLO(fn)->body, env);
            gc_roots[sp] = form;
            gc_sp = sp + 2;
            goto tail;
        }
        err_undefined_function(op);
        r = NIL;
        goto done;
    }
    default:
        r = form;       /* strings, vectors, arrays, floats, bignums: literals */
        goto done;
    }
done:
    eval_depth--;
    gc_sp = sp;
    return r;
}

static void check_binding_list(val bindings) {
    val b;
    if (list_length(bindings) < 0) isl_error("<program-error>", NULL);
    for (b = bindings; IS_CONS(b); b = CDR(b)) {
        val spec = CAR(b);
        if (!IS_CONS(spec) || list_length(spec) != 2) isl_error("<program-error>", NULL);
        if (!IS_SYM(CAR(spec))) err_domain(CAR(spec), "<symbol>");
        if (strchr(STR(SYM(CAR(spec))->name)->data, ':') || strchr(STR(SYM(CAR(spec))->name)->data, '&'))
            isl_error("<program-error>", NULL);
    }
}

static val eval_let(val form, val env, int star, val *tform, val *tenv) {
    val bindings = ARG1(form);
    val newenv;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    check_binding_list(bindings);
    newenv = mk_env(env);
    gc_push(newenv);
    if (!star) {
        /* evaluate all init forms in the outer env first */
        long base = gc_sp;
        val b;
        int n = 0;
        for (b = bindings; IS_CONS(b); b = CDR(b)) {
            val spec = CAR(b);
            val v = IS_CONS(spec) ? eval(IS_CONS(CDR(spec)) ? ARG1(spec) : NIL, env) : NIL;
            gc_push(v);
            n++;
        }
        for (b = bindings, n = 0; IS_CONS(b); b = CDR(b), n++) {
            val spec = CAR(b);
            val name = IS_CONS(spec) ? CAR(spec) : spec;
            if (!IS_SYM(name)) err_domain(name, "<symbol>");
            if (SYM(name)->constant == 2) isl_error("<program-error>", NULL);
            env_bind_var(newenv, name, gc_roots[base + n]);
        }
    } else {
        /* let*: each binding sees the ones before it, and a closure made
         * by an init form must keep seeing the binding that was current
         * then -- so every binding gets its own frame. */
        val b;
        long eslot = gc_push(newenv);
        for (b = bindings; IS_CONS(b); b = CDR(b)) {
            val spec = CAR(b);
            val name = IS_CONS(spec) ? CAR(spec) : spec;
            val v;
            if (!IS_SYM(name)) err_domain(name, "<symbol>");
            if (SYM(name)->constant == 2) isl_error("<program-error>", NULL);
            v = IS_CONS(spec) ? eval(IS_CONS(CDR(spec)) ? ARG1(spec) : NIL, newenv) : NIL;
            gc_push(v);
            newenv = mk_env(newenv);
            gc_roots[eslot] = newenv;
            env_bind_var(newenv, name, v);
            gc_sp = eslot + 1;
        }
    }
    *tform = eval_body_but_last(REST2(form), newenv);
    *tenv = newenv;
    gc_sp = sp;
    return NIL;
}

static val eval_labels(val form, val env, int labels, val *tform, val *tenv) {
    val newenv;
    val defs;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    check_argcount(form, 1, -1);
    if (list_length(ARG1(form)) < 0) isl_error("<program-error>", NULL);
    newenv = mk_env(env);
    gc_push(newenv);
    for (defs = ARG1(form); IS_CONS(defs); defs = CDR(defs)) {
        val d = CAR(defs);
        val fn;
        if (!IS_CONS(d)) isl_error("<program-error>", NULL);
        if (!IS_SYM(CAR(d))) err_domain(CAR(d), "<symbol>");
        if (strchr(STR(SYM(CAR(d))->name)->data, ':') || strchr(STR(SYM(CAR(d))->name)->data, '&') || !IS_CONS(CDR(d)))
            isl_error("<program-error>", NULL);
        fn = make_lambda(ARG1(d), REST2(d), labels ? newenv : env, CAR(d));
        env_bind_fun(newenv, CAR(d), fn);
    }
    *tform = eval_body_but_last(REST2(form), newenv);
    *tenv = newenv;
    gc_sp = sp;
    return NIL;
}

static val eval_special(int id, val form, val env, int *tail, val *tform, val *tenv) {
    val r = NIL;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    switch (id) {
    case SP_QUOTE:
        check_argcount(form, 1, 1);
        r = ARG1(form);
        break;
    case SP_IF:
        check_argcount(form, 2, 3);
        if (eval(ARG1(form), env) != NIL) *tform = ARG2(form);
        else *tform = IS_CONS(REST3(form)) ? ARG3(form) : NIL;
        *tenv = env; *tail = 1;
        break;
    case SP_PROGN:
        *tform = eval_body_but_last(REST1(form), env);
        *tenv = env; *tail = 1;
        break;
    case SP_SETQ: {
        val sym, v, b;
        check_argcount(form, 2, 2);
        sym = ARG1(form);
        if (!IS_SYM(sym)) err_domain(sym, "<symbol>");
        v = eval(ARG2(form), env);
        b = env_var_binding(env, sym);
        if (!b && SYM(sym)->constant) err_immutable(sym);
        if (b) CDR(b) = v;
        else if (SYM(sym)->value != UNBOUND) SYM(sym)->value = v;
        else err_unbound_variable(sym);
        r = v;
        break;
    }
    case SP_LET: eval_let(form, env, 0, tform, tenv); *tail = 1; break;
    case SP_LETSTAR: eval_let(form, env, 1, tform, tenv); *tail = 1; break;
    case SP_LAMBDA:
        check_argcount(form, 1, -1);
        r = make_lambda(ARG1(form), REST2(form), env, NIL);
        break;
    case SP_FUNCTION: {
        val name;
        check_argcount(form, 1, 1);
        name = ARG1(form);
        if (IS_CONS(name) && CAR(name) == sym_lambda) r = make_lambda(ARG1(name), REST2(name), env, NIL);
        else if (IS_SYM(name)) {
            r = lookup_fun(name, env);
            if (IS_OBJ(r, T_MACRO) || SYM(name)->special) err_undefined_function(name);
        } else err_domain(name, "<symbol>");
        break;
    }
    case SP_COND: {
        val clauses;
        for (clauses = REST1(form); IS_CONS(clauses); clauses = CDR(clauses)) {
            val c = CAR(clauses);
            val test;
            if (!IS_CONS(c)) isl_error("<program-error>", NULL);
            test = eval(CAR(c), env);
            if (test != NIL) {
                if (CDR(c) == NIL) { r = test; goto out; }
                *tform = eval_body_but_last(CDR(c), env);
                *tenv = env; *tail = 1;
                goto out;
            }
        }
        r = NIL;
        break;
    }
    case SP_CASE:
    case SP_CASE_USING: {
        val key, clauses, test = NIL;
        int using = id == SP_CASE_USING;
        if (using) { test = eval(ARG1(form), env); gc_push(test); form = CDR(form); }
        key = eval(ARG1(form), env);
        gc_push(key);
        for (clauses = REST2(form); IS_CONS(clauses); clauses = CDR(clauses)) {
            val c = CAR(clauses);
            val keys;
            if (!IS_CONS(c)) isl_error("<program-error>", NULL);
            keys = CAR(c);
            if (keys == T) {
                if (CDR(clauses) != NIL) isl_error("<program-error>", NULL);   /* t only as the last clause */
                *tform = eval_body_but_last(CDR(c), env); *tenv = env; *tail = 1; goto out;
            }
            if (!IS_LIST(keys)) isl_error("<program-error>", NULL);
            if (eval_case_match(key, keys, test, using) != NIL) {
                *tform = eval_body_but_last(CDR(c), env); *tenv = env; *tail = 1; goto out;
            }
        }
        r = NIL;
        break;
    }
    case SP_AND: {
        val a = REST1(form);
        r = T;
        for (; IS_CONS(a); a = CDR(a)) {
            if (CDR(a) == NIL) { *tform = CAR(a); *tenv = env; *tail = 1; goto out; }
            r = eval(CAR(a), env);
            if (r == NIL) break;
        }
        break;
    }
    case SP_OR: {
        val a = REST1(form);
        r = NIL;
        for (; IS_CONS(a); a = CDR(a)) {
            if (CDR(a) == NIL) { *tform = CAR(a); *tenv = env; *tail = 1; goto out; }
            r = eval(CAR(a), env);
            if (r != NIL) break;
        }
        break;
    }
    case SP_WHILE:
        while (eval(ARG1(form), env) != NIL) eval_body(REST2(form), env);
        r = NIL;
        break;
    case SP_FOR: {
        /* (for ((var init [step])*) (end-test result*) body*) */
        val specs = ARG1(form), endclause = ARG2(form), body = REST3(form);
        val newenv;
        val s;
        if (list_length(specs) < 0 || !IS_CONS(endclause) || list_length(endclause) < 0 || list_length(body) < 0)
            isl_error("<program-error>", NULL);
        for (s = specs; IS_CONS(s); s = CDR(s)) {
            long k = list_length(CAR(s));
            val q;
            if (k < 2 || k > 3) isl_error("<program-error>", NULL);
            if (!IS_SYM(CAR(CAR(s)))) err_domain(CAR(CAR(s)), "<symbol>");
            if (SYM(CAR(CAR(s)))->constant == 2 || strchr(STR(SYM(CAR(CAR(s)))->name)->data, ':') || strchr(STR(SYM(CAR(CAR(s)))->name)->data, '&'))
                isl_error("<program-error>", NULL);
            for (q = CDR(s); IS_CONS(q); q = CDR(q)) if (IS_CONS(CAR(q)) && CAR(CAR(q)) == CAR(CAR(s))) isl_error("<program-error>", NULL);
        }
        newenv = mk_env(env);
        gc_push(newenv);
        {
            long base = gc_sp;
            int n = 0;
            for (s = specs; IS_CONS(s); s = CDR(s)) { gc_push(eval(ARG1(CAR(s)), env)); n++; }
            for (s = specs, n = 0; IS_CONS(s); s = CDR(s), n++) env_bind_var(newenv, CAR(CAR(s)), gc_roots[base + n]);
            gc_sp = base;
        }
        for (;;) {
            if (eval(CAR(endclause), newenv) != NIL) break;
            eval_body(body, newenv);
            {
                /* steps are evaluated in parallel, then assigned */
                long base = gc_sp;
                int n = 0;
                for (s = specs; IS_CONS(s); s = CDR(s)) {
                    val spec = CAR(s);
                    if (IS_CONS(REST2(spec))) gc_push(eval(ARG2(spec), newenv));
                    else gc_push(UNBOUND);
                    n++;
                }
                for (s = specs, n = 0; IS_CONS(s); s = CDR(s), n++) {
                    if (gc_roots[base + n] != UNBOUND) CDR(env_var_binding(newenv, CAR(CAR(s)))) = gc_roots[base + n];
                }
                gc_sp = base;
            }
        }
        r = eval_body(CDR(endclause), newenv);
        break;
    }
    case SP_BLOCK: {
        nlx_t f;
        val name = ARG1(form);
        val tag, newenv;
        if (!IS_SYM(name)) err_domain(name, "<symbol>");
        if (list_length(REST2(form)) < 0) isl_error("<program-error>", NULL);
        tag = mk_cons(name, NIL);         /* a fresh identity for this activation */
        gc_push(tag);
        newenv = mk_env(env);
        gc_push(newenv);
        env_bind_tag(newenv, name, tag);
        nlx_push(&f, NLX_BLOCK, tag);
        if (setjmp(f.jb) == 0) {
            r = eval_body(REST2(form), newenv);
            nlx_pop(&f);
        } else {
            r = f.value;
            nlx_pop(&f);
        }
        break;
    }
    case SP_RETURN_FROM: {
        val name = ARG1(form);
        val b, v;
        nlx_t *f;
        check_argcount(form, 2, 2);
        if (!IS_SYM(name)) err_domain(name, "<symbol>");
        b = env_tag_binding(env, name);
        if (!b) err_control();
        v = eval(ARG2(form), env);
        f = find_frame_by_tag(NLX_BLOCK, CDR(b));
        if (!f) err_control();
        nlx_throw(f, v);
        break;
    }
    case SP_CATCH: {
        nlx_t f;
        val tag = eval(ARG1(form), env);
        if (IS_NUM(tag) || IS_CHR(tag)) err_domain(tag, "<object>");   /* §14.7.1 */
        gc_push(tag);
        nlx_push(&f, NLX_CATCH, tag);
        if (setjmp(f.jb) == 0) {
            r = eval_body(REST2(form), env);
            nlx_pop(&f);
        } else {
            r = f.value;
            nlx_pop(&f);
        }
        break;
    }
    case SP_THROW: {
        val tag, v;
        nlx_t *f;
        check_argcount(form, 2, 2);
        tag = eval(ARG1(form), env);
        gc_push(tag);
        v = eval(ARG2(form), env);
        if (IS_NUM(tag) || IS_CHR(tag)) err_domain(tag, "<object>");   /* §14.7.1: a catch tag is neither */
        for (f = nlx_top; f; f = f->prev) if (f->kind == NLX_CATCH && f->active && eql(f->tag, tag)) break;
        if (!f) err_control();
        nlx_throw(f, v);
        break;
    }
    case SP_TAGBODY: {
        nlx_t f;
        val body = REST1(form);
        val tag, newenv, b;
        tag = mk_cons(form, NIL);
        gc_push(tag);
        newenv = mk_env(env);
        gc_push(newenv);
        for (b = body; IS_CONS(b); b = CDR(b))
            if (IS_SYM(CAR(b)) || IS_FIX(CAR(b))) env_bind_tag(newenv, CAR(b), tag);
        nlx_push(&f, NLX_TAGBODY, tag);
        b = body;
        for (;;) {
            if (setjmp(f.jb) == 0) {
                for (; IS_CONS(b); b = CDR(b)) {
                    if (IS_SYM(CAR(b)) || IS_FIX(CAR(b))) continue;
                    eval(CAR(b), newenv);
                }
                break;
            } else {
                /* go: f.value is the tag; resume after it */
                val target = f.value;
                for (b = body; IS_CONS(b); b = CDR(b)) if (CAR(b) == target) break;
                if (!IS_CONS(b)) fatal("tagbody: tag vanished");
                b = CDR(b);
                /* the frame was popped by nlx_throw's bookkeeping? no: re-push */
                nlx_top = &f;
                f.active = 1;
            }
        }
        nlx_pop(&f);
        r = NIL;
        break;
    }
    case SP_GO: {
        val tag = ARG1(form);
        val b;
        nlx_t *f;
        check_argcount(form, 1, 1);
        b = env_tag_binding(env, tag);
        if (!b) err_control();
        f = find_frame_by_tag(NLX_TAGBODY, CDR(b));
        if (!f) err_control();
        nlx_throw(f, tag);
        break;
    }
    case SP_UNWIND_PROTECT: {
        nlx_t f;
        nlx_push(&f, NLX_UNWIND, NIL);
        f.cleanup = REST2(form);
        f.env = env;
        if (setjmp(f.jb) == 0) {
            r = eval(ARG1(form), env);
            nlx_pop(&f);
            gc_push(r);
            eval_body(REST2(form), env);
        } else {
            nlx_t *target = f.pending;
            val v = f.pending_value;
            nlx_pop(&f);
            gc_push(v);
            eval_body(f.cleanup, f.env);
            nlx_throw(target, v);
        }
        break;
    }
    case SP_DYNAMIC: {
        val sym = ARG1(form);
        check_argcount(form, 1, 1);
        if (!IS_SYM(sym)) isl_error("<program-error>", NULL);
        if (SYM(sym)->dyn == UNBOUND) err_undefined_dynamic(sym);
        r = SYM(sym)->dyn;
        break;
    }
    case SP_SET_DYNAMIC: {
        val sym;
        check_argcount(form, 2, 2);
        sym = ARG2(form);
        if (!IS_SYM(sym)) isl_error("<program-error>", NULL);
        r = eval(ARG1(form), env);
        if (SYM(sym)->dyn == UNBOUND) err_undefined_dynamic(sym);
        SYM(sym)->dyn = r;
        break;
    }
    case SP_DYNAMIC_LET: {
        val bindings = ARG1(form), b;
        long saved = dyn_sp;
        long base = gc_sp;
        int n = 0;
        check_binding_list(bindings);
        if (list_length(REST2(form)) < 0) isl_error("<program-error>", NULL);
        for (b = bindings; IS_CONS(b); b = CDR(b)) {
            val spec = CAR(b);
            gc_push(IS_CONS(spec) ? eval(IS_CONS(CDR(spec)) ? ARG1(spec) : NIL, env) : NIL);
            n++;
        }
        for (b = bindings, n = 0; IS_CONS(b); b = CDR(b), n++) {
            val spec = CAR(b);
            val name = IS_CONS(spec) ? CAR(spec) : spec;
            if (!IS_SYM(name)) isl_error("<program-error>", NULL);
            dyn_bind(name, gc_roots[base + n]);
        }
        r = eval_body(REST2(form), env);
        dyn_unwind_to(saved);
        break;
    }
    case SP_SETF:
        check_argcount(form, 2, 2);
        r = eval_setf(ARG1(form), ARG2(form), env);
        break;
    case SP_LABELS: eval_labels(form, env, 1, tform, tenv); *tail = 1; break;
    case SP_FLET: eval_labels(form, env, 0, tform, tenv); *tail = 1; break;
    case SP_THE:
    case SP_ASSURE: {
        val cls;
        check_argcount(form, 2, 2);
        cls = find_class_sym(ARG1(form), 1);
        gc_push(cls);
        r = eval(ARG2(form), env);
        if (!instancep(r, cls)) err_domain(r, STR(SYM(CLS(cls)->name)->name)->data);
        break;
    }
    case SP_CONVERT: {
        val v;
        check_argcount(form, 2, 2);
        v = eval(ARG1(form), env);
        gc_push(v);
        r = sf_convert(v, ARG2(form), env);
        break;
    }
    case SP_CLASS:
        check_argcount(form, 1, 1);
        r = find_class_sym(ARG1(form), 1);
        break;
    case SP_WITH_HANDLER: {
        nlx_t f;
        val h = eval(ARG1(form), env);
        if (!IS_FUNC(h)) err_domain(h, "<function>");
        gc_push(h);
        nlx_push(&f, NLX_HANDLER, NIL);
        cur_handler = mk_cons(h, cur_handler);
        if (setjmp(f.jb) == 0) {
            r = eval_body(REST2(form), env);
            nlx_pop(&f);
            cur_handler = f.handler;
        } else {
            nlx_pop(&f);
            r = f.value;
        }
        break;
    }
    case SP_IGNORE_ERRORS: {
        nlx_t f;
        val tag = mk_cons(sym_ignore_mark, NIL);
        gc_push(tag);
        nlx_push(&f, NLX_CATCH, tag);
        cur_handler = mk_cons(tag, cur_handler);   /* a marker: a cons, not a function */
        if (setjmp(f.jb) == 0) {
            r = eval_body(REST1(form), env);
            nlx_pop(&f);
            cur_handler = f.handler;
        } else {
            nlx_pop(&f);
            r = NIL;
        }
        break;
    }
    case SP_WITH_STANDARD_INPUT:
    case SP_WITH_STANDARD_OUTPUT:
    case SP_WITH_ERROR_OUTPUT: {
        val s = eval(ARG1(form), env);
        val *slot = id == SP_WITH_STANDARD_INPUT ? &cur_stdin : id == SP_WITH_STANDARD_OUTPUT ? &cur_stdout : &cur_stderr;
        val saved = *slot;
        nlx_t f;
        if (!IS_OBJ(s, T_STREAM)) err_domain(s, "<stream>");
        gc_push(s); gc_push(saved);
        /* restore on any exit: an unwind frame whose cleanup is done here */
        nlx_push(&f, NLX_UNWIND, NIL);
        f.cleanup = NIL; f.env = env;
        *slot = s;
        if (setjmp(f.jb) == 0) {
            r = eval_body(REST2(form), env);
            nlx_pop(&f);
            *slot = saved;
        } else {
            nlx_t *target = f.pending;
            val v = f.pending_value;
            nlx_pop(&f);
            *slot = saved;
            nlx_throw(target, v);
        }
        break;
    }
    case SP_WITH_OPEN_INPUT_FILE: r = sf_with_open_file(S_FILE_IN, form, env); break;
    case SP_WITH_OPEN_OUTPUT_FILE: r = sf_with_open_file(S_FILE_OUT, form, env); break;
    case SP_WITH_OPEN_IO_FILE: r = sf_with_open_file(S_FILE_IO, form, env); break;
    case SP_QUASIQUOTE:
        check_argcount(form, 1, 1);
        r = qq_expand(ARG1(form), env, 1);
        break;
    case SP_CALL_NEXT_METHOD:
        if (list_length(form) != 1) {
            /* outside a method the name is simply unbound; inside, the arity is wrong */
            ilos_next_method_p(env);
            check_argcount(form, 0, 0);
        }
        r = ilos_call_next_method(env);
        break;
    case SP_NEXT_METHOD_P:
        if (list_length(form) != 1) {
            ilos_next_method_p(env);
            check_argcount(form, 0, 0);
        }
        r = ilos_next_method_p(env);
        break;
    default:
        fatal("unknown special form");
    }
out:
    gc_sp = sp;
    return r;
}

/* ---------- defining forms (§12.8) ---------- */

static void check_toplevel_name(val name, int variable_ns) {
    if (!IS_SYM(name)) err_domain(name, "<symbol>");
    if (strchr(STR(SYM(name)->name)->data, ':') || strchr(STR(SYM(name)->name)->data, '&'))
        isl_error("<program-error>", NULL);
    if (variable_ns && SYM(name)->constant == 2) isl_error("<program-error>", NULL);
}

static val defining_form(val form, val env, int id) {
    val name;
    val r = NIL;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    switch (id) {
    case SP_DEFUN: {
        val fn;
        check_argcount(form, 2, -1);
        name = ARG1(form);
        check_toplevel_name(name, 0);
        if (SYM(name)->special) isl_error("<program-error>", NULL);
        fn = make_lambda(ARG2(form), REST3(form), global_env, name);
        SYM(name)->fn = fn;
        r = name;
        break;
    }
    case SP_DEFMACRO: {
        val fn;
        macro_t *m;
        check_argcount(form, 2, -1);
        name = ARG1(form);
        check_toplevel_name(name, 0);
        fn = make_lambda(ARG2(form), REST3(form), global_env, name);
        gc_push(fn);
        m = (macro_t *)heap_alloc(T_MACRO, sizeof(macro_t));
        m->expander = fn; m->name = name;
        SYM(name)->fn = (val)m;
        r = name;
        break;
    }
    case SP_DEFGLOBAL: {
        val v;
        check_argcount(form, 2, 2);
        name = ARG1(form);
        check_toplevel_name(name, 1);
        if (SYM(name)->constant) err_immutable(name);
        v = eval(ARG2(form), env);
        SYM(name)->value = v;
        r = name;
        break;
    }
    case SP_DEFCONSTANT: {
        val v;
        check_argcount(form, 2, 2);
        name = ARG1(form);
        check_toplevel_name(name, 1);
        v = eval(ARG2(form), env);
        SYM(name)->value = v;
        SYM(name)->constant = 1;
        if (IS_PTR(v)) ((obj *)v)->imm = 1;
        r = name;
        break;
    }
    case SP_DEFDYNAMIC: {
        val v;
        check_argcount(form, 2, 2);
        name = ARG1(form);
        check_toplevel_name(name, 0);
        v = eval(ARG2(form), env);
        SYM(name)->dyn = v;
        r = name;
        break;
    }
    case SP_DEFCLASS: r = sf_defclass(form, env); break;
    case SP_DEFGENERIC: r = sf_defgeneric(form, env); break;
    case SP_DEFMETHOD: r = sf_defmethod(form, env); break;
    }
    gc_sp = sp;
    return r;
}

/* ---------- init ---------- */

void eval_init(void) {
    dyn_syms = malloc((size_t)dyn_cap * sizeof(val));
    dyn_olds = malloc((size_t)dyn_cap * sizeof(val));
    if (!dyn_syms || !dyn_olds) fatal("out of memory");
    global_env = mk_env(NIL);
    cur_handler = NIL;
    sym_ignore_mark = mk_uninterned("#<ignore-errors>");
    defspecial("QUOTE", SP_QUOTE);
    defspecial("IF", SP_IF);
    defspecial("PROGN", SP_PROGN);
    defspecial("SETQ", SP_SETQ);
    defspecial("LET", SP_LET);
    defspecial("LET*", SP_LETSTAR);
    defspecial("LAMBDA", SP_LAMBDA);
    defspecial("FUNCTION", SP_FUNCTION);
    defspecial("COND", SP_COND);
    defspecial("CASE", SP_CASE);
    defspecial("CASE-USING", SP_CASE_USING);
    defspecial("AND", SP_AND);
    defspecial("OR", SP_OR);
    defspecial("WHILE", SP_WHILE);
    defspecial("FOR", SP_FOR);
    defspecial("BLOCK", SP_BLOCK);
    defspecial("RETURN-FROM", SP_RETURN_FROM);
    defspecial("CATCH", SP_CATCH);
    defspecial("THROW", SP_THROW);
    defspecial("TAGBODY", SP_TAGBODY);
    defspecial("GO", SP_GO);
    defspecial("UNWIND-PROTECT", SP_UNWIND_PROTECT);
    defspecial("DYNAMIC", SP_DYNAMIC);
    defspecial("DYNAMIC-LET", SP_DYNAMIC_LET);
    defspecial("SET-DYNAMIC", SP_SET_DYNAMIC);
    defspecial("SETF", SP_SETF);
    defspecial("LABELS", SP_LABELS);
    defspecial("FLET", SP_FLET);
    defspecial("THE", SP_THE);
    defspecial("ASSURE", SP_ASSURE);
    defspecial("CONVERT", SP_CONVERT);
    defspecial("CLASS", SP_CLASS);
    defspecial("WITH-HANDLER", SP_WITH_HANDLER);
    defspecial("IGNORE-ERRORS", SP_IGNORE_ERRORS);
    defspecial("WITH-STANDARD-INPUT", SP_WITH_STANDARD_INPUT);
    defspecial("WITH-STANDARD-OUTPUT", SP_WITH_STANDARD_OUTPUT);
    defspecial("WITH-ERROR-OUTPUT", SP_WITH_ERROR_OUTPUT);
    defspecial("WITH-OPEN-INPUT-FILE", SP_WITH_OPEN_INPUT_FILE);
    defspecial("WITH-OPEN-OUTPUT-FILE", SP_WITH_OPEN_OUTPUT_FILE);
    defspecial("WITH-OPEN-IO-FILE", SP_WITH_OPEN_IO_FILE);
    defspecial("QUASIQUOTE", SP_QUASIQUOTE);
    defspecial("DEFUN", SP_DEFUN);
    defspecial("DEFMACRO", SP_DEFMACRO);
    defspecial("DEFGLOBAL", SP_DEFGLOBAL);
    defspecial("DEFCONSTANT", SP_DEFCONSTANT);
    defspecial("DEFDYNAMIC", SP_DEFDYNAMIC);
    defspecial("DEFCLASS", SP_DEFCLASS);
    defspecial("DEFGENERIC", SP_DEFGENERIC);
    defspecial("DEFMETHOD", SP_DEFMETHOD);
    defspecial("CALL-NEXT-METHOD", SP_CALL_NEXT_METHOD);
    defspecial("NEXT-METHOD-P", SP_NEXT_METHOD_P);
}

/* The ignore-errors marker test, for cond.c. */
int handler_is_ignore_marker(val h) { return IS_CONS(h) && CAR(h) == sym_ignore_mark; }
nlx_t *find_catch_frame(val tag) { return find_frame_by_tag(NLX_CATCH, tag); }
