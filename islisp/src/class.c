/* class.c -- classes (§10) and the ISLISP Object System (§15).
 *
 * Every object has a class; the built-in ones of Figure 1 are class
 * objects created here with their inheritance, so class-of, instancep
 * and subclassp answer uniformly for a cons and for a defclass'd
 * instance.  The condition classes are built-in classes with slots, so
 * `create` makes conditions the way §29.3 shows.
 *
 * defclass computes the class precedence list by the §15.1.1 rule
 * (self, then the superclasses' lists appended, the LEFTMOST duplicate
 * removed), inherits slots and their initforms (most specific wins),
 * and generates reader/writer/accessor/boundp methods as methods of a
 * special kind so no closure has to be synthesised.  A generic function
 * dispatches by the §15.3 procedure: applicable methods, sorted by the
 * specializers' positions in each argument's precedence list, combined
 * by the standard (:around/:before/primary/:after) or the nil
 * combination.  call-next-method and next-method-p are special
 * operators that read the innermost method context.
 */
#include "isl.h"
#include <string.h>
#include <stdlib.h>

static val cls_object, cls_basic_array, cls_basic_array_star, cls_general_array_star, cls_basic_vector,
           cls_general_vector, cls_string, cls_built_in_class, cls_character, cls_function,
           cls_generic_function, cls_standard_generic_function, cls_list, cls_cons, cls_null, cls_symbol,
           cls_number, cls_float, cls_integer, cls_serious_condition, cls_error, cls_arithmetic_error,
           cls_division_by_zero, cls_fp_overflow, cls_fp_underflow, cls_control_error, cls_parse_error,
           cls_program_error, cls_domain_error, cls_undefined_entity, cls_unbound_variable,
           cls_undefined_function, cls_simple_error, cls_stream_error, cls_end_of_stream,
           cls_storage_exhausted, cls_standard_class, cls_standard_object, cls_stream;

static val sym_setf_key;         /* plist key under which a symbol's setf writer generic lives */
static val gf_initialize_object;

/* A slot spec vector */
#define SLOT_NAME 0
#define SLOT_INITARGS 1
#define SLOT_INITFORM 2
#define SLOT_NFIELDS 3

/* ---------- class objects ---------- */

static val mk_class_obj(val sym, val supers, int builtin) {
    class_t *c;
    long sp = gc_sp;
    gc_push(supers); gc_push(sym);
    c = (class_t *)heap_alloc(T_CLASS, sizeof(class_t));
    c->name = sym; c->supers = supers; c->cpl = NIL; c->slots = NIL; c->allslots = NIL;
    c->builtin = builtin; c->abstractp = 0; c->metaclass = builtin;
    gc_sp = sp;
    return (val)c;
}

static val mk_class(const char *name, val supers, int builtin) {
    val sym = intern_c(name);
    val c = mk_class_obj(sym, supers, builtin);
    SYM(sym)->cls = c;
    return c;
}

/* §15.1.1: C . P1 . ... . Pn, then remove the leftmost of any duplicate. */
static val compute_cpl(val cls) {
    val all = NIL, s, l, result = NIL;
    long sp = gc_sp;
    long aslot = gc_push(NIL), rslot = gc_push(NIL);
    all = mk_cons(cls, NIL);
    gc_roots[aslot] = all;
    for (s = CLS(cls)->supers; IS_CONS(s); s = CDR(s))
        for (l = CLS(CAR(s))->cpl; IS_CONS(l); l = CDR(l)) { all = mk_cons(CAR(l), all); gc_roots[aslot] = all; }
    all = list_nreverse(all);      /* in concatenation order */
    gc_roots[aslot] = all;
    /* keep an element only if it does not occur again later */
    for (l = all; IS_CONS(l); l = CDR(l)) {
        val m;
        int later = 0;
        for (m = CDR(l); IS_CONS(m); m = CDR(m)) if (CAR(m) == CAR(l)) { later = 1; break; }
        if (!later) { result = mk_cons(CAR(l), result); gc_roots[rslot] = result; }
    }
    result = list_nreverse(result);
    gc_sp = sp;
    return result;
}

static void finish_builtin(val c) { CLS(c)->cpl = compute_cpl(c); }

int subclassp(val c1, val c2) {
    val l;
    if (c1 == c2) return 0;
    for (l = CLS(c1)->cpl; IS_CONS(l); l = CDR(l)) if (CAR(l) == c2) return 1;
    return 0;
}

val class_of(val v) {
    if (IS_FIX(v)) return cls_integer;
    if (IS_CHR(v)) return cls_character;
    switch (TAG(v)) {
    case T_CONS: return cls_cons;
    case T_SYMBOL: return v == NIL ? cls_null : cls_symbol;
    case T_STRING: return cls_string;
    case T_FLOAT: return cls_float;
    case T_BIGNUM: return cls_integer;
    case T_VECTOR: return cls_general_vector;
    case T_ARRAY: return cls_general_array_star;
    case T_CLOSURE: case T_BUILTIN: return cls_function;
    case T_GENERIC: return cls_standard_generic_function;
    case T_CLASS: return CLS(v)->metaclass ? cls_built_in_class : cls_standard_class;
    case T_INSTANCE: return INST(v)->cls;
    case T_STREAM: return cls_stream;
    default: return cls_object;
    }
}

int instancep(val v, val cls) {
    val c = class_of(v);
    val l;
    if (c == cls) return 1;
    for (l = CLS(c)->cpl; IS_CONS(l); l = CDR(l)) if (CAR(l) == cls) return 1;
    return 0;
}

val find_class_sym(val sym, int signal) {
    if (sym == NIL) return cls_null;      /* the suite names <null> this way */
    if (!IS_SYM(sym)) { if (signal) err_domain(sym, "<symbol>"); return NIL; }
    if (SYM(sym)->cls == NIL) { if (signal) err_undefined_class(sym); return NIL; }
    return SYM(sym)->cls;
}

/* ---------- instances and slots ---------- */

static long slot_index(val cls, val name) {
    val l;
    long i = 0;
    for (l = CLS(cls)->allslots; IS_CONS(l); l = CDR(l), i++) if (CAR(l) == name) return i;
    return -1;
}

static void err_no_slot(val inst, val name) {
    (void)inst;
    isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
}

val instance_slot(val inst, val name) {
    long i;
    val v;
    if (!IS_OBJ(inst, T_INSTANCE)) err_domain(inst, "<standard-object>");
    i = slot_index(INST(inst)->cls, name);
    if (i < 0) err_no_slot(inst, name);
    v = VEC(INST(inst)->slots)->elt[i];
    if (v == UNBOUND) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);   /* unbound slot */
    return v;
}

static int instance_slot_boundp(val inst, val name) {
    long i;
    if (!IS_OBJ(inst, T_INSTANCE)) err_domain(inst, "<standard-object>");
    i = slot_index(INST(inst)->cls, name);
    if (i < 0) err_no_slot(inst, name);
    return VEC(INST(inst)->slots)->elt[i] != UNBOUND;
}

static void instance_set_slot(val inst, val name, val v) {
    long i;
    if (!IS_OBJ(inst, T_INSTANCE)) err_domain(inst, "<standard-object>");
    i = slot_index(INST(inst)->cls, name);
    if (i < 0) err_no_slot(inst, name);
    VEC(INST(inst)->slots)->elt[i] = v;
}

/* The slot spec with the given name from the most specific class that has one. */
static val find_slot_spec(val cls, val name) {
    val l;
    for (l = CLS(cls)->cpl; IS_CONS(l); l = CDR(l)) {
        val s;
        for (s = CLS(CAR(l))->slots; IS_CONS(s); s = CDR(s))
            if (VEC(CAR(s))->elt[SLOT_NAME] == name) return CAR(s);
    }
    return NIL;
}

/* The initform of a slot: from the most specific spec that carries one. */
static val find_initform(val cls, val name) {
    val l;
    for (l = CLS(cls)->cpl; IS_CONS(l); l = CDR(l)) {
        val s;
        for (s = CLS(CAR(l))->slots; IS_CONS(s); s = CDR(s))
            if (VEC(CAR(s))->elt[SLOT_NAME] == name && VEC(CAR(s))->elt[SLOT_INITFORM] != UNBOUND)
                return VEC(CAR(s))->elt[SLOT_INITFORM];
    }
    return UNBOUND;
}

/* Does initarg k name slot `name` in any spec along the precedence list? */
static int initarg_names_slot(val cls, val k, val name) {
    val l;
    for (l = CLS(cls)->cpl; IS_CONS(l); l = CDR(l)) {
        val s;
        for (s = CLS(CAR(l))->slots; IS_CONS(s); s = CDR(s))
            if (VEC(CAR(s))->elt[SLOT_NAME] == name) {
                val a;
                for (a = VEC(CAR(s))->elt[SLOT_INITARGS]; IS_CONS(a); a = CDR(a)) if (CAR(a) == k) return 1;
            }
    }
    return 0;
}

/* The system-supplied initialize-object: initargs then initforms, for unbound slots only. */
static void fill_slots(val inst, val initargs) {
    val cls = INST(inst)->cls;
    val names;
    long i;
    long sp = gc_sp;
    gc_push(inst); gc_push(initargs);
    for (names = CLS(cls)->allslots, i = 0; IS_CONS(names); names = CDR(names), i++) {
        val a;
        int found = 0;
        if (VEC(INST(inst)->slots)->elt[i] != UNBOUND) continue;
        for (a = initargs; IS_CONS(a) && IS_CONS(CDR(a)); a = CDR(CDR(a))) {
            if (initarg_names_slot(cls, CAR(a), CAR(names))) {
                VEC(INST(inst)->slots)->elt[i] = CAR(CDR(a));
                found = 1;
                break;
            }
        }
        if (!found) {
            val form = find_initform(cls, CAR(names));
            if (form != UNBOUND) {
                val v = eval(form, global_env);
                VEC(INST(inst)->slots)->elt[i] = v;
            }
        }
    }
    gc_sp = sp;
}

static val alloc_instance(val cls) {
    instance_t *in;
    val slots;
    long n;
    long sp = gc_sp;
    gc_push(cls);
    n = list_length(CLS(cls)->allslots);
    slots = mk_vector(n, UNBOUND);
    gc_push(slots);
    in = (instance_t *)heap_alloc(T_INSTANCE, sizeof(instance_t));
    in->cls = cls; in->slots = slots;
    gc_sp = sp;
    return (val)in;
}

static void check_creatable(val cls) {
    if (!IS_OBJ(cls, T_CLASS)) err_domain(cls, "<class>");
    if (CLS(cls)->abstractp) isl_error("<PROGRAM-ERROR>", sym_object, cls, NULL);
    if (CLS(cls)->builtin && !subclassp(cls, cls_error) && cls != cls_error)
        isl_error("<PROGRAM-ERROR>", sym_object, cls, NULL);      /* only the error classes among the built-ins */
}

/* The C-level creator (conditions from isl_error): no generic dispatch,
 * and none of create's restrictions -- the system makes a
 * <storage-exhausted> that a program may not. */
val class_create(val cls, int nargs, val *initargs) {
    val inst, args = NIL;
    int i;
    long sp = gc_sp;
    long aslot;
    if (!IS_OBJ(cls, T_CLASS)) err_domain(cls, "<class>");
    inst = alloc_instance(cls);
    gc_push(inst);
    aslot = gc_push(NIL);
    for (i = nargs - 1; i >= 0; i--) { args = mk_cons(initargs[i], args); gc_roots[aslot] = args; }
    fill_slots(inst, args);
    gc_sp = sp;
    return inst;
}

/* ---------- generic functions and methods ---------- */

static val mk_generic(val name, int nreq, int rest) {
    generic_t *g;
    long sp = gc_sp;
    gc_push(name);
    g = (generic_t *)heap_alloc(T_GENERIC, sizeof(generic_t));
    g->name = name; g->methods = NIL; g->nreq = nreq; g->rest = rest; g->combination = 1;
    gc_sp = sp;
    return (val)g;
}

static val mk_method(val specs, val qualifier, val fn, val slot, int kind, int nreq, int rest) {
    method_t *m;
    long sp = gc_sp;
    gc_push(specs); gc_push(qualifier); gc_push(fn); gc_push(slot);
    m = (method_t *)heap_alloc(T_METHOD, sizeof(method_t));
    m->specs = specs; m->qualifier = qualifier; m->fn = fn; m->slot = slot;
    m->kind = kind; m->nreq = nreq; m->rest = rest;
    gc_sp = sp;
    return (val)m;
}

static int methods_agree(val a, val b) {
    val p, q;
    if (MET(a)->qualifier != MET(b)->qualifier) return 0;
    if (MET(a)->nreq != MET(b)->nreq) return 0;
    for (p = MET(a)->specs, q = MET(b)->specs; IS_CONS(p) && IS_CONS(q); p = CDR(p), q = CDR(q))
        if (CAR(p) != CAR(q)) return 0;
    return 1;
}

/* Add or replace (a redefinition agreeing on specializers and qualifier replaces). */
static void add_method(val gf, val m) {
    val l;
    long sp = gc_sp;
    gc_push(gf); gc_push(m);
    for (l = GEN(gf)->methods; IS_CONS(l); l = CDR(l))
        if (methods_agree(CAR(l), m)) { CAR(l) = m; gc_sp = sp; return; }
    GEN(gf)->methods = mk_cons(m, GEN(gf)->methods);
    gc_sp = sp;
}

/* The generic function a (setf name) names: kept on the symbol's plist. */
static val setf_generic(val name, int create) {
    val p;
    for (p = SYM(name)->plist; IS_CONS(p) && IS_CONS(CDR(p)); p = CDR(CDR(p)))
        if (CAR(p) == sym_setf_key) return CAR(CDR(p));
    if (!create) return NIL;
    {
        val g = mk_generic(name, 2, 0);
        long sp = gc_sp;
        val cell;
        gc_push(g);
        cell = mk_cons(g, SYM(name)->plist);
        gc_push(cell);
        SYM(name)->plist = mk_cons(sym_setf_key, cell);
        gc_sp = sp;
        return g;
    }
}

val setf_writer_for(val accessor) {
    const char *name = STR(SYM(accessor)->name)->data;
    if (strcmp(name, "CAR") == 0) return SYM(intern_c("SET-CAR"))->fn;
    if (strcmp(name, "CDR") == 0) return SYM(intern_c("SET-CDR"))->fn;
    if (strcmp(name, "AREF") == 0) return SYM(intern_c("SET-AREF"))->fn;
    if (strcmp(name, "GAREF") == 0) return SYM(intern_c("SET-GAREF"))->fn;
    if (strcmp(name, "ELT") == 0) return SYM(intern_c("SET-ELT"))->fn;
    if (strcmp(name, "PROPERTY") == 0) return SYM(intern_c("SET-PROPERTY"))->fn;
    return setf_generic(accessor, 0);
}

/* Ensure a generic function named by sym (or its setf writer) exists; the
 * accessor options of defclass create them without a defgeneric. */
static val ensure_generic(val name, int nreq, int setf) {
    val g;
    if (setf) return setf_generic(name, 1);
    g = SYM(name)->fn;
    if (IS_OBJ(g, T_GENERIC)) return g;
    if (g != UNBOUND && !IS_OBJ(g, T_GENERIC)) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    g = mk_generic(name, nreq, 0);
    SYM(name)->fn = g;
    return g;
}

static val system_create(int argc, val *argv);

/* ---------- dispatch ---------- */

/* A method context is a heap vector, bound lexically in the method body's
 * environment under %METHOD-CONTEXT%, so a closure made inside a method
 * can call-next-method after the method has returned (§15.3.4: the
 * binding has indefinite extent).
 *   0 gf  1 arounds  2 befores  3 primaries  4 afters  5 next  6 phase  7 args */
#define CTX_GF 0
#define CTX_AROUNDS 1
#define CTX_BEFORES 2
#define CTX_PRIMARIES 3
#define CTX_AFTERS 4
#define CTX_NEXT 5
#define CTX_PHASE 6
#define CTX_ARGS 7
#define CTX_N 8
static val sym_method_ctx;

static int specializer_rank(val cls, val spec) {
    val l;
    int i = 0;
    for (l = CLS(cls)->cpl; IS_CONS(l); l = CDR(l), i++) if (CAR(l) == spec) return i;
    return 1 << 20;
}

/* m1 more specific than m2 for these arguments? */
static int more_specific(val m1, val m2, int argc, val *argv) {
    val p = MET(m1)->specs, q = MET(m2)->specs;
    int i;
    for (i = 0; IS_CONS(p) && IS_CONS(q) && i < argc; p = CDR(p), q = CDR(q), i++) {
        if (CAR(p) == CAR(q)) continue;
        {
            val c = class_of(argv[i]);
            return specializer_rank(c, CAR(p)) < specializer_rank(c, CAR(q));
        }
    }
    return 0;
}

static int applicable(val m, int argc, val *argv) {
    val p = MET(m)->specs;
    int i;
    if (argc < MET(m)->nreq) return 0;
    for (i = 0; IS_CONS(p); p = CDR(p), i++) {
        if (CAR(p) == cls_object) continue;
        if (!instancep(argv[i], CAR(p))) return 0;
    }
    return 1;
}

/* Insert m into a list sorted most specific first. */
static val insert_sorted(val list, val m, int argc, val *argv) {
    val head, prev;
    long sp = gc_sp;
    gc_push(list); gc_push(m);
    if (list == NIL || more_specific(m, CAR(list), argc, argv)) { head = mk_cons(m, list); gc_sp = sp; return head; }
    head = list;
    prev = list;
    while (IS_CONS(CDR(prev)) && !more_specific(m, CAR(CDR(prev)), argc, argv)) prev = CDR(prev);
    CDR(prev) = mk_cons(m, CDR(prev));
    gc_sp = sp;
    return head;
}

/* The context's arguments onto the root stack; returns the base. */
static long ctx_args(val ctx, int *argc) {
    long base = gc_sp;
    val l;
    int n = 0;
    for (l = VEC(ctx)->elt[CTX_ARGS]; IS_CONS(l); l = CDR(l)) { gc_push(CAR(l)); n++; }
    *argc = n;
    return base;
}

static val ctx_copy(val ctx, long phase, val next) {
    val c2;
    long sp = gc_sp;
    long i;
    gc_push(ctx); gc_push(next);
    c2 = mk_vector(CTX_N, NIL);
    for (i = 0; i < CTX_N; i++) VEC(c2)->elt[i] = VEC(ctx)->elt[i];
    VEC(c2)->elt[CTX_PHASE] = MKFIX(phase);
    VEC(c2)->elt[CTX_NEXT] = next;
    gc_sp = sp;
    return c2;
}

/* Invoke m with its own copy of the context (phase, and the methods that
 * follow it), so a closure it makes can call-next-method later and still
 * find the right next method. */
static val invoke_method(val m, val ctx0, long phase, val next) {
    int argc;
    long sp = gc_sp;
    long base;
    val r, ctx;
    gc_push(m); gc_push(ctx0); gc_push(next);
    ctx = ctx_copy(ctx0, phase, next);
    gc_push(ctx);
    base = ctx_args(ctx, &argc);
    switch (MET(m)->kind) {
    case MK_READER: r = instance_slot(gc_roots[base], MET(m)->slot); break;
    case MK_WRITER: instance_set_slot(gc_roots[base + 1], MET(m)->slot, gc_roots[base]); r = gc_roots[base]; break;
    case MK_BOUNDP: r = mk_bool(instance_slot_boundp(gc_roots[base], MET(m)->slot)); break;
    case MK_CREATE: r = system_create(argc, &gc_roots[base]); break;
    case MK_INIT:
        if (!IS_OBJ(gc_roots[base], T_INSTANCE)) err_domain(gc_roots[base], "<standard-object>");
        if (list_length(gc_roots[base + 1]) < 0) err_domain(gc_roots[base + 1], "<list>");
        fill_slots(gc_roots[base], gc_roots[base + 1]);
        r = gc_roots[base];
        break;
    default: {
        /* the method function with the context bound lexically around its body */
        val fn = MET(m)->fn;
        val env = mk_env(CLO(fn)->env);
        val fn2;
        gc_push(env);
        {
            val cell = mk_cons(sym_method_ctx, ctx);
            ENV(env)->vars = mk_cons(cell, ENV(env)->vars);
        }
        fn2 = mk_closure(CLO(fn)->params, CLO(fn)->rest, CLO(fn)->body, env, CLO(fn)->name);
        gc_push(fn2);
        r = apply(fn2, argc, &gc_roots[base]);
        break;
    }
    }
    gc_sp = sp;
    return r;
}

static val run_primaries(val ctx) {
    val prims = VEC(ctx)->elt[CTX_PRIMARIES];
    if (prims == NIL) isl_error("<PROGRAM-ERROR>", sym_object, GEN(VEC(ctx)->elt[CTX_GF])->name, NULL);   /* no applicable primary */
    return invoke_method(CAR(prims), ctx, 2, CDR(prims));
}

static val run_main(val ctx) {
    val l, r;
    long sp = gc_sp;
    gc_push(ctx);
    for (l = VEC(ctx)->elt[CTX_BEFORES]; IS_CONS(l); l = CDR(l)) invoke_method(CAR(l), ctx, 1, NIL);
    r = run_primaries(ctx);
    gc_push(r);
    {
        val rev = list_reverse(VEC(ctx)->elt[CTX_AFTERS]);      /* least specific first */
        gc_push(rev);
        for (l = rev; IS_CONS(l); l = CDR(l)) invoke_method(CAR(l), ctx, 3, NIL);
    }
    gc_sp = sp;
    return r;
}

static val run_arounds(val ctx) {
    val arounds = VEC(ctx)->elt[CTX_AROUNDS];
    return invoke_method(CAR(arounds), ctx, 0, CDR(arounds));
}

val generic_call(val gf, int argc, val *argv) {
    val ctx, l, r, args = NIL;
    long sp = gc_sp;
    int i;
    if (argc < GEN(gf)->nreq || (!GEN(gf)->rest && argc > GEN(gf)->nreq)) err_arity(gf, argc);
    gc_push(gf);
    ctx = mk_vector(CTX_N, NIL);
    gc_push(ctx);
    VEC(ctx)->elt[CTX_GF] = gf;
    VEC(ctx)->elt[CTX_PHASE] = MKFIX(2);
    for (i = argc - 1; i >= 0; i--) { args = mk_cons(argv[i], args); VEC(ctx)->elt[CTX_ARGS] = args; }
    for (l = GEN(gf)->methods; IS_CONS(l); l = CDR(l)) {
        val m = CAR(l);
        val q;
        if (!applicable(m, argc, argv)) continue;
        q = MET(m)->qualifier;
        if (GEN(gf)->combination == 0) {
            if (q != NIL) isl_error("<PROGRAM-ERROR>", sym_object, q, NULL);
            VEC(ctx)->elt[CTX_PRIMARIES] = insert_sorted(VEC(ctx)->elt[CTX_PRIMARIES], m, argc, argv);
        } else if (q == NIL) VEC(ctx)->elt[CTX_PRIMARIES] = insert_sorted(VEC(ctx)->elt[CTX_PRIMARIES], m, argc, argv);
        else if (q == sym_around) VEC(ctx)->elt[CTX_AROUNDS] = insert_sorted(VEC(ctx)->elt[CTX_AROUNDS], m, argc, argv);
        else if (q == sym_before) VEC(ctx)->elt[CTX_BEFORES] = insert_sorted(VEC(ctx)->elt[CTX_BEFORES], m, argc, argv);
        else if (q == sym_after) VEC(ctx)->elt[CTX_AFTERS] = insert_sorted(VEC(ctx)->elt[CTX_AFTERS], m, argc, argv);
        else isl_error("<PROGRAM-ERROR>", sym_object, q, NULL);        /* unrecognized qualifier */
    }
    if (VEC(ctx)->elt[CTX_AROUNDS] == NIL && VEC(ctx)->elt[CTX_BEFORES] == NIL &&
        VEC(ctx)->elt[CTX_PRIMARIES] == NIL && VEC(ctx)->elt[CTX_AFTERS] == NIL)
        err_undefined_function(GEN(gf)->name);                            /* no applicable method */
    if (VEC(ctx)->elt[CTX_AROUNDS] != NIL) r = run_arounds(ctx);
    else r = run_main(ctx);
    gc_sp = sp;
    return r;
}

/* (call-next-method) and (next-method-p): the lexically visible context. */
static val current_ctx(val env, const char *who) {
    for (; env != NIL; env = ENV(env)->parent) {
        val l;
        for (l = ENV(env)->vars; IS_CONS(l); l = CDR(l))
            if (CAR(CAR(l)) == sym_method_ctx) return CDR(CAR(l));
    }
    err_undefined_function(intern_c(who));     /* a local function: unbound outside a method */
    return NIL;
}

val ilos_call_next_method(val env) {
    val ctx = current_ctx(env, "CALL-NEXT-METHOD");
    val next, r;
    long phase;
    long sp = gc_sp;
    gc_push(ctx);
    next = VEC(ctx)->elt[CTX_NEXT];
    phase = FIXVAL(VEC(ctx)->elt[CTX_PHASE]);
    if (phase == 1 || phase == 3) isl_error("<PROGRAM-ERROR>", NULL);   /* not in :before / :after */
    if (phase == 0) {
        if (next != NIL) r = invoke_method(CAR(next), ctx, 0, CDR(next));
        else r = run_main(ctx);
    } else {
        if (next == NIL) isl_error("<PROGRAM-ERROR>", NULL);               /* no next method */
        r = invoke_method(CAR(next), ctx, 2, CDR(next));
    }
    gc_sp = sp;
    return r;
}

val ilos_next_method_p(val env) {
    val ctx = current_ctx(env, "NEXT-METHOD-P");
    long phase = FIXVAL(VEC(ctx)->elt[CTX_PHASE]);
    if (phase == 0) return T;                        /* the main sequence always follows an :around */
    if (phase == 2) return mk_bool(VEC(ctx)->elt[CTX_NEXT] != NIL);
    return NIL;
}

/* ---------- defclass / defgeneric / defmethod ---------- */

static int reserved_name(val sym) {
    const char *n = STR(SYM(sym)->name)->data;
    return strchr(n, ':') != NULL || strchr(n, '&') != NULL;
}

static void need_identifier(val x) {
    if (!IS_SYM(x)) err_domain(x, "<symbol>");
    if (reserved_name(x) || x == NIL || x == T) isl_error("<PROGRAM-ERROR>", sym_object, x, NULL);
}

static void parse_profile(val profile, val *vars, val *specs, val *rest, int *nreq) {
    val v = NIL, s = NIL, p;
    long sp = gc_sp;
    long vs = gc_push(NIL), ss = gc_push(NIL);
    *rest = NIL; *nreq = 0;
    if (list_length(profile) < 0) isl_error("<PROGRAM-ERROR>", NULL);
    for (p = profile; IS_CONS(p); p = CDR(p)) {
        val x = CAR(p);
        if (x == sym_rest || x == sym_amp_rest) {
            p = CDR(p);
            if (!IS_CONS(p) || CDR(p) != NIL) isl_error("<PROGRAM-ERROR>", NULL);
            need_identifier(CAR(p));
            *rest = CAR(p);
            break;
        }
        if (IS_CONS(x)) {
            val cls;
            if (list_length(x) != 2) isl_error("<PROGRAM-ERROR>", NULL);
            need_identifier(CAR(x));
            cls = find_class_sym(CAR(CDR(x)), 1);
            v = mk_cons(CAR(x), v); gc_roots[vs] = v;
            s = mk_cons(cls, s); gc_roots[ss] = s;
        } else {
            need_identifier(x);
            v = mk_cons(x, v); gc_roots[vs] = v;
            s = mk_cons(cls_object, s); gc_roots[ss] = s;
        }
        (*nreq)++;
    }
    *vars = list_nreverse(v);
    *specs = list_nreverse(s);
    gc_sp = sp;
}

/* func-spec: identifier or (setf identifier); returns the symbol, sets *setf */
static val parse_func_spec(val spec, int *setf) {
    *setf = 0;
    if (IS_CONS(spec) && CAR(spec) == intern_c("SETF") && IS_CONS(CDR(spec)) && CDR(CDR(spec)) == NIL && IS_SYM(CAR(CDR(spec)))) {
        *setf = 1;
        need_identifier(CAR(CDR(spec)));
        return CAR(CDR(spec));
    }
    if (!IS_SYM(spec)) err_domain(spec, "<symbol>");
    need_identifier(spec);
    return spec;
}

static void define_method(val gf, val qualifier, val profile, val body, val env) {
    val vars, specs, rest, fn, m, ll;
    int nreq;
    long sp = gc_sp;
    gc_push(gf); gc_push(qualifier); gc_push(profile); gc_push(body); gc_push(env);
    parse_profile(profile, &vars, &specs, &rest, &nreq);
    gc_push(vars); gc_push(specs); gc_push(rest);
    if (nreq != GEN(gf)->nreq || (rest != NIL) != (GEN(gf)->rest != 0)) isl_error("<PROGRAM-ERROR>", sym_object, GEN(gf)->name, NULL);
    if (qualifier != NIL && qualifier != sym_before && qualifier != sym_after && qualifier != sym_around)
        isl_error("<PROGRAM-ERROR>", sym_object, qualifier, NULL);
    if (qualifier != NIL && GEN(gf)->combination == 0)
        isl_error("<PROGRAM-ERROR>", sym_object, qualifier, NULL);      /* the nil combination has primary methods only */
    /* the method function: a closure over the lambda list (var ... [:rest r]) */
    ll = vars;
    if (rest != NIL) {
        val tail = list2(sym_rest, rest);
        gc_push(tail);
        ll = NIL;
        {
            val l, acc = NIL;
            long aslot = gc_push(NIL);
            for (l = vars; IS_CONS(l); l = CDR(l)) { acc = mk_cons(CAR(l), acc); gc_roots[aslot] = acc; }
            acc = list_nreverse(acc);
            if (acc == NIL) ll = tail;
            else { val e = acc; while (CDR(e) != NIL) e = CDR(e); CDR(e) = tail; ll = acc; }
        }
    }
    gc_push(ll);
    fn = eval(list3(sym_lambda, ll, NIL), env);     /* a closure with an empty body, then the real body */
    CLO(fn)->body = body;
    CLO(fn)->name = GEN(gf)->name;
    gc_push(fn);
    m = mk_method(specs, qualifier, fn, NIL, MK_FN, nreq, rest != NIL);
    add_method(gf, m);
    gc_sp = sp;
}

val sf_defgeneric(val form, val env) {
    val spec, name, ll, opts, g;
    int setf, nreq = 0, rest = 0;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    if (list_length(form) < 3) isl_error("<PROGRAM-ERROR>", NULL);
    spec = CAR(CDR(form));
    name = parse_func_spec(spec, &setf);
    ll = CAR(CDR(CDR(form)));
    if (list_length(ll) < 0) isl_error("<PROGRAM-ERROR>", NULL);
    {
        val p;
        for (p = ll; IS_CONS(p); p = CDR(p)) {
            if (CAR(p) == sym_rest || CAR(p) == sym_amp_rest) {
                p = CDR(p);
                if (!IS_CONS(p) || CDR(p) != NIL) isl_error("<PROGRAM-ERROR>", NULL);
                need_identifier(CAR(p));
                rest = 1;
                break;
            }
            need_identifier(CAR(p));
            nreq++;
        }
    }
    /* Build and validate before installing: a malformed defgeneric must not
     * have replaced the function it names (the suite tries create). */
    if (!setf && SYM(name)->special) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    g = mk_generic(name, nreq, rest);
    gc_push(g);
    for (opts = CDR(CDR(CDR(form))); IS_CONS(opts); opts = CDR(opts)) {
        val o = CAR(opts);
        if (!IS_CONS(o)) isl_error("<PROGRAM-ERROR>", NULL);
        if (CAR(o) == intern_c(":METHOD-COMBINATION")) {
            val k = IS_CONS(CDR(o)) ? CAR(CDR(o)) : NIL;
            if (k == NIL) GEN(g)->combination = 0;
            else if (k == intern_c("STANDARD")) GEN(g)->combination = 1;
            else isl_error("<PROGRAM-ERROR>", sym_object, k, NULL);
        } else if (CAR(o) == intern_c(":GENERIC-FUNCTION-CLASS")) {
            val k = IS_CONS(CDR(o)) ? CAR(CDR(o)) : NIL;
            if (k != intern_c("<STANDARD-GENERIC-FUNCTION>")) isl_error("<PROGRAM-ERROR>", sym_object, k, NULL);
        } else if (CAR(o) == intern_c(":METHOD")) {
            val rest_o = CDR(o), qual = NIL;
            if (IS_CONS(rest_o) && IS_SYM(CAR(rest_o)) && CAR(rest_o) != NIL) { qual = CAR(rest_o); rest_o = CDR(rest_o); }
            if (!IS_CONS(rest_o)) isl_error("<PROGRAM-ERROR>", NULL);
            define_method(g, qual, CAR(rest_o), CDR(rest_o), env);
        } else isl_error("<PROGRAM-ERROR>", sym_object, CAR(o), NULL);
    }
    if (setf) {
        val old_g = setf_generic(name, 1);
        GEN(old_g)->methods = GEN(g)->methods; GEN(old_g)->nreq = nreq; GEN(old_g)->rest = rest;
        GEN(old_g)->combination = GEN(g)->combination;
    } else {
        SYM(name)->fn = g;
    }
    gc_sp = sp;
    return name;
}

val sf_defmethod(val form, val env) {
    val spec, name, g, rest_f, qual = NIL;
    int setf;
    long sp = gc_sp;
    gc_push(form); gc_push(env);
    if (list_length(form) < 3) isl_error("<PROGRAM-ERROR>", NULL);
    spec = CAR(CDR(form));
    name = parse_func_spec(spec, &setf);
    rest_f = CDR(CDR(form));
    if (IS_SYM(CAR(rest_f)) && CAR(rest_f) != NIL) { qual = CAR(rest_f); rest_f = CDR(rest_f); }
    if (!IS_CONS(rest_f)) isl_error("<PROGRAM-ERROR>", NULL);
    if (!setf && SYM(name)->special) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    if (setf) g = setf_generic(name, 0);
    else g = SYM(name)->fn;
    if (g == NIL || g == UNBOUND) err_undefined_function(name);
    if (!IS_OBJ(g, T_GENERIC)) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    define_method(g, qual, CAR(rest_f), CDR(rest_f), env);
    gc_sp = sp;
    return name;
}

static void add_accessor_method(val gfname, int setf, val cls, val slot, int kind) {
    val g, specs, m;
    long sp = gc_sp;
    gc_push(gfname); gc_push(cls); gc_push(slot);
    g = ensure_generic(gfname, kind == MK_WRITER ? 2 : 1, setf);
    gc_push(g);
    if (GEN(g)->nreq != (kind == MK_WRITER ? 2 : 1) || GEN(g)->rest) isl_error("<PROGRAM-ERROR>", sym_object, gfname, NULL);
    if (kind == MK_WRITER) specs = list2(cls_object, cls);
    else specs = list1(cls);
    gc_push(specs);
    m = mk_method(specs, NIL, NIL, slot, kind, kind == MK_WRITER ? 2 : 1, 0);
    add_method(g, m);
    gc_sp = sp;
}

val sf_defclass(val form, val env) {
    val name, scnames, slotspecs, opts, supers = NIL, cls, l, specs = NIL, own = NIL, eff = NIL;
    long sp = gc_sp;
    long sup_slot, spec_slot, own_slot, eff_slot;
    (void)env;
    gc_push(form);
    if (list_length(form) < 4) isl_error("<PROGRAM-ERROR>", NULL);
    name = CAR(CDR(form));
    need_identifier(name);
    if (SYM(name)->special) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    if (SYM(name)->cls != NIL && CLS(SYM(name)->cls)->builtin) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
    scnames = CAR(CDR(CDR(form)));
    slotspecs = CAR(CDR(CDR(CDR(form))));
    opts = CDR(CDR(CDR(CDR(form))));
    if (list_length(scnames) < 0 || list_length(slotspecs) < 0 || list_length(opts) < 0) isl_error("<PROGRAM-ERROR>", NULL);
    sup_slot = gc_push(NIL);
    for (l = scnames; IS_CONS(l); l = CDR(l)) {
        val sc = find_class_sym(CAR(l), 1);
        val m;
        for (m = supers; IS_CONS(m); m = CDR(m)) if (CAR(m) == sc) isl_error("<PROGRAM-ERROR>", sym_object, CAR(l), NULL);
        if (CLS(sc)->builtin && sc != cls_standard_object) isl_error("<PROGRAM-ERROR>", sym_object, CAR(l), NULL);
        supers = mk_cons(sc, supers); gc_roots[sup_slot] = supers;
    }
    supers = list_nreverse(supers);
    if (supers == NIL) supers = list1(cls_standard_object);
    gc_roots[sup_slot] = supers;
    cls = mk_class_obj(name, supers, 0);
    gc_push(cls);
    CLS(cls)->cpl = compute_cpl(cls);
    /* the restricted multiple inheritance of §10 / §15.1.1 */
    for (l = supers; IS_CONS(l); l = CDR(l)) {
        val m;
        for (m = CDR(l); IS_CONS(m); m = CDR(m)) {
            val a;
            for (a = CLS(CAR(l))->cpl; IS_CONS(a); a = CDR(a)) {
                val b;
                if (CAR(a) == cls_standard_object || CAR(a) == cls_object) continue;
                for (b = CLS(CAR(m))->cpl; IS_CONS(b); b = CDR(b)) if (CAR(b) == CAR(a)) isl_error("<PROGRAM-ERROR>", sym_object, name, NULL);
            }
        }
    }
    /* slot specs */
    spec_slot = gc_push(NIL); own_slot = gc_push(NIL);
    for (l = slotspecs; IS_CONS(l); l = CDR(l)) {
        val ss = CAR(l), sname, o, spec, initargs = NIL;
        long ia_slot;
        if (IS_SYM(ss)) { sname = ss; o = NIL; }
        else if (IS_CONS(ss)) { sname = CAR(ss); o = CDR(ss); if (list_length(o) < 0) isl_error("<PROGRAM-ERROR>", NULL); }
        else { err_domain(ss, "<symbol>"); sname = NIL; o = NIL; }
        need_identifier(sname);
        {
            val m;
            for (m = own; IS_CONS(m); m = CDR(m)) if (CAR(m) == sname) isl_error("<PROGRAM-ERROR>", sym_object, sname, NULL);
        }
        spec = mk_vector(SLOT_NFIELDS, NIL);
        VEC(spec)->elt[SLOT_NAME] = sname;
        VEC(spec)->elt[SLOT_INITFORM] = UNBOUND;
        gc_push(spec);
        ia_slot = gc_push(NIL);
        for (; IS_CONS(o); o = CDR(o)) {
            val key = CAR(o), arg;
            if (!IS_CONS(CDR(o))) isl_error("<PROGRAM-ERROR>", sym_object, key, NULL);
            arg = CAR(CDR(o));
            o = CDR(o);
            if (key == sym_initform) {
                if (VEC(spec)->elt[SLOT_INITFORM] != UNBOUND) isl_error("<PROGRAM-ERROR>", sym_object, key, NULL);
                VEC(spec)->elt[SLOT_INITFORM] = arg;
            } else if (key == sym_initarg) {
                need_identifier(arg);
                if (initargs != NIL) isl_error("<PROGRAM-ERROR>", sym_object, key, NULL);   /* once per slot */
                initargs = mk_cons(arg, initargs); gc_roots[ia_slot] = initargs;
            } else if (key == sym_reader) { need_identifier(arg); add_accessor_method(arg, 0, cls, sname, MK_READER); }
            else if (key == sym_writer) { need_identifier(arg); add_accessor_method(arg, 0, cls, sname, MK_WRITER); }
            else if (key == sym_accessor) {
                need_identifier(arg);
                add_accessor_method(arg, 0, cls, sname, MK_READER);
                add_accessor_method(arg, 1, cls, sname, MK_WRITER);
            } else if (key == sym_boundp) { need_identifier(arg); add_accessor_method(arg, 0, cls, sname, MK_BOUNDP); }
            else isl_error("<PROGRAM-ERROR>", sym_object, key, NULL);
        }
        VEC(spec)->elt[SLOT_INITARGS] = list_nreverse(initargs);
        gc_sp = ia_slot;
        specs = mk_cons(spec, specs); gc_roots[spec_slot] = specs;
        own = mk_cons(sname, own); gc_roots[own_slot] = own;
        gc_sp = own_slot + 1;
    }
    CLS(cls)->slots = list_nreverse(specs);
    own = list_nreverse(own);
    gc_roots[own_slot] = own;
    /* effective slots: inherited ones (least specific first), then this class's new ones */
    eff_slot = gc_push(NIL);
    {
        val rev = list_reverse(CLS(cls)->cpl);
        gc_push(rev);
        for (l = rev; IS_CONS(l); l = CDR(l)) {
            val m;
            if (CAR(l) == cls) continue;
            for (m = CLS(CAR(l))->allslots; IS_CONS(m); m = CDR(m)) {
                val k;
                int seen = 0;
                for (k = eff; IS_CONS(k); k = CDR(k)) if (CAR(k) == CAR(m)) seen = 1;
                if (!seen) { eff = mk_cons(CAR(m), eff); gc_roots[eff_slot] = eff; }
            }
        }
        for (l = own; IS_CONS(l); l = CDR(l)) {
            val k;
            int seen = 0;
            for (k = eff; IS_CONS(k); k = CDR(k)) if (CAR(k) == CAR(l)) seen = 1;
            if (!seen) { eff = mk_cons(CAR(l), eff); gc_roots[eff_slot] = eff; }
        }
    }
    CLS(cls)->allslots = list_nreverse(eff);
    /* class options, each at most once */
    {
        int seen_abstract = 0, seen_meta = 0;
    for (l = opts; IS_CONS(l); l = CDR(l)) {
        val o = CAR(l);
        if (!IS_CONS(o) || list_length(o) != 2) isl_error("<PROGRAM-ERROR>", NULL);
        if (CAR(o) == sym_abstractp) {
            int flag;
            if (CAR(CDR(o)) == T) flag = 1;
            else if (CAR(CDR(o)) == NIL) flag = 0;
            else { isl_error("<PROGRAM-ERROR>", sym_object, CAR(CDR(o)), NULL); flag = 0; }
            /* repeated with the same value is tolerated (the suite does it); a conflict is not */
            if (seen_abstract++ && CLS(cls)->abstractp != flag) isl_error("<PROGRAM-ERROR>", sym_object, CAR(o), NULL);
            CLS(cls)->abstractp = flag;
        } else if (CAR(o) == sym_metaclass) {
            if (seen_meta++) isl_error("<PROGRAM-ERROR>", sym_object, CAR(o), NULL);
            if (CAR(CDR(o)) != intern_c("<STANDARD-CLASS>")) isl_error("<PROGRAM-ERROR>", sym_object, CAR(CDR(o)), NULL);
        } else isl_error("<PROGRAM-ERROR>", sym_object, CAR(o), NULL);
    }
    }
    SYM(name)->cls = cls;
    gc_sp = sp;
    return name;
}

/* ---------- builtins ---------- */

static val bi_class_of(int argc, val *argv) { (void)argc; return class_of(argv[0]); }
static val bi_instancep(int argc, val *argv) {
    (void)argc;
    if (!IS_OBJ(argv[1], T_CLASS)) err_domain(argv[1], "<class>");
    return mk_bool(instancep(argv[0], argv[1]));
}
static val bi_subclassp(int argc, val *argv) {
    (void)argc;
    if (!IS_OBJ(argv[0], T_CLASS)) err_domain(argv[0], "<class>");
    if (!IS_OBJ(argv[1], T_CLASS)) err_domain(argv[1], "<class>");
    return mk_bool(subclassp(argv[0], argv[1]));
}
static val bi_generic_function_p(int argc, val *argv) { (void)argc; return mk_bool(IS_OBJ(argv[0], T_GENERIC)); }

/* The system method of the generic function create (§15.4): allocate,
 * then initialize-object with the initarg list. */
static val system_create(int argc, val *argv) {
    val inst, args = NIL, r;
    int i;
    long sp = gc_sp;
    long aslot, base;
    if (!IS_OBJ(argv[0], T_CLASS)) err_domain(argv[0], "<class>");
    if ((argc - 1) % 2) isl_error("<PROGRAM-ERROR>", NULL);
    check_creatable(argv[0]);
    inst = alloc_instance(argv[0]);
    gc_push(inst);
    aslot = gc_push(NIL);
    for (i = argc - 1; i >= 1; i--) { args = mk_cons(argv[i], args); gc_roots[aslot] = args; }
    base = gc_sp;
    gc_push(inst); gc_push(args);
    r = generic_call(gf_initialize_object, 2, &gc_roots[base]);
    gc_sp = sp;
    (void)r;
    return inst;
}

/* The built-in condition classes have slots whose initargs are the slot names. */
static void cond_slots_arr(val cls, val *names, int n) {
    val specs = NIL;
    int i;
    long sp = gc_sp;
    long s1 = gc_push(NIL);
    for (i = n - 1; i >= 0; i--) {
        val spec = mk_vector(SLOT_NFIELDS, NIL);
        VEC(spec)->elt[SLOT_NAME] = names[i];
        VEC(spec)->elt[SLOT_INITARGS] = list1(names[i]);
        VEC(spec)->elt[SLOT_INITFORM] = UNBOUND;
        specs = mk_cons(spec, specs); gc_roots[s1] = specs;
    }
    CLS(cls)->slots = specs;
    gc_sp = sp;
}

static void inherit_slots(val cls) {
    val l, eff = NIL;
    long sp = gc_sp;
    long s = gc_push(NIL);
    val rev = list_reverse(CLS(cls)->cpl);
    gc_push(rev);
    for (l = rev; IS_CONS(l); l = CDR(l)) {
        val m;
        for (m = CLS(CAR(l))->slots; IS_CONS(m); m = CDR(m)) {
            val k;
            int seen = 0;
            for (k = eff; IS_CONS(k); k = CDR(k)) if (CAR(k) == VEC(CAR(m))->elt[SLOT_NAME]) seen = 1;
            if (!seen) { eff = mk_cons(VEC(CAR(m))->elt[SLOT_NAME], eff); gc_roots[s] = eff; }
        }
    }
    CLS(cls)->allslots = list_nreverse(eff);
    gc_sp = sp;
}

void class_init(void) {
    val two[4];
    sym_setf_key = intern_c("%SETF-WRITER%");
    sym_method_ctx = intern_c("%METHOD-CONTEXT%");
    cls_object = mk_class("<OBJECT>", NIL, 1); finish_builtin(cls_object);
    cls_basic_array = mk_class("<BASIC-ARRAY>", list1(cls_object), 1); finish_builtin(cls_basic_array);
    cls_basic_array_star = mk_class("<BASIC-ARRAY*>", list1(cls_basic_array), 1); finish_builtin(cls_basic_array_star);
    cls_general_array_star = mk_class("<GENERAL-ARRAY*>", list1(cls_basic_array_star), 1); finish_builtin(cls_general_array_star);
    cls_basic_vector = mk_class("<BASIC-VECTOR>", list1(cls_basic_array), 1); finish_builtin(cls_basic_vector);
    cls_general_vector = mk_class("<GENERAL-VECTOR>", list1(cls_basic_vector), 1); finish_builtin(cls_general_vector);
    cls_string = mk_class("<STRING>", list1(cls_basic_vector), 1); finish_builtin(cls_string);
    cls_built_in_class = mk_class("<BUILT-IN-CLASS>", list1(cls_object), 1); finish_builtin(cls_built_in_class);
    cls_character = mk_class("<CHARACTER>", list1(cls_object), 1); finish_builtin(cls_character);
    cls_function = mk_class("<FUNCTION>", list1(cls_object), 1); finish_builtin(cls_function);
    cls_generic_function = mk_class("<GENERIC-FUNCTION>", list1(cls_function), 1); finish_builtin(cls_generic_function);
    cls_standard_generic_function = mk_class("<STANDARD-GENERIC-FUNCTION>", list1(cls_generic_function), 1); finish_builtin(cls_standard_generic_function);
    cls_list = mk_class("<LIST>", list1(cls_object), 1); finish_builtin(cls_list);
    cls_cons = mk_class("<CONS>", list1(cls_list), 1); finish_builtin(cls_cons);
    cls_symbol = mk_class("<SYMBOL>", list1(cls_object), 1); finish_builtin(cls_symbol);
    cls_null = mk_class("<NULL>", list2(cls_symbol, cls_list), 1); finish_builtin(cls_null);
    cls_number = mk_class("<NUMBER>", list1(cls_object), 1); finish_builtin(cls_number);
    cls_float = mk_class("<FLOAT>", list1(cls_number), 1); finish_builtin(cls_float);
    cls_integer = mk_class("<INTEGER>", list1(cls_number), 1); finish_builtin(cls_integer);
    cls_serious_condition = mk_class("<SERIOUS-CONDITION>", list1(cls_object), 1); finish_builtin(cls_serious_condition);
    cls_error = mk_class("<ERROR>", list1(cls_serious_condition), 1); finish_builtin(cls_error);
    cls_arithmetic_error = mk_class("<ARITHMETIC-ERROR>", list1(cls_error), 1); finish_builtin(cls_arithmetic_error);
    cls_division_by_zero = mk_class("<DIVISION-BY-ZERO>", list1(cls_arithmetic_error), 1); finish_builtin(cls_division_by_zero);
    cls_fp_overflow = mk_class("<FLOATING-POINT-OVERFLOW>", list1(cls_arithmetic_error), 1); finish_builtin(cls_fp_overflow);
    cls_fp_underflow = mk_class("<FLOATING-POINT-UNDERFLOW>", list1(cls_arithmetic_error), 1); finish_builtin(cls_fp_underflow);
    cls_control_error = mk_class("<CONTROL-ERROR>", list1(cls_error), 1); finish_builtin(cls_control_error);
    cls_parse_error = mk_class("<PARSE-ERROR>", list1(cls_error), 1); finish_builtin(cls_parse_error);
    cls_program_error = mk_class("<PROGRAM-ERROR>", list1(cls_error), 1); finish_builtin(cls_program_error);
    cls_domain_error = mk_class("<DOMAIN-ERROR>", list1(cls_program_error), 1); finish_builtin(cls_domain_error);
    cls_undefined_entity = mk_class("<UNDEFINED-ENTITY>", list1(cls_program_error), 1); finish_builtin(cls_undefined_entity);
    cls_unbound_variable = mk_class("<UNBOUND-VARIABLE>", list1(cls_undefined_entity), 1); finish_builtin(cls_unbound_variable);
    cls_undefined_function = mk_class("<UNDEFINED-FUNCTION>", list1(cls_undefined_entity), 1); finish_builtin(cls_undefined_function);
    cls_simple_error = mk_class("<SIMPLE-ERROR>", list1(cls_error), 1); finish_builtin(cls_simple_error);
    cls_stream_error = mk_class("<STREAM-ERROR>", list1(cls_error), 1); finish_builtin(cls_stream_error);
    cls_end_of_stream = mk_class("<END-OF-STREAM>", list1(cls_stream_error), 1); finish_builtin(cls_end_of_stream);
    cls_storage_exhausted = mk_class("<STORAGE-EXHAUSTED>", list1(cls_serious_condition), 1); finish_builtin(cls_storage_exhausted);
    cls_standard_class = mk_class("<STANDARD-CLASS>", list1(cls_object), 1); finish_builtin(cls_standard_class);
    cls_standard_object = mk_class("<STANDARD-OBJECT>", list1(cls_object), 1); finish_builtin(cls_standard_object);
    cls_stream = mk_class("<STREAM>", list1(cls_object), 1); finish_builtin(cls_stream);
    two[0] = sym_operation; two[1] = sym_operands;
    cond_slots_arr(cls_arithmetic_error, two, 2);
    inherit_slots(cls_arithmetic_error); inherit_slots(cls_division_by_zero); inherit_slots(cls_fp_overflow); inherit_slots(cls_fp_underflow);
    two[0] = sym_object; two[1] = sym_expected_class;
    cond_slots_arr(cls_domain_error, two, 2); inherit_slots(cls_domain_error);
    two[0] = sym_string; two[1] = sym_expected_class;
    cond_slots_arr(cls_parse_error, two, 2); inherit_slots(cls_parse_error);
    two[0] = sym_format_string; two[1] = sym_format_arguments;
    cond_slots_arr(cls_simple_error, two, 2); inherit_slots(cls_simple_error);
    two[0] = sym_stream;
    cond_slots_arr(cls_stream_error, two, 1); inherit_slots(cls_stream_error); inherit_slots(cls_end_of_stream);
    two[0] = sym_name; two[1] = sym_namespace;
    cond_slots_arr(cls_undefined_entity, two, 2); inherit_slots(cls_undefined_entity); inherit_slots(cls_unbound_variable); inherit_slots(cls_undefined_function);
    /* initialize-object: a generic function with the system-supplied primary method */
    gf_initialize_object = mk_generic(intern_c("INITIALIZE-OBJECT"), 2, 0);
    SYM(intern_c("INITIALIZE-OBJECT"))->fn = gf_initialize_object;
    {
        val specs = list2(cls_object, cls_object);
        val m = mk_method(specs, NIL, NIL, NIL, MK_INIT, 2, 0);
        add_method(gf_initialize_object, m);
    }
    defbuiltin("CLASS-OF", bi_class_of, 1, 1);
    defbuiltin("INSTANCEP", bi_instancep, 2, 2);
    defbuiltin("SUBCLASSP", bi_subclassp, 2, 2);
    defbuiltin("GENERIC-FUNCTION-P", bi_generic_function_p, 1, 1);
    {
        /* create: a generic function of (class :rest initargs) with the system method */
        val gf = mk_generic(intern_c("CREATE"), 1, 1);
        val m;
        SYM(intern_c("CREATE"))->fn = gf;
        m = mk_method(list1(cls_object), NIL, NIL, NIL, MK_CREATE, 1, 1);
        add_method(gf, m);
    }
}
