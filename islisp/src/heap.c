/* heap.c -- objects, the collector, and the root stack.
 *
 * Every object is malloc'd and threaded on one list for the sweep; the
 * collector is a precise mark-sweep whose roots are the symbol table,
 * the global environment, the dynamic-binding stack, the non-local-exit
 * chain, and an explicit root stack that C code pushes live values on
 * (GC_PROTECT) while it allocates.  The root stack doubles as the
 * argument stack: eval pushes evaluated arguments on it and hands the
 * callee a pointer into it, so arguments are protected for free.
 *
 * Collection runs only from heap_alloc, when the allocation count since
 * the last run passes a threshold that grows with the live heap.  A
 * malloc per object is the simplest thing that works; size classes
 * come when a measurement asks for them (docs/plans/islisp.md).
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>
#include <setjmp.h>

static obj *all_objects;
long heap_live, heap_allocs, gc_runs;
static long gc_threshold = 200000;

/* Every object's address, in an open-addressing set, so the collector
 * can tell a heap pointer from any other word it finds on the C stack.
 * The precise roots are the real mechanism; the stack scan catches the
 * value a C function holds in a local across an allocation without
 * having pushed it -- the bug that is otherwise found one wrong answer
 * at a time, and never completely. */
static uintptr_t *obj_set;
static long obj_set_cap, obj_set_n;
val *gc_stack_base;

static void obj_set_insert_raw(uintptr_t a) {
    long i = (long)((a >> 4) * 2654435761u) & (obj_set_cap - 1);
    while (obj_set[i]) i = (i + 1) & (obj_set_cap - 1);
    obj_set[i] = a;
    obj_set_n++;
}

/* The set keeps its storage once it has grown: rebuilt after every
 * collection, a set freed and calloc'd anew left its old block to be
 * carved up by the objects allocated next, and on a first-fit malloc
 * (SLOW-32's) each cycle took fresh heap for the set until a program
 * allocating two million conses with a few thousand live ran out of
 * its 32MB. */
static void obj_set_rebuild(long want) {
    obj *o;
    long cap = 1024;
    while (cap < want * 2) cap *= 2;
    if (cap <= obj_set_cap) {
        cap = obj_set_cap;
        memset(obj_set, 0, (size_t)cap * sizeof(uintptr_t));
    } else {
        free(obj_set);
        obj_set = calloc((size_t)cap, sizeof(uintptr_t));
        if (!obj_set) fatal("out of memory (object set)");
    }
    obj_set_cap = cap;
    obj_set_n = 0;
    for (o = all_objects; o; o = o->gcnext) obj_set_insert_raw((uintptr_t)o);
}

static void obj_set_insert(obj *o) {
    if (obj_set_n * 2 >= obj_set_cap) obj_set_rebuild(obj_set_n + 1);
    obj_set_insert_raw((uintptr_t)o);
}

static int obj_set_has(uintptr_t a) {
    long i;
    if (!obj_set_cap || (a & 3u) != 0 || a == 0) return 0;
    i = (long)((a >> 4) * 2654435761u) & (obj_set_cap - 1);
    while (obj_set[i]) {
        if (obj_set[i] == a) return 1;
        i = (i + 1) & (obj_set_cap - 1);
    }
    return 0;
}

val *gc_roots;
long gc_sp, gc_cap;

static val *mark_stack;
static long mark_sp, mark_cap;

void fatal(const char *msg) {
    fputs("islisp: ", stderr);
    fputs(msg, stderr);
    fputc('\n', stderr);
    exit(70);
}

/* The root stack never moves: eval hands callees pointers into it as
 * their argument vectors, so a realloc here would leave them dangling.
 * Its capacity is fixed at start; exhausting it is a fatal error, the
 * same as exhausting the C stack. */
long gc_push(val v) {
    if (gc_sp >= gc_cap) fatal("root stack exhausted (recursion too deep)");
    gc_roots[gc_sp] = v;
    return gc_sp++;
}

void gc_pop_to(long sp) { gc_sp = sp; }

void heap_init(void) {
    gc_cap = 262144;
    gc_roots = malloc(gc_cap * sizeof(val));
    mark_cap = 4096;
    mark_stack = malloc(mark_cap * sizeof(val));
    if (!gc_roots || !mark_stack) fatal("out of memory at start");
}

static void mark_push(val v) {
    if (!IS_PTR(v)) return;
    if (((obj *)v)->mark) return;
    ((obj *)v)->mark = 1;
    if (mark_sp >= mark_cap) {
        mark_cap *= 2;
        mark_stack = realloc(mark_stack, mark_cap * sizeof(val));
        if (!mark_stack) fatal("mark stack exhausted");
    }
    mark_stack[mark_sp++] = v;
}

static void mark_children(val v) {
    long i;
    switch (TAG(v)) {
    case T_CONS: mark_push(CAR(v)); mark_push(CDR(v)); break;
    case T_SYMBOL: {
        symbol_t *s = SYM(v);
        mark_push(s->name); mark_push(s->fn); mark_push(s->value);
        mark_push(s->dyn); mark_push(s->plist); mark_push(s->cls);
        break;
    }
    case T_VECTOR: for (i = 0; i < VEC(v)->n; i++) mark_push(VEC(v)->elt[i]); break;
    case T_ARRAY: mark_push(ARR(v)->dims); mark_push(ARR(v)->data); break;
    case T_CLOSURE: {
        closure_t *c = CLO(v);
        mark_push(c->params); mark_push(c->rest); mark_push(c->body); mark_push(c->env); mark_push(c->name);
        break;
    }
    case T_BUILTIN: mark_push(BI(v)->sym); break;
    case T_MACRO: mark_push(MAC(v)->expander); mark_push(MAC(v)->name); break;
    case T_GENERIC: mark_push(GEN(v)->name); mark_push(GEN(v)->methods); break;
    case T_METHOD: mark_push(MET(v)->specs); mark_push(MET(v)->qualifier); mark_push(MET(v)->fn); mark_push(MET(v)->slot); break;
    case T_CLASS: {
        class_t *c = CLS(v);
        mark_push(c->name); mark_push(c->supers); mark_push(c->cpl); mark_push(c->slots); mark_push(c->allslots);
        break;
    }
    case T_INSTANCE: mark_push(INST(v)->cls); mark_push(INST(v)->slots); break;
    case T_STREAM: mark_push(STM(v)->str); mark_push(STM(v)->name); break;
    case T_ENV: mark_push(ENV(v)->vars); mark_push(ENV(v)->funs); mark_push(ENV(v)->tags); mark_push(ENV(v)->parent); break;
    default: break;   /* strings, floats, bignums: leaves */
    }
}

static void mark_symbols(void);   /* symbol.c hands the table over */
void symbol_mark_all(void (*mark)(val));

static void mark_symbols(void) { symbol_mark_all(mark_push); }

void gc_collect(void) {
    long i;
    obj **pp;
    nlx_t *f;
    gc_runs++;
    mark_sp = 0;
    mark_symbols();
    mark_push(NIL); mark_push(T); mark_push(UNBOUND);
    mark_push(global_env);
    mark_push(cur_handler);
    mark_push(std_input); mark_push(std_output); mark_push(std_error);
    mark_push(cur_stdin); mark_push(cur_stdout); mark_push(cur_stderr);
    for (i = 0; i < gc_sp; i++) mark_push(gc_roots[i]);
    for (i = 0; i < dyn_sp; i++) { mark_push(dyn_syms[i]); mark_push(dyn_olds[i]); }
    for (f = nlx_top; f; f = f->prev) {
        mark_push(f->tag); mark_push(f->value); mark_push(f->handler);
        mark_push(f->pending_value); mark_push(f->cleanup); mark_push(f->env);
    }
    /* the C stack and the registers (spilled into the jmp_buf) */
    {
        jmp_buf regs;
        volatile uintptr_t here = 0;
        uintptr_t *p, *lo, *hi;
        setjmp(regs);
        lo = (uintptr_t *)&here;
        hi = (uintptr_t *)gc_stack_base;
        if (lo > hi) { uintptr_t *t = lo; lo = hi; hi = t; }
        for (p = lo; p < hi; p++) if (obj_set_has(*p)) mark_push((val)*p);
        for (p = (uintptr_t *)&regs; p < (uintptr_t *)(&regs + 1); p++) if (obj_set_has(*p)) mark_push((val)*p);
    }
    while (mark_sp > 0) {
        val v = mark_stack[--mark_sp];
        mark_children(v);
    }
    heap_live = 0;
    pp = &all_objects;
    while (*pp) {
        obj *o = *pp;
        if (o->mark) {
            o->mark = 0;
            heap_live++;
            pp = &o->gcnext;
        } else {
            *pp = o->gcnext;
            if (o->tag == T_STREAM) {
                stream_t *s = (stream_t *)o;
                if (s->open && s->fp && s->kind != S_STD_IN && s->kind != S_STD_OUT && s->kind != S_STD_ERR) fclose(s->fp);
            }
            free(o);
        }
    }
    heap_allocs = 0;
    if (gc_threshold < heap_live * 2) gc_threshold = heap_live * 2;
    obj_set_rebuild(heap_live);
}

obj *heap_alloc(int tag, size_t bytes) {
    obj *o;
    if (heap_allocs >= gc_threshold) gc_collect();
    o = malloc(bytes);
    if (!o) {
        gc_collect();
        o = malloc(bytes);
        if (!o) isl_error("<storage-exhausted>", NULL);
    }
    memset(o, 0, bytes);
    o->tag = (unsigned char)tag;
    o->gcnext = all_objects;
    all_objects = o;
    obj_set_insert(o);
    heap_allocs++;
    return o;
}

val mk_cons(val a, val d) {
    cons_t *c;
    long sp = gc_sp;
    gc_push(a); gc_push(d);
    c = (cons_t *)heap_alloc(T_CONS, sizeof(cons_t));
    c->car = a; c->cdr = d;
    gc_sp = sp;
    return (val)c;
}

val mk_string_uninit(long len) {
    string_t *s = (string_t *)heap_alloc(T_STRING, sizeof(string_t) + (size_t)len);
    s->len = len;
    s->data[len] = 0;
    return (val)s;
}

val mk_string(const char *str, long len) {
    val v = mk_string_uninit(len);
    memcpy(STR(v)->data, str, (size_t)len);
    return v;
}

val mk_float(double d) {
    flonum_t *f = (flonum_t *)heap_alloc(T_FLOAT, sizeof(flonum_t));
    if (d == 0.0) d = 0.0;      /* no negative zero: §13.3 allows it, the oracle does it */
    f->d = d;
    return (val)f;
}

val mk_vector(long n, val fill) {
    vector_t *vec;
    long i;
    long sp = gc_sp;
    gc_push(fill);
    vec = (vector_t *)heap_alloc(T_VECTOR, sizeof(vector_t) + (size_t)(n > 0 ? n - 1 : 0) * sizeof(val));
    vec->n = n;
    for (i = 0; i < n; i++) vec->elt[i] = fill;
    gc_sp = sp;
    return (val)vec;
}

val mk_env(val parent) {
    env_t *e;
    long sp = gc_sp;
    gc_push(parent);
    e = (env_t *)heap_alloc(T_ENV, sizeof(env_t));
    e->vars = NIL; e->funs = NIL; e->tags = NIL; e->parent = parent;
    gc_sp = sp;
    return (val)e;
}

val mk_closure(val params, val rest, val body, val env, val name) {
    closure_t *c;
    long sp = gc_sp;
    gc_push(params); gc_push(rest); gc_push(body); gc_push(env); gc_push(name);
    c = (closure_t *)heap_alloc(T_CLOSURE, sizeof(closure_t));
    c->params = params; c->rest = rest; c->body = body; c->env = env; c->name = name;
    c->nparams = (int)list_length(params);
    gc_sp = sp;
    return (val)c;
}

val mk_builtin(const char *name, builtin_fn fn, int minargs, int maxargs) {
    builtin_t *b = (builtin_t *)heap_alloc(T_BUILTIN, sizeof(builtin_t));
    b->name = name; b->fn = fn; b->minargs = minargs; b->maxargs = maxargs; b->sym = NIL;
    return (val)b;
}

val list1(val a) { return mk_cons(a, NIL); }
val list2(val a, val b) {
    val r;
    long sp = gc_sp;
    gc_push(a);
    r = mk_cons(b, NIL);
    r = mk_cons(a, r);
    gc_sp = sp;
    return r;
}
val list3(val a, val b, val c) {
    val r;
    long sp = gc_sp;
    gc_push(a); gc_push(b);
    r = mk_cons(c, NIL);
    r = mk_cons(b, r);
    r = mk_cons(a, r);
    gc_sp = sp;
    return r;
}

long list_length(val l) {
    long n = 0;
    while (IS_CONS(l)) { n++; l = CDR(l); }
    return l == NIL ? n : -1;
}

val list_nreverse(val l) {
    val prev = NIL;
    while (IS_CONS(l)) {
        val next = CDR(l);
        CDR(l) = prev;
        prev = l;
        l = next;
    }
    return prev;
}

val list_reverse(val l) {
    val r = NIL;
    long sp = gc_sp;
    gc_push(l);
    while (IS_CONS(l)) {
        r = mk_cons(CAR(l), r);
        gc_roots[sp] = l;   /* keep the remaining tail reachable */
        gc_push(r);
        gc_sp = sp + 1;
        l = CDR(l);
    }
    gc_sp = sp;
    return r;
}
