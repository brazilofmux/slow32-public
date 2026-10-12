/* builtin.c -- predicates (§13), lists (§21), symbols (§18), the basic
 * numbers (§19, the rest in milestone 3), apply and funcall (§12.7),
 * and the two extensions the harness needs: import/load of a source
 * file, and gc.
 */
#include "isl.h"
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <unistd.h>

val mk_bool(int b) { return b ? T : NIL; }

void defbuiltin(const char *name, builtin_fn fn, int minargs, int maxargs) {
    val sym = intern_c(name);
    val b = mk_builtin(name, fn, minargs, maxargs);
    BI(b)->sym = sym;
    SYM(sym)->fn = b;
}

val list_nth(val l, long n) {
    while (n-- > 0 && IS_CONS(l)) l = CDR(l);
    return IS_CONS(l) ? CAR(l) : NIL;
}

/* ---------- equality (§13.3) ---------- */

static int sign_bit(double d) {
    unsigned char b[8];
    memcpy(b, &d, 8);
    /* little-endian on every target this runs on (SLOW-32 and the hosts) */
    return (b[7] & 0x80) != 0;
}

int eql(val a, val b) {
    if (a == b) return 1;
    if (IS_FLT(a) && IS_FLT(b)) {
        double x = FLT(a)->d, y = FLT(b)->d;
        /* eql distinguishes 0.0 and -0.0 (§13.3) */
        return x == y && (x != 0.0 || sign_bit(x) == sign_bit(y));
    }
    if (IS_BIG(a) && IS_BIG(b)) return num_cmp(a, b) == 0;
    return 0;
}

int equal(val a, val b) {
    if (eql(a, b)) return 1;
    if (IS_CONS(a) && IS_CONS(b)) {
        while (IS_CONS(a) && IS_CONS(b)) {
            if (!equal(CAR(a), CAR(b))) return 0;
            a = CDR(a); b = CDR(b);
        }
        return equal(a, b);
    }
    if (IS_STR(a) && IS_STR(b)) return STR(a)->len == STR(b)->len && memcmp(STR(a)->data, STR(b)->data, (size_t)STR(a)->len) == 0;
    if (IS_VEC(a) && IS_VEC(b)) {
        long i;
        if (VEC(a)->n != VEC(b)->n) return 0;
        for (i = 0; i < VEC(a)->n; i++) if (!equal(VEC(a)->elt[i], VEC(b)->elt[i])) return 0;
        return 1;
    }
    if (IS_ARR(a) && IS_ARR(b)) return ARR(a)->rank == ARR(b)->rank && equal(ARR(a)->dims, ARR(b)->dims) && equal(ARR(a)->data, ARR(b)->data);
    return 0;
}

static val bi_eq(int argc, val *argv) { (void)argc; return mk_bool(argv[0] == argv[1] || (IS_FLT(argv[0]) && eql(argv[0], argv[1])) || (IS_BIG(argv[0]) && eql(argv[0], argv[1]))); }
static val bi_eql(int argc, val *argv) { (void)argc; return mk_bool(eql(argv[0], argv[1])); }
static val bi_equal(int argc, val *argv) { (void)argc; return mk_bool(equal(argv[0], argv[1])); }
static val bi_not(int argc, val *argv) { (void)argc; return mk_bool(argv[0] == NIL); }

/* ---------- class predicates (§13.2) ---------- */

static val bi_consp(int argc, val *argv) { (void)argc; return mk_bool(IS_CONS(argv[0])); }
static val bi_null(int argc, val *argv) { (void)argc; return mk_bool(argv[0] == NIL); }
static val bi_listp(int argc, val *argv) { (void)argc; return mk_bool(IS_LIST(argv[0])); }
static val bi_symbolp(int argc, val *argv) { (void)argc; return mk_bool(IS_SYM(argv[0])); }
static val bi_functionp(int argc, val *argv) { (void)argc; return mk_bool(IS_FUNC(argv[0])); }
static val bi_numberp(int argc, val *argv) { (void)argc; return mk_bool(IS_NUM(argv[0])); }
static val bi_integerp(int argc, val *argv) { (void)argc; return mk_bool(IS_INT(argv[0])); }
static val bi_floatp(int argc, val *argv) { (void)argc; return mk_bool(IS_FLT(argv[0])); }
static val bi_characterp(int argc, val *argv) { (void)argc; return mk_bool(IS_CHR(argv[0])); }
static val bi_stringp(int argc, val *argv) { (void)argc; return mk_bool(IS_STR(argv[0])); }
static val bi_general_vector_p(int argc, val *argv) { (void)argc; return mk_bool(IS_VEC(argv[0])); }
static val bi_basic_vector_p(int argc, val *argv) { (void)argc; return mk_bool(IS_VEC(argv[0]) || IS_STR(argv[0])); }
static val bi_basic_array_p(int argc, val *argv) { (void)argc; return mk_bool(IS_VEC(argv[0]) || IS_STR(argv[0]) || IS_ARR(argv[0])); }
static val bi_basic_array_star_p(int argc, val *argv) { (void)argc; return mk_bool(IS_ARR(argv[0])); }
static val bi_general_array_star_p(int argc, val *argv) { (void)argc; return mk_bool(IS_ARR(argv[0])); }

/* ---------- lists (§21) ---------- */

static void check_cons(val v) { if (!IS_CONS(v)) err_domain(v, "<cons>"); }
static void check_list(val v) { if (!IS_LIST(v)) err_domain(v, "<list>"); }
static void check_proper(val v) { if (list_length(v) < 0) { if (IS_CONS(v)) err_improper_list(v); err_domain(v, "<list>"); } }
static void check_function(val v) { if (!IS_FUNC(v)) err_domain(v, "<function>"); }
static void check_immutable(val v) { if (IS_PTR(v) && ((obj *)v)->imm) err_immutable(v); }

static val bi_cons(int argc, val *argv) { (void)argc; return mk_cons(argv[0], argv[1]); }
static val bi_car(int argc, val *argv) { (void)argc; check_cons(argv[0]); return CAR(argv[0]); }
static val bi_cdr(int argc, val *argv) { (void)argc; check_cons(argv[0]); return CDR(argv[0]); }
static val bi_set_car(int argc, val *argv) { (void)argc; check_cons(argv[1]); check_immutable(argv[1]); CAR(argv[1]) = argv[0]; return argv[0]; }
static val bi_set_cdr(int argc, val *argv) { (void)argc; check_cons(argv[1]); check_immutable(argv[1]); CDR(argv[1]) = argv[0]; return argv[0]; }

static val bi_create_list(int argc, val *argv) {
    long n, i;
    val fill = argc > 1 ? argv[1] : NIL, r = NIL;
    long sp = gc_sp;
    long slot;
    if (!IS_INT(argv[0]) || int_sign(argv[0]) < 0) err_domain(argv[0], "<integer>");
    if (!IS_FIX(argv[0]) || FIXVAL(argv[0]) > 100000000) isl_error("<STORAGE-EXHAUSTED>", NULL);
    n = FIXVAL(argv[0]);
    slot = gc_push(NIL);
    for (i = 0; i < n; i++) { r = mk_cons(fill, r); gc_roots[slot] = r; }
    gc_sp = sp;
    return r;
}

static val bi_list(int argc, val *argv) {
    val r = NIL;
    int i;
    long sp = gc_sp;
    long slot = gc_push(NIL);
    for (i = argc - 1; i >= 0; i--) { r = mk_cons(argv[i], r); gc_roots[slot] = r; }
    gc_sp = sp;
    return r;
}

static val bi_reverse(int argc, val *argv) { (void)argc; check_proper(argv[0]); return list_reverse(argv[0]); }
static val bi_nreverse(int argc, val *argv) { (void)argc; check_proper(argv[0]); check_immutable(argv[0]); return list_nreverse(argv[0]); }

static val bi_append(int argc, val *argv) {
    val head = NIL, tail = NIL;
    int i;
    long sp = gc_sp;
    long hslot = gc_push(NIL);
    for (i = 0; i < argc; i++) check_proper(argv[i]);
    for (i = 0; i < argc - 1; i++) {
        val l;
        for (l = argv[i]; IS_CONS(l); l = CDR(l)) {
            val cell = mk_cons(CAR(l), NIL);
            if (head == NIL) { head = cell; gc_roots[hslot] = head; } else CDR(tail) = cell;
            tail = cell;
        }
    }
    if (argc > 0) { if (head == NIL) head = argv[argc - 1]; else CDR(tail) = argv[argc - 1]; }
    gc_sp = sp;
    return head;
}

static val bi_member(int argc, val *argv) {
    val l;
    (void)argc;
    check_proper(argv[1]);
    for (l = argv[1]; IS_CONS(l); l = CDR(l)) if (eql(argv[0], CAR(l))) return l;
    return NIL;
}

static val bi_assoc(int argc, val *argv) {
    val l;
    (void)argc;
    check_proper(argv[1]);
    for (l = argv[1]; IS_CONS(l); l = CDR(l)) {
        if (!IS_CONS(CAR(l))) err_domain(CAR(l), "<cons>");
        if (eql(argv[0], CAR(CAR(l)))) return CAR(l);
    }
    return NIL;
}

/* The mapping family: fn, then one or more lists, stepping all in parallel. */
static long min_length(int nlists, val *lists) {
    long m = -1;
    int i;
    for (i = 0; i < nlists; i++) {
        long n;
        check_proper(lists[i]);
        n = list_length(lists[i]);
        if (m < 0 || n < m) m = n;
    }
    return m < 0 ? 0 : m;
}

/* mode: 0 mapcar, 1 mapc, 2 mapcan, 3 maplist, 4 mapl, 5 mapcon */
static val map_family(int mode, int argc, val *argv) {
    int nlists = argc - 1;
    val fn = argv[0];
    long m, i;
    val result = NIL, tail = NIL;
    long sp = gc_sp;
    long rslot, base;
    int j;
    check_function(fn);
    m = min_length(nlists, argv + 1);
    rslot = gc_push(NIL);
    base = gc_sp;
    for (j = 0; j < nlists; j++) gc_push(argv[1 + j]);      /* the moving cursors */
    for (i = 0; i < m; i++) {
        val r;
        long abase = gc_sp;
        for (j = 0; j < nlists; j++) {
            val cur = gc_roots[base + j];
            gc_push(mode >= 3 ? cur : CAR(cur));
        }
        r = apply(fn, nlists, &gc_roots[abase]);
        gc_sp = abase;
        for (j = 0; j < nlists; j++) gc_roots[base + j] = CDR(gc_roots[base + j]);
        if (mode == 0 || mode == 3) {
            val cell = mk_cons(r, NIL);
            if (result == NIL) { result = cell; gc_roots[rslot] = result; } else CDR(tail) = cell;
            tail = cell;
        } else if (mode == 2 || mode == 5) {
            /* nconc the results */
            if (r != NIL) {
                if (result == NIL) { result = r; gc_roots[rslot] = result; } else CDR(tail) = r;
                tail = r;
                while (IS_CONS(CDR(tail))) tail = CDR(tail);
            }
        }
    }
    gc_sp = sp;
    if (mode == 1 || mode == 4) return argv[1];
    return result;
}
static val bi_mapcar(int argc, val *argv) { return map_family(0, argc, argv); }
static val bi_mapc(int argc, val *argv) { return map_family(1, argc, argv); }
static val bi_mapcan(int argc, val *argv) { return map_family(2, argc, argv); }
static val bi_maplist(int argc, val *argv) { return map_family(3, argc, argv); }
static val bi_mapl(int argc, val *argv) { return map_family(4, argc, argv); }
static val bi_mapcon(int argc, val *argv) { return map_family(5, argc, argv); }

/* ---------- symbols (§18) ---------- */

static void check_symbol(val v) { if (!IS_SYM(v)) err_domain(v, "<symbol>"); }

static val bi_property(int argc, val *argv) {
    val p;
    check_symbol(argv[0]); check_symbol(argv[1]);
    for (p = SYM(argv[0])->plist; IS_CONS(p) && IS_CONS(CDR(p)); p = CDR(CDR(p)))
        if (CAR(p) == argv[1]) return CAR(CDR(p));
    return argc > 2 ? argv[2] : NIL;
}
static val bi_set_property(int argc, val *argv) {
    val p;
    (void)argc;
    check_symbol(argv[1]); check_symbol(argv[2]);
    for (p = SYM(argv[1])->plist; IS_CONS(p) && IS_CONS(CDR(p)); p = CDR(CDR(p)))
        if (CAR(p) == argv[2]) { CAR(CDR(p)) = argv[0]; return argv[0]; }
    {
        long sp = gc_sp;
        val cell = mk_cons(argv[0], SYM(argv[1])->plist);
        gc_push(cell);
        SYM(argv[1])->plist = mk_cons(argv[2], cell);
        gc_sp = sp;
    }
    return argv[0];
}
static val bi_remove_property(int argc, val *argv) {
    val *pp;
    (void)argc;
    check_symbol(argv[0]); check_symbol(argv[1]);
    pp = &SYM(argv[0])->plist;
    while (IS_CONS(*pp) && IS_CONS(CDR(*pp))) {
        if (CAR(*pp) == argv[1]) { val v = CAR(CDR(*pp)); *pp = CDR(CDR(*pp)); return v; }
        pp = &CDR(CDR(*pp));
    }
    return NIL;
}
static val bi_gensym(int argc, val *argv) { (void)argc; (void)argv; return gensym(); }

/* ---------- functions (§12.7) ---------- */

static val bi_apply(int argc, val *argv) {
    val fn = argv[0];
    val last = argv[argc - 1];
    long sp = gc_sp;
    long base = gc_sp;
    int i, n = 0;
    val r;
    check_function(fn);
    if (list_length(last) < 0) err_improper_list(last);
    for (i = 1; i < argc - 1; i++) { gc_push(argv[i]); n++; }
    for (; IS_CONS(last); last = CDR(last)) { gc_push(CAR(last)); n++; }
    r = apply(fn, n, &gc_roots[base]);
    gc_sp = sp;
    return r;
}
static val bi_funcall(int argc, val *argv) { check_function(argv[0]); return apply(argv[0], argc - 1, argv + 1); }
static val bi_identity(int argc, val *argv) { (void)argc; return argv[0]; }

/* ---------- sequences, the part lists need now (§25; the rest in milestone 4) ---------- */

static val bi_length(int argc, val *argv) {
    (void)argc;
    if (IS_LIST(argv[0])) {
        long n = list_length(argv[0]);
        if (n < 0) err_domain(argv[0], "<list>");
        return MKFIX(n);
    }
    if (IS_VEC(argv[0])) return MKFIX(VEC(argv[0])->n);
    if (IS_STR(argv[0])) return MKFIX(STR(argv[0])->len);
    err_domain(argv[0], "<basic-vector>");
    return NIL;
}

/* ---------- numbers, the arithmetic part (§19.1) ---------- */

static void check_num(val v) { if (!IS_NUM(v)) err_domain(v, "<number>"); }

static val bi_add(int argc, val *argv) {
    val acc = MKFIX(0);
    int i;
    long sp = gc_sp;
    long slot = gc_push(acc);
    for (i = 0; i < argc; i++) { check_num(argv[i]); acc = num_add(acc, argv[i]); gc_roots[slot] = acc; }
    gc_sp = sp;
    return acc;
}
static val bi_mul(int argc, val *argv) {
    val acc = MKFIX(1);
    int i;
    long sp = gc_sp;
    long slot = gc_push(acc);
    for (i = 0; i < argc; i++) { check_num(argv[i]); acc = num_mul(acc, argv[i]); gc_roots[slot] = acc; }
    gc_sp = sp;
    return acc;
}
static val bi_sub(int argc, val *argv) {
    val acc;
    int i;
    long sp = gc_sp;
    long slot;
    check_num(argv[0]);
    if (argc == 1) return num_sub(MKFIX(0), argv[0]);
    acc = argv[0];
    slot = gc_push(acc);
    for (i = 1; i < argc; i++) { check_num(argv[i]); acc = num_sub(acc, argv[i]); gc_roots[slot] = acc; }
    gc_sp = sp;
    return acc;
}
static val cmp_chain(int argc, val *argv, int lo, int hi) {
    int i;
    for (i = 0; i < argc; i++) check_num(argv[i]);
    for (i = 0; i + 1 < argc; i++) {
        int c = num_cmp(argv[i], argv[i + 1]);
        if (c < lo || c > hi) return NIL;
    }
    return T;
}
static val bi_num_eq(int argc, val *argv) { return cmp_chain(argc, argv, 0, 0); }
static val bi_num_ne(int argc, val *argv) { (void)argc; check_num(argv[0]); check_num(argv[1]); return mk_bool(num_cmp(argv[0], argv[1]) != 0); }
static val bi_lt(int argc, val *argv) { return cmp_chain(argc, argv, -1, -1); }
static val bi_gt(int argc, val *argv) { return cmp_chain(argc, argv, 1, 1); }
static val bi_le(int argc, val *argv) { return cmp_chain(argc, argv, -1, 0); }
static val bi_ge(int argc, val *argv) { return cmp_chain(argc, argv, 0, 1); }

/* ---------- extensions ---------- */

static val bi_import(int argc, val *argv) {
    char path[1024];
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    snprintf(path, sizeof path, "%s/%s.lsp", isl_libdir, STR(argv[0])->data);
    return load_file(path, 0);
}
static val bi_load(int argc, val *argv) {
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    return load_file(STR(argv[0])->data, 0);
}
static val bi_gc(int argc, val *argv) { (void)argc; (void)argv; gc_collect(); return NIL; }
/* eval is not ISLISP; the verification suite uses it, as the oracle provides it. */
static val bi_eval(int argc, val *argv) { (void)argc; return eval(argv[0], global_env); }
static val bi_exit(int argc, val *argv) {
    stream_flush(cur_stdout); stream_flush(cur_stderr);
    exit(argc > 0 && IS_FIX(argv[0]) ? (int)FIXVAL(argv[0]) : 0);
    return NIL;
}

/* ---------- §30 miscellaneous ---------- */

static val bi_get_universal_time(int argc, val *argv) {
    (void)argc; (void)argv;
    /* seconds since 1900-01-01 00:00:00 UT: Unix time plus 70 years of
     * seconds (2208988800, past a 32-bit long, so built as a number) */
    int ok;
    val epoch = parse_integer_text("2208988800", 10, 10, &ok);
    long sp = gc_sp;
    val r;
    gc_push(epoch);
    r = num_add(int_from_long((long)time(NULL)), epoch);
    gc_sp = sp;
    return r;
}
/* Internal time in microseconds from the real-time clock; the SLOW-32
 * runtime's clock() is a stub, and the emulator serves clock_gettime. */
static val microseconds_now(void) {
    struct timespec ts;
    val sec, us;
    long sp = gc_sp;
    ts.tv_sec = 0; ts.tv_nsec = 0;
    clock_gettime(CLOCK_REALTIME, &ts);
    sec = num_mul(int_from_long((long)ts.tv_sec), MKFIX(1000000));
    gc_push(sec);
    us = num_add(sec, MKFIX((long)(ts.tv_nsec / 1000)));
    gc_sp = sp;
    return us;
}
static val bi_get_internal_real_time(int argc, val *argv) { (void)argc; (void)argv; return microseconds_now(); }
static val bi_get_internal_run_time(int argc, val *argv) {
    (void)argc; (void)argv;
    if (clock() > 0) return num_mul(int_from_long((long)clock()), MKFIX(1000000 / CLOCKS_PER_SEC > 0 ? 1000000 / CLOCKS_PER_SEC : 1));
    return microseconds_now();
}
static val bi_internal_time_units_per_second(int argc, val *argv) { (void)argc; (void)argv; return MKFIX(1000000); }
static val bi_sleep(int argc, val *argv) {
    (void)argc;
    if (!IS_NUM(argv[0])) err_domain(argv[0], "<number>");
    sleep((unsigned)num_to_double(argv[0]));
    return NIL;
}
static val bi_system(int argc, val *argv) {
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    return int_from_long(system(STR(argv[0])->data));
}

void builtins_init(void) {
    defbuiltin("GET-UNIVERSAL-TIME", bi_get_universal_time, 0, 0);
    defbuiltin("GET-INTERNAL-REAL-TIME", bi_get_internal_real_time, 0, 0);
    defbuiltin("GET-INTERNAL-RUN-TIME", bi_get_internal_run_time, 0, 0);
    defbuiltin("INTERNAL-TIME-UNITS-PER-SECOND", bi_internal_time_units_per_second, 0, 0);
    defbuiltin("SLEEP", bi_sleep, 1, 1);
    defbuiltin("SYSTEM", bi_system, 1, 1);
    defbuiltin("EQ", bi_eq, 2, 2);
    defbuiltin("EQL", bi_eql, 2, 2);
    defbuiltin("EQUAL", bi_equal, 2, 2);
    defbuiltin("NOT", bi_not, 1, 1);
    defbuiltin("CONSP", bi_consp, 1, 1);
    defbuiltin("NULL", bi_null, 1, 1);
    defbuiltin("LISTP", bi_listp, 1, 1);
    defbuiltin("SYMBOLP", bi_symbolp, 1, 1);
    defbuiltin("FUNCTIONP", bi_functionp, 1, 1);
    defbuiltin("NUMBERP", bi_numberp, 1, 1);
    defbuiltin("INTEGERP", bi_integerp, 1, 1);
    defbuiltin("FLOATP", bi_floatp, 1, 1);
    defbuiltin("CHARACTERP", bi_characterp, 1, 1);
    defbuiltin("STRINGP", bi_stringp, 1, 1);
    defbuiltin("GENERAL-VECTOR-P", bi_general_vector_p, 1, 1);
    defbuiltin("BASIC-VECTOR-P", bi_basic_vector_p, 1, 1);
    defbuiltin("BASIC-ARRAY-P", bi_basic_array_p, 1, 1);
    defbuiltin("BASIC-ARRAY*-P", bi_basic_array_star_p, 1, 1);
    defbuiltin("GENERAL-ARRAY*-P", bi_general_array_star_p, 1, 1);
    defbuiltin("CONS", bi_cons, 2, 2);
    defbuiltin("CAR", bi_car, 1, 1);
    defbuiltin("CDR", bi_cdr, 1, 1);
    defbuiltin("SET-CAR", bi_set_car, 2, 2);
    defbuiltin("SET-CDR", bi_set_cdr, 2, 2);
    defbuiltin("CREATE-LIST", bi_create_list, 1, 2);
    defbuiltin("LIST", bi_list, 0, -1);
    defbuiltin("REVERSE", bi_reverse, 1, 1);
    defbuiltin("NREVERSE", bi_nreverse, 1, 1);
    defbuiltin("APPEND", bi_append, 0, -1);
    defbuiltin("MEMBER", bi_member, 2, 2);
    defbuiltin("ASSOC", bi_assoc, 2, 2);
    defbuiltin("MAPCAR", bi_mapcar, 2, -1);
    defbuiltin("MAPC", bi_mapc, 2, -1);
    defbuiltin("MAPCAN", bi_mapcan, 2, -1);
    defbuiltin("MAPLIST", bi_maplist, 2, -1);
    defbuiltin("MAPL", bi_mapl, 2, -1);
    defbuiltin("MAPCON", bi_mapcon, 2, -1);
    defbuiltin("PROPERTY", bi_property, 2, 3);
    defbuiltin("SET-PROPERTY", bi_set_property, 3, 3);
    defbuiltin("REMOVE-PROPERTY", bi_remove_property, 2, 2);
    defbuiltin("GENSYM", bi_gensym, 0, 0);
    defbuiltin("APPLY", bi_apply, 2, -1);
    defbuiltin("FUNCALL", bi_funcall, 1, -1);
    defbuiltin("IDENTITY", bi_identity, 1, 1);
    defbuiltin("LENGTH", bi_length, 1, 1);
    defbuiltin("+", bi_add, 0, -1);
    defbuiltin("*", bi_mul, 0, -1);
    defbuiltin("-", bi_sub, 1, -1);
    defbuiltin("=", bi_num_eq, 2, -1);
    defbuiltin("/=", bi_num_ne, 2, 2);
    defbuiltin("<", bi_lt, 2, -1);
    defbuiltin(">", bi_gt, 2, -1);
    defbuiltin("<=", bi_le, 2, -1);
    defbuiltin(">=", bi_ge, 2, -1);
    defbuiltin("IMPORT", bi_import, 1, 1);
    defbuiltin("LOAD", bi_load, 1, 1);
    defbuiltin("GC", bi_gc, 0, 0);
    defbuiltin("EVAL", bi_eval, 1, 1);
    defbuiltin("EXIT", bi_exit, 0, 1);
}

/* ---------- convert (§17) ---------- */

static val str_to_list(val s) {
    val r = NIL;
    long i;
    long sp = gc_sp;
    long slot = gc_push(NIL);
    for (i = STR(s)->len - 1; i >= 0; i--) { r = mk_cons(MKCHR((unsigned char)STR(s)->data[i]), r); gc_roots[slot] = r; }
    gc_sp = sp;
    return r;
}

val sf_convert(val o, val clsname, val env) {
    const char *c;
    (void)env;
    if (!IS_SYM(clsname) || SYM(clsname)->cls == NIL) err_undefined_class(clsname);
    c = STR(SYM(clsname)->name)->data;
    if (strcmp(c, "<OBJECT>") == 0) return o;
    if (IS_CHR(o)) {
        if (strcmp(c, "<CHARACTER>") == 0) return o;
        if (strcmp(c, "<INTEGER>") == 0) return MKFIX(CHRVAL(o));
        if (strcmp(c, "<SYMBOL>") == 0) { char b[2]; b[0] = (char)CHRVAL(o); b[1] = 0; return intern(b, 1); }
        /* character -> string is "-" in the table: (create-string 1 obj) instead */
    } else if (IS_INT(o)) {
        if (strcmp(c, "<INTEGER>") == 0) return o;
        if (strcmp(c, "<CHARACTER>") == 0) {
            if (!IS_FIX(o) || FIXVAL(o) < 0 || FIXVAL(o) > 255) err_domain(o, "<integer>");
            return MKCHR((int)FIXVAL(o));
        }
        if (strcmp(c, "<FLOAT>") == 0) return mk_float(num_to_double(o));
        if (strcmp(c, "<STRING>") == 0) return print_to_string(o, 0);
    } else if (IS_FLT(o)) {
        if (strcmp(c, "<FLOAT>") == 0) return o;
        if (strcmp(c, "<STRING>") == 0) return print_to_string(o, 0);
    } else if (IS_SYM(o) && (o != NIL || strcmp(c, "<SYMBOL>") == 0 || strcmp(c, "<STRING>") == 0)) {
        if (strcmp(c, "<SYMBOL>") == 0) return o;
        if (strcmp(c, "<STRING>") == 0) return mk_string(STR(SYM(o)->name)->data, STR(SYM(o)->name)->len);
    } else if (IS_STR(o)) {
        if (strcmp(c, "<STRING>") == 0) return o;
        if (strcmp(c, "<INTEGER>") == 0 || strcmp(c, "<FLOAT>") == 0) {
            val v;
            if (!parse_number_text(STR(o)->data, STR(o)->len, &v)) err_domain(o, c);
            if (strcmp(c, "<INTEGER>") == 0 && !IS_INT(v)) err_domain(o, c);
            if (strcmp(c, "<FLOAT>") == 0 && !IS_FLT(v)) err_domain(o, c);
            return v;
        }
        if (strcmp(c, "<SYMBOL>") == 0) return intern(STR(o)->data, STR(o)->len);
        if (strcmp(c, "<GENERAL-VECTOR>") == 0) {
            long i;
            val v = mk_vector(STR(o)->len, NIL);
            for (i = 0; i < STR(o)->len; i++) VEC(v)->elt[i] = MKCHR((unsigned char)STR(o)->data[i]);
            return v;
        }
        if (strcmp(c, "<LIST>") == 0) return str_to_list(o);
    } else if (IS_VEC(o)) {
        if (strcmp(c, "<GENERAL-VECTOR>") == 0) return o;
        if (strcmp(c, "<LIST>") == 0) {
            val r = NIL;
            long i;
            long sp = gc_sp;
            long slot = gc_push(NIL);
            for (i = VEC(o)->n - 1; i >= 0; i--) { r = mk_cons(VEC(o)->elt[i], r); gc_roots[slot] = r; }
            gc_sp = sp;
            return r;
        }
    } else if (IS_LIST(o)) {
        if (strcmp(c, "<LIST>") == 0) return o;
        if (strcmp(c, "<GENERAL-VECTOR>") == 0) {
            long n = list_length(o), i;
            val v;
            if (n < 0) err_domain(o, "<list>");
            v = mk_vector(n, NIL);
            for (i = 0; i < n; i++, o = CDR(o)) VEC(v)->elt[i] = CAR(o);
            return v;
        }
    }
    err_domain(o, c);
    return NIL;
}
